"""
finetuned_analysis.py
---------------------
Stage 3 of the pipeline: apply the best-performing model (DeBERTa-base, per the
methodology) to label the analysis corpus. The full corpus is partitioned upstream
into the cluster files that answer each research question:

  RQ1 (temporal):            gaza_rq1_t{1..6}.csv
  RQ2 (alignment x time):    gaza_rq2_{il,bb,ps}_t{1..6}.csv
  RQ3 (system x time):       gaza_rq3_{0,1,2,3}_t{1..6}.csv

Note on RQ3: only the four V-Dem regime types (0-3) appear here. Palestine and
Hong Kong are not classifiable in the RoW typology, so they are excluded upstream
from the RQ3 files (Methodology, Operationalizing Political System).

Each input file carries a 'Content' column holding the article text; this script
writes the eight predicted binary frame columns back and saves the labelled file
to results/inference/, which is the input directory for the R statistical analysis.
"""

import os
import json
import math
import torch
import numpy as np
import pandas as pd
from tqdm.auto import tqdm
from transformers import AutoTokenizer, AutoModelForSequenceClassification

BASE_DIR = os.path.abspath("./")
DATA_DIR = os.path.join(BASE_DIR, "data")
RESULTS_DIR = os.path.join(BASE_DIR, "results", "inference")
MODEL_DIR = os.path.join(BASE_DIR, "results", "deberta",
                         "trained_models", "deberta_labelframes")
LABEL_JSON = os.path.join(MODEL_DIR, "label2id_deberta_labelframes.json")

THRESHOLD = 0.5

# Column containing the article text used as model input.
TEXT_COLUMN = "Content"

# Read CSV files using a robust encoding fallback sequence to
# preserve text exported from heterogeneous operating systems.
def read_csv_robust(path):
    for enc in ("utf-8", "cp1252", "latin-1"):
        try:
            df = pd.read_csv(path, encoding=enc)
            if enc != "utf-8":
                print(f"  [encoding] {os.path.basename(path)} read as {enc} "
                      f"(not UTF-8)")
            return df
        except UnicodeDecodeError:
            continue
    # Final fallback: replace undecodable bytes to avoid aborting inference.
    print(f"  [encoding] {os.path.basename(path)} had undecodable bytes; "
          f"reading utf-8 with replacement")
    return pd.read_csv(path, encoding="utf-8", encoding_errors="replace")


MAX_LEN = 512
BATCH_SIZE = 8  # On MPS/CUDA you can raise this (e.g. 16-32) for more throughput.

# Input datasets grouped by research question and temporal phase.
RQ1_FILES = [f"gaza_rq1_t{i}.csv" for i in range(1, 7)]
RQ2_FILES = [f"gaza_rq2_{align}_t{i}.csv"
             for align in ["il", "bb", "ps"] for i in range(1, 7)]
RQ3_FILES = [f"gaza_rq3_{sys}_t{i}.csv"
             for sys in [0, 1, 2, 3] for i in range(1, 7)]
TARGET_FILES = RQ1_FILES + RQ2_FILES + RQ3_FILES


if __name__ == "__main__":
    os.makedirs(RESULTS_DIR, exist_ok=True)

    if not os.path.isdir(MODEL_DIR):
        raise FileNotFoundError(
            f"Trained DeBERTa model not found at {MODEL_DIR}. "
            f"Run train_deberta.py (and evaluate_models.py) first.")

    with open(LABEL_JSON, "r") as f:
        label2id = json.load(f)
    id2label = {int(v): k for k, v in label2id.items()}

    tokenizer = AutoTokenizer.from_pretrained(MODEL_DIR, use_fast=False)
    model = AutoModelForSequenceClassification.from_pretrained(MODEL_DIR)

    # Select the highest-performance compute device available.