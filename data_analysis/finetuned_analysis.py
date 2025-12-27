# finetuned_analysis.py
"""
Apply the fine-tuned models to:
- the exact cluster tokenized CSVs in /data,
- plus /data/train_data.csv and /data/eval_data.csv,
and save predictions into /results with the same filenames.

Models used:
  - LabelFrames  → Longformer (results/longformer/trained_models/longformer_labelframes)
  - TopLabelFrames → MPNet (results/mpnet/trained_models/mpnet_topframes)

Writes columns:
  - LabelFrames_pred (multi-label, '|' delimited; falls back to argmax if none ≥ threshold)
  - TopLabelFrames_pred (single best label)
Also appends a run log to READme.md.
"""

import os
import json
import time
import platform
from datetime import datetime

import torch
import pandas as pd
import numpy as np
from transformers import AutoTokenizer, AutoModelForSequenceClassification

# Paths
BASE_DIR = os.path.abspath("/Users/davidluu/Model Training & Performance Testing")
DATA_DIR = os.path.join(BASE_DIR, "data")
RESULTS_DIR = os.path.join(BASE_DIR, "results")
README_MD = os.path.join(BASE_DIR, "READme.md")

# Model Folders & Label Maps (Longformer for LabelFrames, MPNet for TopLabelFrames)
LF_LABEL_DIR = os.path.join(RESULTS_DIR, "longformer", "trained_models", "longformer_labelframes")
MP_TOP_DIR   = os.path.join(RESULTS_DIR, "mpnet", "trained_models", "mpnet_topframes")

LF_LABEL_JSON = os.path.join(LF_LABEL_DIR, "label2id_longformer_labelframes.json")
MP_TOP_JSON   = os.path.join(MP_TOP_DIR, "label2id_mpnet_topframes.json")

# Constants & Helpers
SEP = "|"

def split_labels(s: str):
    return [l.strip() for l in str(s).split(SEP) if l.strip()]

def join_labels(labels):
    return SEP.join(labels)

def get_ver(mod, attr="__version__"):
    try:
        return getattr(mod, attr)
    except Exception:
        return "n/a"

def hms(seconds: float):
    m, s = divmod(int(seconds), 60)
    h, m = divmod(m, 60)
    return f"{h:02d}:{m:02d}:{s:02d}"

def log_to_readme(action: str, started_at: float, notes: str = ""):
    try:
        import torch as _torch, transformers, pandas as _pd, numpy as _np
    except Exception:
        _torch = transformers = _pd = _np = None
    finished_at = time.time()
    lines = [
        f"## [{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] {action}",
        f"- Duration: {hms(finished_at - started_at)}",
        f"- Host: {platform.node()} — Python {platform.python_version()} ({platform.platform()})",
        "- Package versions:",
        f"  - torch: {get_ver(_torch)}",
        f"  - transformers: {get_ver(transformers)}",
        f"  - pandas: {get_ver(_pd)}",
        f"  - numpy: {get_ver(_np)}",
        f"- Notes: {notes}" if notes else "",
        "\n---\n",
    ]
    text = "\n".join([l for l in lines if l != ""])
    try:
        with open(README_MD, "a", encoding="utf-8") as f:
            f.write(text)
    except Exception:
        pass

def read_csv_robust(path: str) -> pd.DataFrame:
    """Try multiple encodings to avoid UnicodeDecodeError."""
    for enc in ("utf-8", "utf-8-sig", "latin-1", "cp1252"):
        try:
            return pd.read_csv(path, encoding=enc)
        except UnicodeDecodeError:
            continue
    # Replace errors
    return pd.read_csv(path, encoding="utf-8", errors="replace")

# Validate models
if not os.path.exists(LF_LABEL_DIR):
    raise FileNotFoundError(f"Longformer LabelFrames model folder not found: {LF_LABEL_DIR}")
if not os.path.exists(MP_TOP_DIR):
    raise FileNotFoundError(f"MPNet TopLabelFrames model folder not found: {MP_TOP_DIR}")

# Load Label Maps & Models
with open(LF_LABEL_JSON, "r") as f:
    label2id_label = json.load(f)
id2label_label = {int(v): k for k, v in label2id_label.items()}

with open(MP_TOP_JSON, "r") as f:
    label2id_top = json.load(f)
id2label_top = {int(v): k for k, v in label2id_top.items()}

tokenizer_label = AutoTokenizer.from_pretrained(LF_LABEL_DIR, use_fast=True)
model_label = AutoModelForSequenceClassification.from_pretrained(LF_LABEL_DIR)
tokenizer_top = AutoTokenizer.from_pretrained(MP_TOP_DIR, use_fast=False)
model_top = AutoModelForSequenceClassification.from_pretrained(MP_TOP_DIR)

device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")
model_label.to(device).eval()
model_top.to(device).eval()
print(f"Using device: {device} (Longformer for LabelFrames, MPNet for TopLabelFrames)")

# File list
CLUSTER_FILES = [
    # cluster 1 — political compliance
    "cluster1_politicalcompliance_il_t1_tokenized.csv",
    "cluster1_politicalcompliance_il_t2_tokenized.csv",
    "cluster1_politicalcompliance_il_t3_tokenized.csv",
    "cluster1_politicalcompliance_il_t4_tokenized.csv",
    "cluster1_politicalcompliance_il_t5_tokenized.csv",
    "cluster1_politicalcompliance_il_tokenized.csv",
    "cluster1_politicalcompliance_ps_t1_tokenized.csv",
    "cluster1_politicalcompliance_ps_t2_tokenized.csv",
    "cluster1_politicalcompliance_ps_t3_tokenized.csv",
    "cluster1_politicalcompliance_ps_t4_tokenized.csv",
    "cluster1_politicalcompliance_ps_t5_tokenized.csv",
    "cluster1_politicalcompliance_ps_tokenized.csv",
    "cluster1_politicalcompliance_nc_t1_tokenized.csv",
    "cluster1_politicalcompliance_nc_t2_tokenized.csv",
    "cluster1_politicalcompliance_nc_t3_tokenized.csv",
    "cluster1_politicalcompliance_nc_t4_tokenized.csv",
    "cluster1_politicalcompliance_nc_t5_tokenized.csv",
    "cluster1_politicalcompliance_nc_tokenized.csv",

    # cluster 2 — political system
    "cluster2_politicalsystem_democratic_t1_tokenized.csv",
    "cluster2_politicalsystem_democratic_t2_tokenized.csv",
    "cluster2_politicalsystem_democratic_t3_tokenized.csv",
    "cluster2_politicalsystem_democratic_t4_tokenized.csv",
    "cluster2_politicalsystem_democratic_t5_tokenized.csv",
    "cluster2_politicalsystem_democratic_tokenized.csv",
    "cluster2_politicalsystem_nondemocratic_t1_tokenized.csv",
    "cluster2_politicalsystem_nondemocratic_t2_tokenized.csv",
    "cluster2_politicalsystem_nondemocratic_t3_tokenized.csv",
    "cluster2_politicalsystem_nondemocratic_t4_tokenized.csv",
    "cluster2_politicalsystem_nondemocratic_t5_tokenized.csv",
    "cluster2_politicalsystem_nondemocratic_tokenized.csv",
]

CORE_FILES = ["train_data.csv", "eval_data.csv"]

def predict_labels(model, tokenizer, texts, threshold=0.5, batch_size=4, max_length=512):
    all_probs = []
    for i in range(0, len(texts), batch_size):
        batch_texts = texts[i:i+batch_size]
        enc = tokenizer(batch_texts, truncation=True, padding=True, max_length=max_length, return_tensors="pt")
        enc = {k: v.to(device) for k, v in enc.items()}
        with torch.no_grad():
            out = model(**enc)
        logits = out.logits.cpu().numpy()
        probs = 1 / (1 + np.exp(-logits))
        all_probs.append(probs)
    probs_all = np.vstack(all_probs) if len(all_probs) else np.empty((0, 0))
    binary_preds = (probs_all >= threshold).astype(int) if probs_all.size else np.empty((0, 0), dtype=int)
    return probs_all, binary_preds

if __name__ == "__main__":
    t0 = time.time()
    processed = []

    targets = [os.path.join(DATA_DIR, f) for f in (CLUSTER_FILES + CORE_FILES)]
    targets = [p for p in targets if os.path.exists(p)]

    os.makedirs(RESULTS_DIR, exist_ok=True)

    for file_path in targets:
        fname = os.path.basename(file_path)
        out_path = os.path.join(RESULTS_DIR, fname)
        print(f"\nProcessing {fname} ...")

        df = read_csv_robust(file_path)
        if "Text" not in df.columns:
            print(f"Skipping {fname}, no 'Text' column.")
            continue

        texts = df["Text"].astype(str).tolist()

        # LabelFrames (Longformer, multi-label). Use 1024 to match training.
        probs_label, preds_label = predict_labels(model_label, tokenizer_label, texts, max_length=1024)
        label_strings = []
        for p_row, probs_row in zip(preds_label, probs_label):
            idxs = np.where(p_row == 1)[0]
            if len(idxs) == 0:
                idxs = [int(np.argmax(probs_row))]
            labels = [id2label_label[i] for i in idxs]
            label_strings.append(join_labels(labels))
        df["LabelFrames_pred"] = label_strings

        # TopLabelFrames (MPNet, single-label argmax at 512)
        probs_top, _ = predict_labels(model_top, tokenizer_top, texts, max_length=512)
        top_preds = [id2label_top[int(np.argmax(row))] for row in probs_top]
        df["TopLabelFrames_pred"] = top_preds

        df.to_csv(out_path, index=False)
        print(f"Saved predictions → {out_path}")
        processed.append(fname)

    notes = f"Predictions saved for {len(processed)} files → /results. Files: {', '.join(processed[:5])}..."
    log_to_readme("Apply Longformer (LabelFrames) & MPNet (TopLabelFrames) to cluster/train/eval files", t0, notes=notes)
    print("\nAll requested files processed.")
