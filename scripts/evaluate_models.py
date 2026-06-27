"""
Model evaluation script for multi-label frame classification.

This script evaluates one or more fine-tuned transformer models on a held-out
evaluation dataset. For each available model, it computes per-frame precision,
recall, F1-score, and support, together with macro- and micro-averaged metrics.
The resulting evaluation tables are written to CSV files to facilitate
reproducibility and downstream statistical reporting.
"""

import os
import torch
import pandas as pd
import numpy as np
from transformers import AutoTokenizer, AutoModelForSequenceClassification
from sklearn.metrics import precision_recall_fscore_support

FRAMES = [
    'Military Conflict Frame', 'Human Interest Frame',
    'Violence of War Frame', 'Anti-War Protest Frame',
    'Media Self-Reference Frame', 'Responsibility Frame',
    'Diagnostic Frame', 'Prognostic Frame'
]

EVAL_PATH = "data/eval_data.csv"
RESULTS_DIR = "results/evaluation"
THRESHOLD = 0.5
MAX_LEN = 512

# Model display names and corresponding directories containing trained checkpoints.
MODELS = [
    ("RoBERTa-base", "results/roberta/trained_models/roberta_labelframes"),
    ("DeBERTa-base", "results/deberta/trained_models/deberta_labelframes"),
]


def get_predictions(model_dir, texts, max_len=MAX_LEN, batch_size=16):
    tokenizer = AutoTokenizer.from_pretrained(model_dir)
    model = AutoModelForSequenceClassification.from_pretrained(model_dir)
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model.to(device).eval()

    all_probs = []
    for i in range(0, len(texts), batch_size):
        batch = texts[i:i + batch_size]
        enc = tokenizer(batch, truncation=True, padding=True,
                        max_length=max_len, return_tensors="pt")
        enc = {k: v.to(device) for k, v in enc.items()}
        with torch.no_grad():
            out = model(**enc)
        probs = torch.sigmoid(out.logits).cpu().numpy()
        all_probs.append(probs)
    return np.vstack(all_probs)


def evaluate_model(name, model_dir, df):
    print("\n" + "#" * 60)
    print(f"# Evaluating {name}  ({model_dir})")
    print("#" * 60)

    texts = df['Text'].astype(str).tolist()

    # Construct the binary ground-truth label matrix from the pipe-delimited frame annotations.
    y_true = np.array([
        [1 if frame in str(row.get("LabelFrames", "")).split("|") else 0
         for frame in FRAMES]
        for _, row in df.iterrows()
    ])

    probs = get_predictions(model_dir, texts)
    y_pred = (probs >= THRESHOLD).astype(int)

    # Compute precision, recall, F1-score, and support independently for each frame.
    p, r, f1, support = precision_recall_fscore_support(
        y_true, y_pred, average=None, zero_division=0,
        labels=list(range(len(FRAMES))))
    per_frame = pd.DataFrame({
        "Model": name, "Frame": FRAMES,
        "Precision": p, "Recall": r, "F1": f1, "Support": support,
    })

    # Compute macro- and micro-averaged performance across all frames.
    macro_p, macro_r, macro_f1, _ = precision_recall_fscore_support(
        y_true, y_pred, average="macro", zero_division=0)
    micro_p, micro_r, micro_f1, _ = precision_recall_fscore_support(
        y_true, y_pred, average="micro", zero_division=0)
    overall = pd.DataFrame([
        {"Model": name, "Average": "macro", "Precision": macro_p,
         "Recall": macro_r, "F1": macro_f1},
        {"Model": name, "Average": "micro", "Precision": micro_p,
         "Recall": micro_r, "F1": micro_f1},
    ])

    # Display evaluation results in a human-readable format.
    print("\nPER-FRAME PERFORMANCE (Precision, Recall, F1, Support)")
    print(per_frame.to_string(index=False,
          float_format=lambda x: f"{x:.4f}"))
    print("\nOVERALL")
    print(f"  Macro -> P: {macro_p:.4f} | R: {macro_r:.4f} | F1: {macro_f1:.4f}")
    print(f"  Micro -> P: {micro_p:.4f} | R: {micro_r:.4f} | F1: {micro_f1:.4f}")

    return per_frame, overall


if __name__ == "__main__":
    os.makedirs(RESULTS_DIR, exist_ok=True)
    df = pd.read_csv(EVAL_PATH).dropna(subset=['Text']).reset_index(drop=True)
    print(f"Evaluation set: {len(df)} articles")

    all_per_frame, all_overall = [], []
    for name, model_dir in MODELS:
        if not os.path.isdir(model_dir):
            print(f"\n[skip] {name}: model directory not found ({model_dir}). "
                  f"Train it first.")
            continue
        per_frame, overall = evaluate_model(name, model_dir, df)
        per_frame.to_csv(
            os.path.join(RESULTS_DIR, f"per_frame_{name.replace('-', '_').lower()}.csv"),
            index=False)
        all_per_frame.append(per_frame)
        all_overall.append(overall)

    if all_overall:
        overall_cmp = pd.concat(all_overall, ignore_index=True)
        per_frame_cmp = pd.concat(all_per_frame, ignore_index=True)
        overall_cmp.to_csv(os.path.join(RESULTS_DIR, "overall_comparison.csv"), index=False)
        per_frame_cmp.to_csv(os.path.join(RESULTS_DIR, "per_frame_comparison.csv"), index=False)

        print("\n" + "=" * 60)
        print("MODEL COMPARISON (overall)")
        print("=" * 60)
        print(overall_cmp.to_string(index=False,
              float_format=lambda x: f"{x:.4f}"))

        # Select the highest-performing model according to micro-averaged F1-score.
        # This model can subsequently be used for inference on the full corpus.
        micro = overall_cmp[overall_cmp["Average"] == "micro"]
        if not micro.empty:
            best = micro.loc[micro["F1"].idxmax(), "Model"]
            print(f"\nBest model on micro-F1: {best}")
            print("=> Use this model for full-corpus inference (finetuned_analysis.py).")
    else:
        print("\nNo trained models found to evaluate.")