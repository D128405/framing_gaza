# evaluate_zeroshot.py

import os
import time
import platform
from datetime import datetime

import pandas as pd
import numpy as np
from transformers import pipeline
from sklearn.metrics import accuracy_score, precision_recall_fscore_support
import torch

# Paths
BASE_DIR = os.path.abspath("/Users/davidluu/Model Training & Performance Testing")
DATA_DIR = os.path.join(BASE_DIR, "data")
RESULTS_DIR = os.path.join(BASE_DIR, "results")
README_MD = os.path.join(BASE_DIR, "READme.md")

os.makedirs(RESULTS_DIR, exist_ok=True)

# Constants & Helpers
SEP = "|"
NLI_MODEL = "facebook/bart-large-mnli"

def split_labels(s: str):
    return [l.strip() for l in str(s).split(SEP) if l.strip()]

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
        import transformers as _tf
    except Exception:
        _tf = None
    finished_at = time.time()
    lines = [
        f"## [{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] {action}",
        f"- Duration: {hms(finished_at - started_at)}",
        f"- Host: {platform.node()} — Python {platform.python_version()} ({platform.platform()})",
        f"- transformers: {get_ver(_tf)}",
        f"- Notes: {notes}" if notes else "",
        "\n---\n",
    ]
    text = "\n".join([l for l in lines if l != ""])
    try:
        with open(README_MD, "a", encoding="utf-8") as f:
            f.write(text)
    except Exception:
        pass

def evaluate_level(df, column_name, candidate_labels, multi_label=False, subset_size=200, clf=None):
    print(f"\n--- Evaluating {column_name} ---")
    if len(candidate_labels) > 500 and multi_label:
        print(f"{len(candidate_labels)} fine-grained labels is too many for meaningful zero-shot evaluation. Skipping metrics.")
        subset_size = min(subset_size, 50)

    texts = df["Text"].astype(str).tolist()[:subset_size]
    true_labels = df[column_name].astype(str).tolist()[:subset_size]

    preds, scores = [], []
    for i, txt in enumerate(texts):
        res = clf(txt, candidate_labels, multi_label=multi_label)
        if multi_label:
            threshold = 0.3
            chosen = [lab for lab, sc in zip(res["labels"], res["scores"]) if sc > threshold]
            preds.append(SEP.join(chosen) if chosen else res["labels"][0])
            scores.append(float(np.max(res["scores"])))
        else:
            preds.append(res["labels"][0])
            scores.append(float(res["scores"][0]))
        if (i + 1) % 20 == 0:
            print(f"Processed {i+1}/{subset_size} texts...")

    label_to_idx = {lab: idx for idx, lab in enumerate(candidate_labels)}
    y_true_idx = np.array([label_to_idx.get(str(t).split(SEP)[0], -1) for t in true_labels])
    y_pred_idx = np.array([label_to_idx.get(str(p).split(SEP)[0], -1) for p in preds])

    out = df.iloc[:subset_size].copy()
    out[f"{column_name}_zs_pred"] = preds
    out[f"{column_name}_zs_score"] = scores
    out_path = os.path.join(RESULTS_DIR, f"zs_eval_subset_{column_name}.csv")
    out.to_csv(out_path, index=False)

    if len(candidate_labels) > 500 and multi_label:
        print(f"Saved {column_name} predictions to {out_path}")
        return None

    acc = accuracy_score(y_true_idx, y_pred_idx)
    p, r, f1, _ = precision_recall_fscore_support(y_true_idx, y_pred_idx, average="weighted", zero_division=0)

    print(f"Zero-shot metrics ({column_name}):")
    metrics = {
        "accuracy": round(acc, 4),
        "precision_weighted": round(p, 4),
        "recall_weighted": round(r, 4),
        "f1_weighted": round(f1, 4),
    }
    print(metrics)
    return metrics, out_path

if __name__ == "__main__":
    t0 = time.time()

    train_df = pd.read_csv(os.path.join(DATA_DIR, "train_data.csv"))
    eval_df = pd.read_csv(os.path.join(DATA_DIR, "eval_data.csv"))

    # Delimiter normalization
    for df in (train_df, eval_df):
        for col in ("LabelFrames", "TopLabelFrames"):
            if col in df.columns:
                df[col] = df[col].fillna("").astype(str).str.replace(";", SEP)

    # Device
    if torch.cuda.is_available():
        device = 0
    elif torch.backends.mps.is_available():
        device = 0
    else:
        device = -1
    print(f"Using device: {'GPU/MPS' if device == 0 else 'CPU'}")

    # Zero-shot classifier (NLI)
    classifier = pipeline("zero-shot-classification", model=NLI_MODEL, device=device)
    print("Loaded zero-shot pipeline:", NLI_MODEL)

    top_labels = sorted({lab for labs in train_df["TopLabelFrames"].fillna("") for lab in split_labels(labs)})
    fine_labels = sorted({lab for labs in train_df["LabelFrames"].fillna("") for lab in split_labels(labs)})

    notes = []
    if "TopLabelFrames" in eval_df.columns:
        m_top = evaluate_level(eval_df, "TopLabelFrames", top_labels, multi_label=False, subset_size=200, clf=classifier)
        if m_top:
            metrics, path = m_top
            notes.append(f"TopLabelFrames zshot: {metrics} (saved {path})")

    if "LabelFrames" in eval_df.columns:
        m_fine = evaluate_level(eval_df, "LabelFrames", fine_labels, multi_label=True, subset_size=200, clf=classifier)
        if m_fine:
            metrics, path = m_fine
            notes.append(f"LabelFrames zshot: {metrics} (saved {path})")

    log_to_readme(
        action="Zero-shot evaluation (facebook/bart-large-mnli)",
        started_at=t0,
        notes="; ".join(notes)
    )
    print("\nZero-shot evaluation complete.")
