# train_longformer.py

import os
import json
import time
import platform
from datetime import datetime

import torch
import pandas as pd
import numpy as np
from datasets import Dataset, Features, Value
from transformers import AutoTokenizer, AutoModelForSequenceClassification, Trainer, TrainingArguments
from sklearn.metrics import precision_recall_fscore_support, accuracy_score

# Paths
BASE_DIR = os.path.abspath("/Users/davidluu/Model Training & Performance Testing")
DATA_DIR = os.path.join(BASE_DIR, "data")
RESULTS_DIR = os.path.join(BASE_DIR, "results", "longformer")
README_MD = os.path.join(BASE_DIR, "READme.md")

os.makedirs(os.path.join(RESULTS_DIR, "trained_models"), exist_ok=True)
os.makedirs(os.path.join(RESULTS_DIR, "logs"), exist_ok=True)

# Constants & Helpers
SEP = "|"

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
        import transformers, datasets, accelerate, sklearn, numpy, pandas, torch as _torch
    except Exception:
        transformers = datasets = accelerate = sklearn = numpy = pandas = _torch = None
    finished_at = time.time()
    lines = [
        f"## [{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] {action}",
        f"- Duration: {hms(finished_at - started_at)}",
        f"- Host: {platform.node()} — Python {platform.python_version()} ({platform.platform()})",
        "- Package versions:",
        f"  - torch: {get_ver(_torch)}",
        f"  - transformers: {get_ver(transformers)}",
        f"  - datasets: {get_ver(datasets)}",
        f"  - accelerate: {get_ver(accelerate)}",
        f"  - scikit-learn: {get_ver(sklearn)}",
        f"  - pandas: {get_ver(pandas)}",
        f"  - numpy: {get_ver(numpy)}",
        f"- Notes: {notes}" if notes else "",
        "\n---\n",
    ]
    text = "\n".join([l for l in lines if l != ""])
    try:
        with open(README_MD, "a", encoding="utf-8") as f:
            f.write(text)
    except Exception:
        pass

# Load data
train_df = pd.read_csv(os.path.join(DATA_DIR, "train_data.csv"))
eval_df = pd.read_csv(os.path.join(DATA_DIR, "eval_data.csv"))
for df in (train_df, eval_df):
    df["LabelFrames"] = df["LabelFrames"].fillna("").astype(str).str.replace(";", SEP)
    df["TopLabelFrames"] = df["TopLabelFrames"].fillna("").astype(str).str.replace(";", SEP)
train_df = train_df[["Text", "LabelFrames", "TopLabelFrames"]]
eval_df = eval_df[["Text", "LabelFrames", "TopLabelFrames"]]

# Tokenizer
model_name = "allenai/longformer-base-4096"
tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)

def tokenize(batch):
    return tokenizer(batch["Text"], truncation=True, padding="max_length", max_length=1024)

def short_name(column):
    return "labelframes" if column == "LabelFrames" else "topframes"

def train_for(column_name):
    t0 = time.time()
    print(f"\n==============================\nTraining Longformer on {column_name}\n==============================")

    all_labels = sorted({
        label
        for sub in pd.concat([train_df[column_name], eval_df[column_name]]).fillna("")
        for label in split_labels(sub)
    })
    label2id = {label: i for i, label in enumerate(all_labels)}
    id2label = {i: label for label, i in label2id.items()}
    num_labels = len(all_labels)

    save_path = os.path.join(RESULTS_DIR, "trained_models", f"longformer_{short_name(column_name)}")
    os.makedirs(save_path, exist_ok=True)

    features = Features({"Text": Value("string"), "LabelFrames": Value("string"), "TopLabelFrames": Value("string")})

    def set_labels(example):
        multi = np.zeros(num_labels, dtype=np.float32)
        for l in split_labels(example[column_name]):
            if l in label2id:
                multi[label2id[l]] = 1
        example["labels"] = multi.tolist()
        return example

    train_dataset = Dataset.from_pandas(train_df, features=features, preserve_index=False)
    eval_dataset = Dataset.from_pandas(eval_df, features=features, preserve_index=False)

    train_dataset = train_dataset.map(tokenize, batched=True, remove_columns=["Text"], load_from_cache_file=False)
    eval_dataset = eval_dataset.map(tokenize, batched=True, remove_columns=["Text"], load_from_cache_file=False)
    train_dataset = train_dataset.map(set_labels, batched=False)
    eval_dataset = eval_dataset.map(set_labels, batched=False)

    model = AutoModelForSequenceClassification.from_pretrained(
        model_name,
        num_labels=num_labels,
        problem_type="multi_label_classification",
        id2label=id2label,
        label2id=label2id,
    )

    def compute_metrics(pred):
        labels = pred.label_ids.astype(int)
        preds = (pred.predictions > 0).astype(int)
        precision, recall, f1, _ = precision_recall_fscore_support(labels, preds, average="micro", zero_division=0)
        precision_w, recall_w, f1_w, _ = precision_recall_fscore_support(labels, preds, average="weighted", zero_division=0)
        acc = accuracy_score(labels.flatten(), preds.flatten())
        return {
            "precision_micro": precision, "recall_micro": recall, "f1_micro": f1,
            "precision_weighted": precision_w, "recall_weighted": recall_w, "f1_weighted": f1_w,
            "flat_accuracy": acc,
        }

    lr = 2e-5 if column_name == "LabelFrames" else 1e-5
    training_args = TrainingArguments(
        output_dir=save_path,
        learning_rate=lr,
        save_strategy="no",
        per_device_train_batch_size=1,
        per_device_eval_batch_size=1,
        gradient_accumulation_steps=2,
        num_train_epochs=3,
        weight_decay=0.01,
        logging_dir=os.path.join(RESULTS_DIR, "logs", f"longformer_{short_name(column_name)}"),
        report_to="none",
        push_to_hub=False,
        dataloader_pin_memory=False if torch.backends.mps.is_available() else True,
        load_best_model_at_end=False,
        max_grad_norm=1.0,
    )

    device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")
    print(f"Using device: {device}")

    trainer = Trainer(
        model=model.to(device),
        args=training_args,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        compute_metrics=compute_metrics,
    )
    trainer.train()

    model.to("cpu").save_pretrained(save_path)
    tokenizer.save_pretrained(save_path)
    with open(os.path.join(save_path, f"label2id_longformer_{short_name(column_name)}.json"), "w") as f:
        json.dump(label2id, f, indent=2)
    with open(os.path.join(save_path, f"id2label_longformer_{short_name(column_name)}.json"), "w") as f:
        json.dump(id2label, f, indent=2)

    print(f"Model and label maps saved to {save_path}")
    log_to_readme(
        action=f"Train Longformer on {column_name}",
        started_at=t0,
        notes=f"Saved → {save_path} | labels={num_labels}"
    )

if __name__ == "__main__":
    train_for("LabelFrames")
    train_for("TopLabelFrames")
