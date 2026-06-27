import os
import json

import numpy as np
import pandas as pd
import torch
from torch.utils.data import Dataset
from tqdm import tqdm
from transformers import (
    AutoModelForSequenceClassification,
    AutoTokenizer,
    Trainer,
    TrainingArguments,
)

# Base model identifier used for fine-tuning.
MODEL_NAME = "microsoft/deberta-base"

# Directory in which the trained model, tokenizer, and label mapping are saved.
OUTPUT_DIR = "results/deberta/trained_models/deberta_labelframes"

# Maximum sequence length (in tokens) used during tokenization.
MAX_LEN = 512

# Ordered list of frame labels defining the multi-label classification space.
FRAMES = [
    "Military Conflict Frame",
    "Human Interest Frame",
    "Violence of War Frame",
    "Anti-War Protest Frame",
    "Media Self-Reference Frame",
    "Responsibility Frame",
    "Diagnostic Frame",
    "Prognostic Frame",
]


class MultiLabelDataset(Dataset):
    """PyTorch dataset for multi-label text classification."""

    def __init__(self, texts, labels, tokenizer, max_len):
        self.texts = texts
        self.labels = labels
        self.tokenizer = tokenizer
        self.max_len = max_len

    def __len__(self):
        """Return the number of instances in the dataset."""
        return len(self.texts)

    def __getitem__(self, idx):
        """Tokenize a single text instance and return model-ready tensors."""
        text = str(self.texts[idx])
        labels = self.labels[idx]

        encoding = self.tokenizer(
            text,
            add_special_tokens=True,
            max_length=self.max_len,
            padding="max_length",
            truncation=True,
            return_attention_mask=True,
            return_tensors="pt",
        )

        return {
            "input_ids": encoding["input_ids"].flatten(),
            "attention_mask": encoding["attention_mask"].flatten(),
            "labels": torch.FloatTensor(labels),
        }


def load_data(filepath):
    """
    Load a CSV file and convert pipe-delimited frame labels into
    binary multi-label vectors corresponding to FRAMES.
    """
    df = pd.read_csv(filepath)
    df = df.dropna(subset=["Text"])

    labels_matrix = []
    for _, row in tqdm(df.iterrows(), total=df.shape[0], desc="Processing Data"):
        active_labels = str(row.get("LabelFrames", "")).split("|")
        binary_vector = [1.0 if frame in active_labels else 0.0 for frame in FRAMES]
        labels_matrix.append(binary_vector)

    return df["Text"].tolist(), np.array(labels_matrix)


if __name__ == "__main__":
    # Create the output directory if it does not already exist.
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Create label-index mappings for model configuration and reproducibility.
    label2id = {label: i for i, label in enumerate(FRAMES)}
    id2label = {i: label for i, label in enumerate(FRAMES)}

    with open(os.path.join(OUTPUT_DIR, "label2id_deberta_labelframes.json"), "w") as f:
        json.dump(label2id, f)

    # Load the tokenizer and initialize the DeBERTa model for multi-label classification.
    tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)
    model = AutoModelForSequenceClassification.from_pretrained(
        MODEL_NAME,
        num_labels=len(FRAMES),
        problem_type="multi_label_classification",
        label2id=label2id,
        id2label=id2label,
    )

    print("Loading datasets...")
    train_texts, train_labels = load_data("data/train_data.csv")
    eval_texts, eval_labels = load_data("data/eval_data.csv")

    train_dataset = MultiLabelDataset(train_texts, train_labels, tokenizer, MAX_LEN)
    eval_dataset = MultiLabelDataset(eval_texts, eval_labels, tokenizer, MAX_LEN)

    # Training configuration.
    training_args = TrainingArguments(
        output_dir=OUTPUT_DIR,
        eval_strategy="epoch",
        save_strategy="epoch",
        learning_rate=2e-5,
        per_device_train_batch_size=2,
        gradient_accumulation_steps=4,
        per_device_eval_batch_size=2,
        num_train_epochs=3,
        weight_decay=0.01,
        load_best_model_at_end=True,
        metric_for_best_model="eval_loss",
        logging_steps=1,
        dataloader_pin_memory=False if torch.backends.mps.is_available() else True,
    )

    trainer = Trainer(
        model=model,
        args=training_args,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
    )

    print("Starting DeBERTa training...")
    trainer.train()
    trainer.save_model(OUTPUT_DIR)
    tokenizer.save_pretrained(OUTPUT_DIR)
    print("DeBERTa training complete and saved.")
