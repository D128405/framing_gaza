import os
import pandas as pd


def prepare_data():
    """
    Prepare training and evaluation datasets from the gold-standard article corpus.

    The function:
    1. Loads the annotated dataset.
    2. Combines binary frame annotations into a single multi-label field.
    3. Removes records without textual content.
    4. Randomly shuffles the data using a fixed seed for reproducibility.
    5. Splits the dataset into 80% training and 20% evaluation subsets.
    6. Writes the resulting datasets to disk.
    """
    print("Loading gold standard dataset...")
    df = pd.read_csv("data/gaza_all_articles_gold.csv")

    # Frame labels defined in the annotation codebook.
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

    def combine_labels(row):
        """
        Combine all active binary frame labels for a record into a
        pipe-delimited multi-label string.
        """
        active_frames = [frame for frame in FRAMES if row.get(frame) == 1]
        return "|".join(active_frames)

    # Create a consolidated multi-label target variable.
    df["LabelFrames"] = df.apply(combine_labels, axis=1)

    # Exclude observations without valid text input.
    df = df.dropna(subset=["Text"])

    # Shuffle the dataset with a fixed random seed to ensure reproducibility.
    df = df.sample(frac=1, random_state=42).reset_index(drop=True)

    # Create an 80/20 train-evaluation split.
    train_size = int(0.8 * len(df))
    train_df = df.iloc[:train_size]
    eval_df = df.iloc[train_size:]

    # Create the output directory if it does not already exist.
    os.makedirs("data", exist_ok=True)

    # Save the processed datasets.
    train_df.to_csv("data/train_data.csv", index=False)
    eval_df.to_csv("data/eval_data.csv", index=False)

    print(
        f"Data split successful. Train: {len(train_df)} rows, "
        f"Eval: {len(eval_df)} rows."
    )


if __name__ == "__main__":
    prepare_data()
