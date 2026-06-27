# Political Environments and Event Dynamics: A Multi-National Computational Study of Gaza War Framing

This repository contains the code accompanying the article:

> **Political Environments and Event Dynamics: A Multi-National Computational Study of Gaza War Framing**

**Article:** *Link will be added upon publication.*

---

## Overview

This repository provides the complete computational workflow used in the study, including:

* Preparation of the manually annotated training corpus
* Fine-tuning of transformer-based multi-label classifiers
* Model evaluation and selection
* Automated annotation of the full news corpus
* Statistical analyses
* Figure generation

The repository is intended to facilitate computational reproducibility of the machine learning and statistical analyses reported in the manuscript.

---

## Abstract

How do media narratives of war evolve in response to events on the battlefield or to the political environments in which journalism is produced? While research on conflict framing has largely focused on individual countries or short periods of coverage, less is known about the relative importance of temporal dynamics and political context in shaping international news framing. This study addresses that question through a longitudinal, cross-national analysis of **45,714 English-language news articles** published by **21 mainstream news outlets** across **14 countries** between **7 October 2023** and **11 August 2025**.

Using supervised transformer-based natural language processing, we identify eight conflict-specific news frames and examine their variation across six phases of the Gaza war, countries' political alignment toward Israel and Palestine, and political regime type. We find that conflict framing remained remarkably stable over time despite major military, legal, and diplomatic developments. By contrast, political alignment was consistently associated with substantial differences in framing: pro-Israel, pro-Palestine, and politically balanced outlets emphasized distinct dimensions of the conflict, whereas differences across political systems were more nuanced than a simple democracy–autocracy divide. These findings suggest that international conflict reporting is shaped less by the changing trajectory of war than by the political contexts within which news organizations operate. By integrating computational text analysis with theories of media-politics relations, this study extends research on conflict framing, indexing, and international political communication, demonstrating that political alignment is a stronger correlate of cross-national framing than temporal developments in protracted armed conflict.

**Keywords:** Framing; Media Bias; Gaza; War and Conflict; International Relations; Political Communication; Text as Data; Computational Social Science

---

# Repository Structure

```
.
├── data/
│   ├── gaza_all_articles_gold.csv
│   ├── train_data.csv
│   ├── eval_data.csv
│   ├── gaza_rq1_*.csv
│   ├── gaza_rq2_*.csv
│   └── gaza_rq3_*.csv
│
├── results/
│   ├── deberta/
│   ├── roberta/
│   ├── evaluation/
│   ├── inference/
│   ├── statistics/
│   └── visualizations/
│
├── data_preparation.py
├── train_roberta.py
├── train_deberta.py
├── evaluate_models.py
├── finetuned_analysis.py
├── statistical_analysis.R
└── README.md
```

---

# Computational Workflow

The computational pipeline consists of five sequential stages.

## Stage 1 — Data Preparation

**Script**

```
data_preparation.py
```

This script:

* Loads the manually annotated gold-standard dataset.
* Combines binary frame annotations into a single multi-label variable.
* Removes observations without article text.
* Randomly shuffles the corpus using a fixed random seed (`42`).
* Splits the corpus into **80% training** and **20% evaluation** subsets.
* Exports:

```
data/train_data.csv
data/eval_data.csv
```

---

## Stage 2 — Transformer Fine-Tuning

Two transformer architectures are independently fine-tuned for multi-label frame classification.

### RoBERTa

```
train_roberta.py
```

Base model:

```
roberta-base
```

### DeBERTa

```
train_deberta.py
```

Base model:

```
microsoft/deberta-base
```

### Model Configuration

| Parameter               |                      Value |
| ----------------------- | -------------------------: |
| Problem type            | Multi-label classification |
| Maximum sequence length |                 512 tokens |
| Learning rate           |                   2 × 10⁻⁵ |
| Batch size              |                          2 |
| Gradient accumulation   |                          4 |
| Training epochs         |                          3 |
| Weight decay            |                       0.01 |

The models classify the following eight conflict frames:

* Military Conflict Frame
* Human Interest Frame
* Violence of War Frame
* Anti-War Protest Frame
* Media Self-Reference Frame
* Responsibility Frame
* Diagnostic Frame
* Prognostic Frame

Trained models, tokenizers, and label mappings are saved to:

```
results/deberta/
results/roberta/
```

---

## Stage 3 — Model Evaluation

**Script**

```
evaluate_models.py
```

The evaluation script compares the fine-tuned transformer models using the held-out evaluation dataset.

Performance metrics include:

* Precision
* Recall
* F1-score
* Support

reported for each frame, together with:

* Macro-average performance
* Micro-average performance

Evaluation tables are written to:

```
results/evaluation/
```

The best-performing model (DeBERTa-base in the published study) is subsequently selected for inference on the complete corpus.

---

## Stage 4 — Automated Corpus Annotation

**Script**

```
finetuned_analysis.py
```

The selected DeBERTa model is applied to the full corpus.

Input datasets are organized according to the study's research questions.

### Research Question 1

Temporal evolution of framing

```
gaza_rq1_t1.csv
...
gaza_rq1_t6.csv
```

### Research Question 2

Political alignment × time

```
gaza_rq2_il_*.csv
gaza_rq2_bb_*.csv
gaza_rq2_ps_*.csv
```

where

* `il` = Pro-Israel
* `bb` = Politically balanced
* `ps` = Pro-Palestine

### Research Question 3

Political system × time

```
gaza_rq3_0_*.csv
...
gaza_rq3_3_*.csv
```

using the four V-Dem *Regimes of the World* classifications.

For each article, predicted binary frame variables are appended and exported to:

```
results/inference/
```

These files serve as the input for the statistical analyses.

---

## Stage 5 — Statistical Analysis

**Script**

```
statistical_analysis.R
```

This script reproduces all statistical analyses and figures reported in the manuscript.

Analyses include:

* Descriptive statistics
* Pearson χ² tests
* Cramer's V
* Logistic regression models
* Likelihood-ratio tests
* Marginal effects estimation
* Benjamini–Hochberg false discovery rate correction
* McFadden's pseudo-*R²*
* Variance inflation diagnostics
* Firth logistic regression sensitivity analyses
* Mixed-effects robustness analyses
* Publication-quality visualizations

Outputs are written to:

```
results/statistics/
results/visualizations/
```

---

# Reproducing the Analysis

Execute the scripts in the following order:

```bash
python data_preparation.py

python train_roberta.py

python train_deberta.py

python evaluate_models.py

python finetuned_analysis.py

Rscript statistical_analysis.R
```

---

# Software Requirements

## Python

Recommended version:

```
Python 3.11+
```

Required packages:

```
transformers
torch
numpy
pandas
scikit-learn
tqdm
```

Install with:

```bash
pip install transformers torch pandas numpy scikit-learn tqdm
```

---

## R

Recommended version:

```
R 4.3+
```

Required packages:

```
tidyverse
broom
broom.mixed
vcd
viridis
lme4
marginaleffects
car
MuMIn
patchwork
```

Optional package:

```
logistf
```

which is used only for Firth logistic regression sensitivity analyses.

---

# Reproducibility

Several design choices were implemented to facilitate computational reproducibility:

* Fixed random seed (`42`) for dataset partitioning.
* Explicit hyperparameter specification.
* Deterministic label mappings.
* Consistent preprocessing across all models.
* Saved tokenizer and model checkpoints.
* Fully scripted statistical analyses.
* Automatic generation of all tables and figures.

---

# Data Availability

The manually annotated gold-standard dataset and the complete corpus of news articles are **not included** in this repository because they are subject to third-party licensing and copyright restrictions.

Researchers wishing to reproduce the analyses should reconstruct the corpus using the data collection procedures described in the manuscript and format the datasets according to the expected directory structure.

---

# Citation

If you use this repository, please cite:

*To be added.*
