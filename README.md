# Cantonese Alpaca

Generate Cantonese Instruction dataset by Gemini Pro using [Stanford's Alpaca](https://github.com/tatsu-lab/stanford_alpaca) prompts for fine-tuning LLMs.
this repo contain a script to generate the dataset and manually translate seed prompts to Cantonese from [Alpaca repo](https://github.com/tatsu-lab/stanford_alpaca/blob/main/seed_tasks.jsonl).

## Pre-requisites

```bash
pip install -r requirements.txt
```

## Usage

```bash
export GOOGLE_AISTUDIO_API_KEY=YOUR_API_KEY

python generate.py
```