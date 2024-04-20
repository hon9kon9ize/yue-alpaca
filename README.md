# 廣東話草泥馬 Cantonese Alpaca

![Cantonese Alpaca](https://github.com/hon9kon9ize/hon9kon9ize.github.io/blob/main/public/images/alpaca_with_tank.jpg?raw=true)

Generate Cantonese Instruction dataset by Gemini Pro using [Stanford's Alpaca](https://github.com/tatsu-lab/stanford_alpaca) prompts for fine-tuning LLMs.
this repo contain a script to generate the dataset and manually translate seed prompts to Cantonese from [Alpaca repo](https://github.com/tatsu-lab/stanford_alpaca/blob/main/seed_tasks.jsonl).

You can find the generated dataset on Huggingface [here](https://huggingface.co/datasets/hon9kon9ize/yue-alpaca).

## Pre-requisites

```bash
pip install -r requirements.txt
```

## Usage

```bash
export GOOGLE_AISTUDIO_API_KEY=YOUR_API_KEY

python generate.py
```

## Citation Information

```
@misc{alpaca,
  author = {Rohan Taori and Ishaan Gulrajani and Tianyi Zhang and Yann Dubois and Xuechen Li and Carlos Guestrin and Percy Liang and Tatsunori B. Hashimoto },
  title = {Stanford Alpaca: An Instruction-following LLaMA model},
  year = {2023},
  publisher = {GitHub},
  journal = {GitHub repository},
  howpublished = {\url{https://github.com/tatsu-lab/stanford_alpaca}},
}
```
