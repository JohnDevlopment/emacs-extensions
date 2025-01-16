#!/bin/bash

echo "Making virtual environment..."
python -m venv .venv || exit 1

# Install libraries in requirements.txt
source .venv/bin/activate
pip install -r requirements.txt

echo "source .venv/bin/activate" | xclip -selection clipboard
