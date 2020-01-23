#!/bin/bash
source ~/.bash_profile
conda activate strava
snakemake
git add .
git commit -m "📊 Auto-update"
git push
