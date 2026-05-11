#!/usr/bin/env bash
set -euo pipefail

CONFIG_PATH="${1:-configs/cv_pipeline_config.json}"

if [[ ! -f "$CONFIG_PATH" ]]; then
  echo "Config file not found: $CONFIG_PATH"
  echo "Usage: bash run_cv_pipeline.sh path/to/config.json"
  exit 1
fi

export CV_PIPELINE_CONFIG="$CONFIG_PATH"

Rscript scripts/run_pipeline.R
