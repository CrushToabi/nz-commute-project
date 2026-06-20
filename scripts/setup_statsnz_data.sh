#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: bash scripts/setup_statsnz_data.sh /path/to/statsnz-data" >&2
  exit 1
fi

source_dir=$1

if [ ! -d "$source_dir" ]; then
  echo "Source directory does not exist: $source_dir" >&2
  exit 1
fi

mkdir -p data

find_one() {
  local pattern=$1
  find "$source_dir" -type f -name "$pattern" | sort | head -n 1
}

copy_required() {
  local pattern=$1
  local target=$2
  local source_file

  source_file=$(find_one "$pattern")
  if [ -z "$source_file" ]; then
    echo "Could not find required file matching: $pattern" >&2
    exit 1
  fi

  cp "$source_file" "$target"
  echo "Copied $target"
}

copy_required "2023-census-main-means-of-travel-to-work-by-statistical-area.csv" \
  "data/2023-census-main-means-of-travel-to-work-by-statistical-area.csv"

copy_required "2023-census-electoral-population-at-meshblock-level-2025-meshblock-data.csv" \
  "data/2023-census-electoral-population-at-meshblock-level-2025-meshblock-data.csv"

meshblock_csv=$(find "$source_dir" -type f -name "*.csv" -print0 |
  xargs -0 grep -l "WKT,MB2025_V1_00" 2>/dev/null |
  sort |
  head -n 1 || true)

if [ -z "$meshblock_csv" ]; then
  echo "Could not find meshblock geometry CSV with WKT and MB2025_V1_00 columns." >&2
  exit 1
fi

cp "$meshblock_csv" "data/meshblock-2025.csv"
echo "Copied data/meshblock-2025.csv"

for ext in shp dbf shx prj cpg xml txt; do
  source_file=$(find_one "statistical-area-2-2023-generalised.$ext")
  if [ -n "$source_file" ]; then
    cp "$source_file" "data/statistical-area-2-2023-generalised.$ext"
    echo "Copied data/statistical-area-2-2023-generalised.$ext"
  fi
done

for ext in shp dbf shx prj; do
  if [ ! -f "data/statistical-area-2-2023-generalised.$ext" ]; then
    echo "Missing required SA2 shapefile component: statistical-area-2-2023-generalised.$ext" >&2
    exit 1
  fi
done

echo "Stats NZ data setup complete."
