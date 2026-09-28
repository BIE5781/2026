#!/usr/bin/env bash

set -euo pipefail

site_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
force_render=false

if [[ "${1:-}" == "--force" ]]; then
  force_render=true
elif [[ $# -gt 0 ]]; then
  printf 'Uso: %s [--force]\n' "$0" >&2
  exit 2
fi

cd "$site_dir"

while IFS= read -r -d '' source_file; do
  output_file="${source_file%.Rmd}.html"

  if [[ "$force_render" == true || ! -e "$output_file" || "$source_file" -nt "$output_file" ]]; then
    printf 'Renderizando %s\n' "$source_file"
    Rscript -e 'rmarkdown::render(commandArgs(TRUE)[1], quiet = TRUE)' "$source_file"
  else
    printf 'Sem alterações: %s\n' "$source_file"
  fi
done < <(find . -type f -name '*.Rmd' -not -path './.git/*' -print0 | sort -z)