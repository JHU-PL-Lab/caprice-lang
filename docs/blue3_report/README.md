## One shot generate pdf:

pandoc programming_blue3.md \
  --from=markdown+tex_math_dollars+fenced_code_attributes \
  --lua-filter=code_labels.lua \
  --filter=mermaid-filter \
  --pdf-engine=xelatex \
  -H unicode-fixes.tex \
  -V monofont="DejaVu Sans Mono" \
  -o programming_blue3.pdf
