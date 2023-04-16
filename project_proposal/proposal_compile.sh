#!/bin/bash
# Desc: compiles LaTeX report


pdflatex project_proposal.tex
bibtex project_proposal.aux
pdflatex project_proposal.tex
pdflatex project_proposal.tex

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg
echo "Compiled LaTeX report"

