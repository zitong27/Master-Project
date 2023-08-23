#!/bin/bash
# Desc: compiles LaTeX report


pdflatex coursework.tex
bibtex coursework.aux
pdflatex coursework.tex
pdflatex coursework.tex

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg
rm *.toc
rm *.out
rm *.thm
rm *.brf
echo "Compiled LaTeX report"

