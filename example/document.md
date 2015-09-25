---
title: Makedoc
author: Diogo Ramos
date: September 2015
bibliography: document.bib
template: template.tex
...


Makedoc is a converter using Pandoc and LaTeX to generate a Docx or PDF from a
Markdown source. It acts basically as a substitute for Make.

I started developing it when I had to write a Pandoc filter for my academic
thesis, and wondered if I could substitute my makefile and Perl patches
entirely by a single Haskell program. Since it's still being useful for other
academic essays I write, I decided to publish it (althrough it's very tailored
for my personal workflow, and I may or may not develop it further).

# Usage

Assuming there is a file called `document.md` in the working directory, call:

```sh
makedoc document.docx  # to generate a Word document
makedoc document.pdf   # to generate a PDF
makedoc document.tex   # to generate a LaTeX source
makedoc clean          # to cleanup LaTeX's temporary files
```

Makedoc uses the source document's YAML metadata [see @pandoc] to specify a
`template` for the output format, and a `bibliography` with any
bibliographic source files. If the target format is Docx, or LaTeX without
natbib/biblatex packages, the references will be processed with pandoc-citeproc
(in this case the `csl` key may specify a citation style file).

# Compilation

```sh
git clone --depth 1 https://github.com/ogoid/makedoc
cd makedoc
cabal sandbox init
cabal install
cp .cabal-sandbox/bin/makedoc ~/.cabal/bin    # or anywhere else in your $PATH
```
