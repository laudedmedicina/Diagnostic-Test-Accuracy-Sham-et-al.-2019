# Diagnostic Test Accuracy (Sham et al., 2019)

This repository contains R code based on a study published by Sham et al., 2019 used to perform univariate and bivariate meta-analysis of diagnostic test accuracy (DTA) based on the methods described by Sham et al. (2019). The script includes calculations for sensitivity, specificity, diagnostic odds ratio, and SROC curves.

---

## Files

- `Diagnostic Test Accuracy (Sham et al., 2019).R` — R script with full analysis code
- `DTA.xlsx` — Input data (not included in repo; you must add it)

---

## Required R Packages

Make sure the following R packages are installed:

```r
install.packages(c("mada", "mvtnorm", "ellipse", "mvmeta", "meta", "metafor", "readxl"))
