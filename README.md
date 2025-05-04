# otsuSeg

**otsuSeg** is an R package designed for the segmentation of burned areas using smoothed histograms and Otsu's thresholding method. It facilitates the analysis of fire severity and vegetation regeneration by processing raster data, particularly focusing on indices like the Relative Burn Ratio (RBR) and differenced Normalized Burn Ratio (dNBR).

## 🔍 Overview

The package provides tools to:

- Apply Otsu's method to smoothed histograms of RBR and dNBR images.
- Segment fire scars and assess post-fire vegetation regeneration.
- Handle raster mosaics and perform spatial analyses relevant to fire history studies.

## 📘 Full Documentation

[Click here to download help_otsuSeg.R.pdf](https://olgaviedma.github.io/OtsuSeg/help_otsuSeg.R.pdf)
This documentation includes examples, illustrations, and step-by-step workflows to guide the use of the package.

## Authors
Hammadi Achour, Olga Viedma, Zina Soltani, Imene Habibi, Wahbi Jaouadi


## 📦 Installation

To install the latest version of `otsuSeg` from GitHub:

```r
# Install the 'remotes' package if not already installed
install.packages("remotes")

# Install 'otsuSeg' from GitHub
remotes::install_github("olgaviedma/otsuSeg")

