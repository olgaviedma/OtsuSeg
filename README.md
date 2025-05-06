# OtsuSeg

**OtsuSeg** is an R package designed for the segmentation of burned areas using smoothed histograms and Otsu's thresholding method applied to spectral indices such as the Normalized Burn Ratio (NBR).

## 🔍 Overview

The package provides tools to:

- Apply Otsu's method to smoothed histograms of Relative Burn Ratio (RBR) and delta Normalized Burn Ratio (dNBR) images.
- Segment fire scars and assess the accuracy of burn area maps.

## 📁 Example Data (External)

Large example datasets are hosted externally and are **not bundled in the CRAN/GitHub package** to comply with size limits.

These files are hosted at:
https://github.com/olgaviedma/OtsuSeg/releases/tag/v0.1.0

To download and access example raster data (e.g., `NBRpre.tif`, `NBRpost.tif`), use:

```r
library(OtsuSeg)

# Download and extract example data
get_external_data()

# Load a specific file as a raster object
pre_fire <- get_external_data("NBRpre.tif", load = TRUE)
post_fire <- get_external_data("NBRpost.tif", load = TRUE)

```

## 📘 Full Documentation

[Click here to download help_otsuSeg.R.pdf](https://olgaviedma.github.io/OtsuSeg/)
This documentation includes examples, illustrations, and step-by-step workflows to guide the use of the package.

**Authors:** Hammadi Achour, Olga Viedma, Zina Soltani, Imene Habibi, Wahbi Jaouadi


## 📦 Installation

To install the latest version of `OtsuSeg` from GitHub:

```r
# Install the 'remotes' package if not already installed
install.packages("remotes")

# Install 'otsuSeg' from GitHub
remotes::install_github("olgaviedma/OtsuSeg")

```



