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
NBRpre <- get_external_data("NBRpre")
NBRpost <- get_external_data("NBRpost")
NBRpre<-raster(NBRpre)
NBRpost<-raster(NBRpost)

```
## 📘 Full Documentation

- 👉 [Click here to view the full HTML help file](https://olgaviedma.github.io/OtsuSeg/)
- 📄 [Download PDF version of the help](https://olgaviedma.github.io/OtsuSeg/help_otsuSeg.R.pdf)

This documentation includes examples, illustrations, and step-by-step workflows to guide the use of the package.


This documentation includes examples, illustrations, and step-by-step workflows to guide the use of the package.


**Authors:** Hammadi Achour, Olga Viedma, Zina Soltani, Imene Habibi, Wahbi Jaouadi


## 📦 Installation

To install the latest version of `OtsuSeg`:

```r
#The CRAN version:
install.packages("OtsuSeg")

# Install the 'remotes' package if not already installed
install.packages("remotes")

# Install 'otsuSeg' from GitHub
remotes::install_github("olgaviedma/OtsuSeg")

```



