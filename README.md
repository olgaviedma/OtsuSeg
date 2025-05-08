# OtsuSeg

**OtsuSeg** is an R package designed for the segmentation of burned areas using smoothed histograms and Otsu's thresholding method applied to spectral indices such as the Normalized Burn Ratio (NBR).

**Authors:** Hammadi Achour, Olga Viedma, Zina Soltani, Imene Habibi, Wahbi Jaouadi

## 🔍 Overview

The package provides tools to:

- Apply Otsu's method to smoothed histograms of Relative Burn Ratio (RBR) and delta Normalized Burn Ratio (dNBR) images.
- Segment fire scars and assess the accuracy of burn area maps.

## 📁 Example Data (External) and Usage

Large example datasets are hosted externally and are **not bundled in the CRAN/GitHub package** to comply with size limits.

These files are hosted at:
https://github.com/olgaviedma/OtsuSeg/releases/tag/v0.1.0


```r
library(OtsuSeg)

# Download and extract example data
get_external_data()

# 1.Load a specific file as a raster object
NBRpre <- get_external_data("NBRpre.tif", load = TRUE)
NBRpost <- get_external_data("NBRpost.tif", load = TRUE)
shapefile_reference <- get_external_data("shapefile_reference.shp", load = TRUE)

## 2.Fire scars segments
shapefile_path <- file.path("D:/OLGA/binary_segments.shp")
result <- binarize_raster(NBRpre, NBRpost, shapefile_path = shapefile_path)

print(result$area_hectares)

raster<-result$binary_raster_smoothed
polygons<-st_as_sf(result$binary_shapefile)

plot(raster, main = "Binary Raster (Smoothed)")
plot(polygons, main = "Binary Shapefile")

## 3.Accuracy assessmet
binary_shape <-polygons
reference_shape <- st_as_sf(shapefile_reference)

# Apply Quality Control
result2 <- Quality_control(binary_shape, reference_shape)
print(result2)

```
## 📘 Full Documentation

- 👉 [Click here to view the full HTML help file](https://olgaviedma.github.io/OtsuSeg/)
- 📄 [Download PDF version of the help](https://olgaviedma.github.io/OtsuSeg/help_otsuSeg.R.pdf)

This documentation includes examples, illustrations, and step-by-step workflows to guide the use of the package.

🛈 Note: If the links stop working after the first click, this is a known issue with GitHub Pages navigation and browser cache.
The links are correct and stable, but the browser may cache or misroute when going back from GitHub Pages. 
For the best experience, open HTML and PDF documents in a new window or tab (right-click the link and choose "Open in New Tab").


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



