# Global estimate of Riverine Methane Flux

Ongoing project trying to quantify CH4 emissions from rivers globally and monthly.

## Setup

Currently the datasets are private in a google drive, so you need access to run the code in this repository. Code is in the `code/` folder. A summary of the code chunks is as follows. Each script is self-enclosed so welcome to jump in at any of them.

-   Script 1 downloads the hydrology layers and gets the IDs for the methane sites.
-   Script 2 obtains some new attributes: land cover, groundwater table depth and nutrient fluxes.
-   Script 3 combines all attributes into two files, one for the modelling with only the sites with CH4 concentrations and another with the 2 million catchments for upscaling.
-   Script 4 is the random forest modelling to predict CH4 concentrations globally.
-   Script 5 is to extract seasonal hydrological parameters and estimate k.
-   Script 6 is to calculate fluxes for all GRADES reaches and extrapolate to smaller streams.
-   Script 7 is to do some maps and figures.

## Modelling

### Pre-processing

The predictors selected are shown below. Several have been removed due to high correlation among other ones (e.g. GPP and NPP, heterotrophic soil respiration and total soil respiration, several soil properties...)

![](figures/histograms_transformed.png)

### Model performance



