# Global estimate of Riverine Methane Flux

Ongoing project trying to quantify CH4 emissions from rivers globally and monthly. 


## Setup
Currently the datasets are private in a google drive, so you need access to run the code in this repository. 
Code is in the `code/` folder. A summary of the code chunks is as follows. Each script is self-enclosed so welcome to jump in at any of them.

 - Script 1 downloads the hydrology layers and gets the IDs for the methane sites.
 - Script 2 obtains some new attributes: land cover, groundwater table depth and nutrient fluxes.
 - Script 3 combines all attributes into two files, one for the modelling with only the sites with CH4 concentrations and another with the 2 million catchments for upscaling.
 - Script 4 is the random forest modelling to predict CH4 concentrations globally.
 - Script 5 is to extract seasonal hydrological parameters and estimate k.
 - Script 6 is to calculate fluxes for all GRADES reaches and extrapolate to smaller streams.
 - Script 7 is to do some  maps.


## Modelling
### Pre-processing
The predictors selected are shown below. Several have been removed due to high correlation among other ones (e.g. GPP and NPP, heterotrophic soil respiration and total soil respiration, several soil properties...)

![](figures/histograms_transformed.png)


### Model performance
I did some grid-tuning to find the optimal hyperparameters for the random forest model, ending up with 
`mtry = 10, trees = 1200, min_n = 21`. I also tried running a model with the whole dataset, or an independent RF model for each month.

The random forest model seems to do OK, with an average $R^{2}= 0.5$. 

One problem I find is that monthly models are often very different among them, given that the number of sites changes a lot month to month as well as the spatial coverage. Thus it not only captures seasonal changes but also the  (changing) geographical coverage of the data. One solution to maintain spatial coherence as well as the seasonality is for each month, feed the data of the given month and the adjacent months (e.g. for march use the data form february, march and april). This makes the model more homogeneous and robust, as well as an increase in predictive power. 

![](figures/model_perf_monthly_adjacent.png)

We can also look at the importance of the model variables. Here I show the average (+- SE) from all the 12 models used.


![](figures/VIP_scores_monthly.png)

Another way to look at the role of each model is at partial dependence plots of each variable. Which shows the marginal response independently. it does not take into account potential interactions though.

![](figures/partial_depend_monthly.png)

## Upscaling

## Problems?


