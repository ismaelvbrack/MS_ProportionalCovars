# MS_ProportionalCovars

This repository contains data and code scripts for the manuscript "*Estimation and interpretation problems and solutions when using proportion covariates in linear regression models*". We provide an R tutorial to create conditional and marginal plots, code to simulate data with proportional covariates and analyze with different modelling approaches and code to run the BBS example. 

## Authors
Denis Valle, Jeffrey Mintz, Ismael V. Brack

## Repository content

-   `Appendix 2 code.R`: Tutorial for creating conditional 
    and marginal plots using `base R` and `ggplot` 

-   `simul/`: code to simulate data and analyze under different GLM, GLMM and GAM models (MLE and Bayesian)
    - `fake data.R`: simulate data
    - `models.R`: run GLM, GLMM and GAM using maximum likelihood estimation
    - `run JAGS poisson.R`: run Poisson GLM using JAGS model `JAGS poisson regression.R`
    - `model selection AIC.R`: conduct model selection using AIC (rely on `useful functions.R`)
    - `model selection BIC`: conduct model selection using BIC (rely on `useful functions.R`)

-   `BBS_example/`: code to run the example using North American Breeding Bird Surveys and National Land Cover Database
    -   `data/`: BBS and LULC processed data
    -   `GIS/`:
        -   `BBSrte*`: route centroids shapefiles
        -   `nlcd_2013*`: 2013 LULC raster. Large files not included in this repo, download from: [NLCD](https://www.mrlc.gov/data?f%5B0%5D=category%3Aland%20cover&f%5B1%5D=year%3A2013)
    -   `JAGS_models/`: 3 JAGS models: 1) full model; 2) without 1 covar; 3) without intercept
    -   `outputs/`: JAGS outputs with posterior samples
    -   `extract_LULC_BBSroutes.R`: extract LULC classes (grouped into 6) 
        within a buffer around each route centroid.
    -   `filter_BBSdata.R`: filter BBS data for 2013 and exclude missing data
    -   `run_model_bayes.R`: fit the 4 models using JAGS
    -   `conditional_plots.R`: make predictions and create panel with conditional plots
    -   `marginal_plots.R`: make predictions and create panel with marginal plots
    
- `useful functions`: supporting functions
    
## Data bases
#### BBS database
BBS data containing richness estimates of native birds for 2013 and routes centroids were obtained from Forest Service Resaerch Data Archive:

[![DOI:10.2737/RDS-2021-0001](https://img.shields.io/badge/DOI-10.2737%2FRDS--2021--0001-o?logo=doi&color=red&link=https%3A%2F%2Fdoi.org%2F10.2737%2FRDS-2021-0001)](https://doi.org/10.2737/RDS-2021-0001)

\\

#### Land Use / Land Cover
2013 LULC data was downloaded from the National Land Cover Database:

[![DOI:10.5066/P9KZCM54](https://img.shields.io/badge/DOI-10.5066%2FP9KZCM54-o?logo=doi&color=green&link=https%3A%2F%2Fwww.mrlc.gov%2Fdata%3Ff%255B0%255D%3Dcategory%253Aland%2520cover%26f%255B1%255D%3Dyear%253A2013)](https://www.mrlc.gov/data?f%5B0%5D=category%3Aland%20cover&f%5B1%5D=year%3A2013)


