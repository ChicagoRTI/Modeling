# ModelPredictorRanking
The R script to analyze the model predictors using the AIC metrics. It does the following:

*  Create a set of models with identical predictor variables for each common species
*  Using AIC metrics, ranks each predictor variable for each common species

The output is a spreadsheet with the results tabulated.

To run the program, set your current directory to the R project then run these commands:
````
source (source(paste(getwd(), '/analyze_all_models_V4.r', sep='')))
report_common_species()
````
