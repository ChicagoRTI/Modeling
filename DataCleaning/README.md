# DataCleaning
R scripts for cleaning up the input data

Each new set of data needs to be cleaned up to make it conform to the format expected by the other tools - especially the shiny visualization app.  So the target of the cleanup script is always the format required by the equivalent version shiny app. 

Data cleaning includes:
* Changing column names to the standard names
* Coercing all trees identified as street trees to "Land use = Street Tree" (regardless of its assigned land use)
* Removing records where DATATYPE != 'Point"
* Removing unused columns
* Removing records where a value exceeds specified bounds

## Future updates
* Pass the input dataset file location to the script
* Pass the output directory to the script 
