# source("D:/CRTI/r_projects/Modeling/RandomForest/rf.r")
# run()

library(stringr)
library(randomForest)
library(caret)
library(e1071)


DATA_PATH = 'https://don-morrison-2000.github.io/data/glendale_accepted_V1.csv'
PREDICTORS <- c("LU", "DBH_IN", "CROWN_COMPACTNESS", "HEIGHT_MAX", "HEIGHT_MEAN", "CROWN_AREA", "BLDG_AGE", "HU_DENS", "RELBORDER_TREE", "NDVI_MEAN")
NUM_RECORDS <- -1  # -1 for all records
NUM_SPECIES <- 5
LAND_USE <- read.delim('https://don-morrison-2000.github.io/data/land_use.csv', as.is=TRUE, header=FALSE)



run <- function()
{
      # Read the input csv file
      all_data = tree_data = read.delim (DATA_PATH, as.is = TRUE, sep=",", header = TRUE)
      # Remove records with NA - should figure out how to make the model tolerate NAs
      filtered_data <- na.omit(all_data)
      # Select just the specified number of records (to make it run faster for testing)
      filtered_data <- head(filtered_data, NUM_RECORDS)
      # Select just the columns we need for the model
      filtered_data <- filtered_data[,c("GENUSSPECI", PREDICTORS)]
      # Fold the species into the genus
      filtered_data$GENUSSPECI <- word(filtered_data$GENUSSPECI,1)
      # Get the names of the most abundant genera
      top_genera <- sort(names(head(sort(table(factor(filtered_data$GENUSSPECI)), decreasing = TRUE), NUM_SPECIES)))
      # Select the records of the most abundant genera
      filtered_data <- filtered_data[filtered_data$GENUSSPECI %in% top_genera,]
      # Make the dependent variable a factor (to tell the model that this is classification vs regression)
      filtered_data$GENUSSPECI <- as.factor(filtered_data$GENUSSPECI)
      # Convert land use numbers to descriptive names (and factors)
      filtered_data$LU <- as.factor(LAND_USE$V2[as.numeric(as.character(filtered_data$LU))])
      # Partition the data into traning (2/3) and test (1/3)
      set.seed(123)
      train_data_indicies <- sample(seq_len(nrow(filtered_data)), size = floor(0.666 * nrow(filtered_data)))
      train_data <- filtered_data[train_data_indicies, ]
      test_data <- filtered_data[-train_data_indicies, ]

      # Create the model
      formula = as.formula(paste('GENUSSPECI ~ ', paste(PREDICTORS,collapse='+')))
      rf_model <- randomForest(formula, data=train_data, mtry=3, importance=TRUE, proximity=TRUE, na.action=na.omit)
      print (rf_model)
      print (importance(rf_model))
      
      ## Do MDS (multi-dimensional scaling) on 1 - proximity:
      # rf_mds <- cmdscale(1 - rf_model$proximity, eig=TRUE)
      # op <- par(pty="s")
      # pairs(cbind(train_data[,1:length(colnames(train_data))], rf_mds$points), cex=0.6, gap=0,
      #       col=c(rainbow(NUM_SPECIES))[as.numeric(train_data$GENUSSPECI)],
      #       main="Glenview Data: Predictors and MDS of Proximity Based on RandomForest")
      # par(op)
      # print(rf_model$GOF)
      
      # Variable Importance Plot
      varImpPlot(rf_model,
                 sort = T,
                 main="Variable Importance",
                 n.var=length(PREDICTORS))

      # Variable Importance Table
      var_imp <- data.frame(importance(rf_model, type=2))
      var_imp$Variables <- row.names(var_imp)
      print(var_imp[order(var_imp$MeanDecreaseGini,decreasing = T),])
            
      # Create Confusion Matrix
      test_data$predicted.response <- predict(rf_model, test_data)
      cm <- confusionMatrix(data=test_data$predicted.response, reference=test_data$GENUSSPECI, positive='yes')
      print(cm)
}