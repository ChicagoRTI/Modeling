
# source(paste(getwd(), '/confusion_matrix.r', sep=''))


create_confusion_matrix <- function(LU=13)
{
      library(doParallel)
      library(foreach)

      ctree_local_file <- 'D:/CRTI/data/cleaned/dupage_county_accepted_V4.csv'
      ctree <- read.delim(ctree_local_file, as.is=TRUE, sep=',') 
      
      # Get the land use categories that have at least 400 observations
      land_use <- table(ctree$LU)
      land_use <- land_use[land_use>400]
      
      # Filter out records that are not in these land use categories
      ctree <-ctree[ctree$LU %in% as.numeric(rownames(land_use)),]

      
      all_predictors <- c(
            'DBH_IN',
            'CROWN_COMPACTNESS',
            'HEIGHT_MAX',
            'HEIGHT_MEAN',
            'CROWN_AREA',
            'BLDG_AGE',
            'HU_DENS',
            'RELBORDER_TREE')
      
      # Create a single list that contains the top 10 (aka "abundant") species in each land use category
      top_spps = NULL
      for (l in as.numeric(rownames(land_use)))
      {
            top_spps <- append(top_spps, names(head(sort(table(factor(ctree$GENUSSPECI[ctree$LU==l])), decreasing = TRUE), 10)))
      }
      top_spps <- unique(top_spps)
      
      # Coerce all non-abundant species to "Other"
      ctree$GENUSSPECI <- ifelse ((match(ctree$GENUSSPECI, top_spps, nomatch = 0) > 0), ctree$GENUSSPECI, "Other")
      top_spps <- c(top_spps, "Other")
      
      threads <- 6
      cl<-makeCluster(threads)
      registerDoParallel(cl)
      
      # Filter on land use
      ctree_lu <- ctree[ctree$LU==LU,]
      # Split the data in half
      ctree_split <- split(ctree_lu, sample(1:2, nrow(ctree_lu), replace=T))
      # Training data
      ctree_train <- ctree_split[[1]]
      # Verification data - each thread gets its share
      ctree_test <- split(ctree_split[[2]], sample(1:threads, nrow(ctree_split[[2]]), replace=T))
      rm(ctree_lu)
      rm(ctree_split)
      
      formula <- paste('occur ~ ', paste(all_predictors,collapse='+'))
      models <- list()

      for (spp in top_spps)
      {
            if (nrow(ctree_train) > 0)
            {
                  ctree_train_spp <- ctree_train
                  ctree_train_spp[,'occur'] <- 0
                  ctree_train_spp[ctree_train_spp[,'GENUSSPECI']==spp, 'occur'] <- 1
                  models[[spp]] <- glm(formula=as.formula(formula),family=binomial(link='logit'),data=ctree_train_spp)
                  models[[spp]]$data <- NULL
                  models[[spp]]$y <- NULL
                  models[[spp]]$linear.predictors <- NULL
                  models[[spp]]$weights <- NULL
                  models[[spp]]$fitted.values <- NULL
                  models[[spp]]$models[[spp]] <- NULL
                  models[[spp]]$prior.weights <- NULL
                  models[[spp]]$residuals <- NULL
                  models[[spp]]$effects <- NULL
                  models[[spp]]$qr$qr <- NULL
            }
      }
      

      foreach (p=seq(1:threads)) %dopar% 
#      for (p in seq(1:threads))
      {
            ctree_test_p <- ctree_test[[p]]
            
            #FOR TEST
#            ctree_test_p <- ctree_test_p[1:10,]
            
            # Run a prediction for each record in the test data set.
            rows <- nrow(ctree_test[[p]])
            results <- matrix(nrow=rows, ncol=3)
            results_i <- 0
            for (i in rownames(ctree_test_p))
            {
                  max_probability <- 0
                  predicted_species = "None"
                  prediction_input <- as.data.frame(ctree_test_p[i,all_predictors])
                  for (spp in top_spps)
                  {
                        model <- models[[spp]]
                        probability <- predict(model, newdata=prediction_input, type="response")
                        if (is.finite(probability) && probability > max_probability)
                        {
                              predicted_species <- spp
                              max_probability <- probability
                        }
                  }
                  results_i <- results_i + 1
                  results[results_i,] = c(object_id=ctree_test_p[i,'OBJECTID'], observed_species=ctree_test_p[i,'GENUSSPECI'], predicted_species=predicted_species)
#                  cat(object_id=ctree_test_p[i,'OBJECTID'],ctree_test_p[i,'GENUSSPECI'],predicted_species, "\n", sep=" ")
            }
            # Save this processor's results to disk
            saveRDS(results, file=paste(getwd(), '/predictions_', p, '.rds', sep=''))
      }
      stopCluster(cl)
      
      # Read the each processor's results from disk and concatenate them
      final_results <- matrix(nrow=0, ncol=3)
      for (p in seq(1:threads))
      {
            final_results <- rbind(final_results, readRDS(paste(getwd(), '/predictions_', p, '.rds', sep='')))
      }
      
      # Convert to data frame, sort on object ID and write results to CSV file
      df <- data.frame(final_results, stringsAsFactors=FALSE)
      colnames(df) <- c("Object ID", "Observed Species", "Predicted Species")
      df <- df[order(as.numeric(df[['Object ID']])),]
      write.csv(df, paste(getwd(), '/predictions.csv', sep=''), row.names=FALSE)
      
      # Remove "None" predictions and build a confusion table
      ct <- df[df[['Predicted Species']]!="None",]
      ct<-table(ct[,2:3])
#      print(ct)
      
      # Print the same table with percentages instead of counts
      ct_percent <- round(ct/rowSums(ct),digits=4)
#      print (ct_percent)
      
      # Find the missing columns (species observed but never predicted) and append them to the table filled with zeroes
      missing_cols <- setdiff(rownames(ct), colnames(ct))
      if (length(missing_cols) > 0)
      {
            ext_matrix <- matrix(0, nrow=nrow(ct), ncol=length(missing_cols))
            colnames(ext_matrix) <- missing_cols
            ct <- cbind(ct,ext_matrix)
      }

      # Find the missing rows (species predicted but never observed) and append them to the table filled with zeroes
      missing_rows <- setdiff(colnames(ct), rownames(ct))
      if (length(missing_rows) > 0)
      {
            ext_matrix <- matrix(0, nrow=length(missing_rows), ncol=ncol(ct))
            rownames(ext_matrix) <- missing_rows
            ct <- rbind(ct,ext_matrix)
      }
      
      # For each observed species create a confusion summary (true positives,  false positives, etc)
      ct_summary <- matrix(nrow=nrow(ct), ncol=4)
      rownames(ct_summary) <- rownames(ct)
      colnames(ct_summary) <- c("True positives", 'False positives', 'False negatives', 'True negatives')
      for (spp in rownames(ct))
      {
            true_positives <- ct[spp,spp]
            false_positives <- sum(ct[,spp]) - true_positives
            false_negatives <- sum(ct[spp,]) - true_positives
            true_negatives <- sum(ct) - true_positives - false_positives - false_negatives
            ct_summary[spp,] <- c(true_positives, false_positives, false_negatives, true_negatives)
      }
#      print (ct_summary)   
      
      # Compute accuracy statistics - from https://en.m.wikipedia.org/wiki/Confusion_matrix
      stats_cols = c("P","N","TP","TN","FP","FN","TPR","TNR","PPV","NPV","FNR","FPR","FDR","FOR","ACC","F1","MCC", "BM", "MK")
      stats <- matrix(nrow=nrow(ct_summary), ncol=length(stats_cols))
      rownames(stats) <- rownames(ct_summary)
      colnames(stats) <- stats_cols
      for (spp in rownames(ct_summary))
      {
            stats[spp,"P"] <- ct_summary[spp,'True positives'] + ct_summary[spp,'False negatives']
            stats[spp,"N"] <- ct_summary[spp,'False positives'] + ct_summary[spp,'True negatives']
            stats[spp,"TP"] <- ct_summary[spp,'True positives'] 
            stats[spp,"TN"] <- ct_summary[spp,'True negatives'] 
            stats[spp,"FP"] <- ct_summary[spp,'False positives'] 
            stats[spp,"FN"] <- ct_summary[spp,'False negatives'] 
            stats[spp,"TPR"] <- stats[spp,"TP"]/stats[spp,"P"]
            stats[spp,"TNR"] <- stats[spp,"TN"]/stats[spp,"N"]
            stats[spp,"PPV"] <- stats[spp,"TP"]/(stats[spp,"TP"] + stats[spp,"FP"])
            stats[spp,"NPV"] <- stats[spp,"TN"]/(stats[spp,"TN"] + stats[spp,"FN"])
            stats[spp,"FNR"] <- 1 - stats[spp,"TPR"]
            stats[spp,"FPR"] <- 1 - stats[spp,"TNR"]
            stats[spp,"FDR"] <- 1 - stats[spp,"PPV"]
            stats[spp,"FOR"] <- 1 - stats[spp,"NPV"]
            stats[spp,"ACC"] <- (stats[spp,"TP"]+stats[spp,"TN"])/(stats[spp,"P"]+stats[spp,"N"])
            stats[spp,"F1"] <- 2*((stats[spp,"PPV"]*stats[spp,"TPR"])/(stats[spp,"PPV"]+stats[spp,"TPR"]))
            stats[spp,"MCC"] <- ((stats[spp,"TP"]*stats[spp,"TN"])-(stats[spp,"FP"]*stats[spp,"FN"])) / sqrt( (stats[spp,"TP"]+stats[spp,"FP"]) * (stats[spp,"TP"]+stats[spp,"FN"]) * (stats[spp,"TN"]+stats[spp,"FP"]) * (stats[spp,"TN"]+stats[spp,"FN"]) ) 
            stats[spp,"BM"] <- stats[spp,"TPR"] + stats[spp,"TNR"] - 1
            stats[spp,"MK"] <- stats[spp,"PPV"] + stats[spp,"NPV"] - 1
      }
#      print (round(stats, digits=4))
      results <- list(df,ct,ct_summary,stats)
      names(results) <- c('predictions', 'counts', 'counts_summary', 'confusion_matrix')
      return (results)
}



