# source ('D:/CRTI/r_projects/analyze_models/analyze_all_models.r')


library(gtools)

threads <- 10
min_predictors <- 8
max_predictors <- 8
p <- sort(c("BLDG_AGE", 'HU_DENS', 'DIST_WATER', 'DIST_MINOR', 'DIST_MAJOR', 'PD', 'LPI', 'LSI', 'CROWN_AREA', 'CROWN_COMPACTNESS', 'HEIGHT_MAX', 'HEIGHT_MEAN', 'RELBORDER_TREE', 'DBH_IN'))

test_performance <- function()
{
      for (i in seq(1:10))
      {
            threads <<- i
            build_models()
      }
}


# Create the predictor formula permutations
create_formulas <- function ()
{
      for (i in seq(min_predictors, max_predictors))
      {    
#        x <- apply(permutations(n = length(p), r = i, v = p),1, paste, collapse='+')
            x <- apply(combn(p,i),c(2), paste, collapse='+')
            write.csv(x, file=paste('D:/CRTI/r_projects/analyze_models/output/all_model_formulas_', i, '.csv', sep=''), row.names=FALSE)
      }
}


# Recombine the files that split them evently
concat_files <- function()
{
      # Combine the individual files
      x <- character(0)
      for (i in seq(min_predictors,max_predictors))
      {
            y <- read.csv (paste('D:/CRTI/r_projects/analyze_models/output/all_model_formulas_', i, '.csv', sep=''), as.is = TRUE, header = TRUE)
            x <- rbind(x,y)
      }
      
      # Split it up in to equal size files that will be farmed out to a dedicated thread
      x <- split(x, rep(1:threads, each=nrow(x)/threads, length.out=nrow(x)))
      for (i in seq(1:threads))
      {
            write.csv(x[[i]], file=paste('D:/CRTI/r_projects/analyze_models/output/all_model_formulas_P', i, '.csv', sep=''), row.names=FALSE)
      }
}


# Create parallel threads, each one analyzing one share of predictors
build_models <- function() 
{
      library(doParallel)
      library(foreach)
      cl<-makeCluster(threads)
      registerDoParallel(cl)
      
      species <- 'Acer saccharinum'
      chicago_tree_data = read.csv ('D:/CRTI/data/cleaned/dupage_county_accepted_V1.csv', as.is = TRUE, header = TRUE)

      
      
      p <- c(1:threads)
      foreach (x=p) %dopar% {
#      for (x in p) {
                  
            chunk_size = 100
            
            chicago_tree_data[,'occur']=0
            chicago_tree_data[chicago_tree_data[,'GENUSSPECI']==species,'occur']=1
            
            formulas = read.csv (paste('D:/CRTI/r_projects/analyze_models/output/all_model_formulas_P', x, '.csv', sep=''), as.is = TRUE, header = TRUE)
            
            for (j in seq(1, nrow(formulas), chunk_size))
            {
                  aics = matrix(NA, nrow=chunk_size, ncol=2)
                  for (k in seq(1, min(chunk_size, nrow(formulas)-j+1)))
                  {
                        index = j + k - 1    
                        fullformula <- as.formula(paste('occur ~ ', formulas[[1]][index])) 
                        model=glm(formula=fullformula,family=binomial(link='logit'),data=chicago_tree_data)
                        aics[k,1] <- index
                        aics[k,2] <- as.integer(summary(model)$aic) 
                  }
                  write.table(aics, file=paste('D:/CRTI/r_projects/analyze_models/output/all_model_AICs_', x, '.csv', sep=''), sep=',', col.names=FALSE, row.names=FALSE, append=TRUE)
                  aics = NULL
            }
      }
      stopCluster(cl)
}

# Rank the models from one to n
analyze_results <- function ()
{

      results <- data.frame()
      for (i in seq(1:threads))
      {
            r <- read.csv (paste('D:/CRTI/r_projects/analyze_models/output/all_model_AICs_', i, '.csv', sep=''), as.is = TRUE, header = FALSE) 
            names(r) <- c("index", "aic")
            r <- r[complete.cases(r),]
            r <- cbind (partition=i, r)
            
            f = read.csv (paste('D:/CRTI/r_projects/analyze_models/output/all_model_formulas_P', i, '.csv', sep=''), as.is = TRUE, header = TRUE)
            names(f) <- c("formula")
            f <- head(f,nrow(r))
            
            results <- rbind(results, cbind (f, r))
      }

      # Sort the results, add a rank, columnn then write out the results
      ordered_results <- results[order(results$aic),]
      ordered_results$rank <- seq.int(nrow(ordered_results))
      write.table(ordered_results, file=paste('D:/CRTI/r_projects/analyze_models/output/all_model_AICs_ordered.csv', sep=''), sep=',', col.names=TRUE, row.names=FALSE)
      return (head(ordered_results,20))
}

# Rank the predictors based on mean AIC
score_predictors <- function()
{
      a <- read.csv ('D:/CRTI/r_projects/analyze_models/output/all_model_AICs_ordered.csv', as.is = TRUE, header = TRUE)
      scorecard <- numeric()
      for (pred in p)
      {
            scorecard[pred] <- mean(a[grepl(pred,a$formula),]$aic)
      }
      scorecard <- sort(scorecard)
      write.table(scorecard, file='D:/CRTI/r_projects/analyze_models/output/scorecard.csv', sep=',', col.names=TRUE, row.names=TRUE)
      return (scorecard)

}

