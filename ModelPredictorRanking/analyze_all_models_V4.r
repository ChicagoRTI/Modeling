# source(paste(getwd(), '/analyze_all_models_V4.r', sep=''))
# report_common_species()


library(gtools)

threads <- 8
min_predictors <- 4
max_predictors <- 4
p <- sort(c("BLDG_AGE", 'HU_DENS', 'CROWN_AREA', 'CROWN_COMPACTNESS', 'HEIGHT_MAX', 'HEIGHT_MEAN', 'RELBORDER_TREE', 'DBH_IN', 'NDVI_MEAN', 'NDVI_MAX', 'NDVI_STD'))
#p <- sort(c("BLDG_AGE", 'HU_DENS', 'DIST_WATER', 'DIST_MINOR', 'DIST_MAJOR', 'PD', 'LPI', 'LSI', 'CROWN_AREA', 'CROWN_COMPACTNESS', 'HEIGHT_MAX', 'HEIGHT_MEAN', 'RELBORDER_TREE', 'DBH_IN', 'NDVI_MEAN', 'NDVI_MAX', 'NDVI_SD'))
#p <- sort(c("BLDG_AGE", 'HU_DENS', 'CROWN_AREA', 'CROWN_COMPACTNESS', 'HEIGHT_MAX', 'HEIGHT_MEAN', 'RELBORDER_TREE', 'DBH_IN', 'HEIGHT_MAX:CROWN_AREA','HEIGHT_MEAN:CROWN_AREA' ))
common_species <- sort(unique(read.delim('https://don-morrison-2000.github.io/data/common_species.csv', as.is=TRUE, header=FALSE)$V1))

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
            write.csv(x, file=paste(getwd(), '/output/all_model_formulas_', i, '.csv', sep=''), row.names=FALSE)
      }
}


# Recombine the files then split them evently
concat_files <- function()
{
      # Combine the individual files
      x <- character(0)
      for (i in seq(min_predictors,max_predictors))
      {
            y <- read.csv (paste(getwd(), '/output/all_model_formulas_', i, '.csv', sep=''), as.is = TRUE, header = TRUE)
            x <- rbind(x,y)
      }
      
      # Split it up in to equal size files that will be farmed out to a dedicated thread
      threads <<- min(nrow(x),threads)
      x <- split(x, rep(1:threads, each=nrow(x)/threads, length.out=nrow(x)))
      for (i in seq(1:threads))
      {
            write.csv(x[[i]], file=paste(getwd(), '/output/all_model_formulas_P', i, '.csv', sep=''), row.names=FALSE)
      }
}


# Create parallel threads, each one analyzing one share of predictors
build_models <- function(species='Acer saccharinum') 
{
      library(doParallel)
      library(foreach)
      cl<-makeCluster(threads)
      registerDoParallel(cl)
      
      chicago_tree_data = read.csv ('D:/CRTI/data/cleaned/glendale_accepted_V1.csv', as.is = TRUE, header = TRUE)

      dir.create(file.path(getwd(), '/output/', species), showWarnings = FALSE)
      
      p <- c(1:threads)
      foreach (x=p) %dopar% {
#      for (x in p) {
                  
            chunk_size = 100
            
            chicago_tree_data[,'occur']=0
            chicago_tree_data[chicago_tree_data[,'GENUSSPECI']==species,'occur']=1
            
            formulas = read.csv (paste(getwd(), '/output/all_model_formulas_P', x, '.csv', sep=''), as.is = TRUE, header = TRUE)
            
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
                  write.table(aics, file=paste(getwd(), '/output/', species, '/all_model_AICs_', x, '.csv', sep=''), sep=',', col.names=FALSE, row.names=FALSE, append=TRUE)
                  aics = NULL
            }
      }
      stopCluster(cl)
}

# Rank the models from one to n
analyze_results <- function (species='Acer saccharinum')
{

      results <- data.frame()
      for (i in seq(1:threads))
      {
            r <- read.csv (paste(getwd(), '/output/', species, '/all_model_AICs_', i, '.csv', sep=''), as.is = TRUE, header = FALSE) 
            names(r) <- c("index", "aic")
            r <- r[complete.cases(r),]
            r <- cbind (partition=i, r)
            
            f = read.csv (paste(getwd(), '/output/all_model_formulas_P', i, '.csv', sep=''), as.is = TRUE, header = TRUE)
            names(f) <- c("formula")
            f <- head(f,nrow(r))
            
            results <- rbind(results, cbind (f, r, row.names=NULL))
      }

      # Sort the results, add a rank, columnn then write out the results
      ordered_results <- results[order(results$aic),]
      ordered_results$rank <- seq.int(nrow(ordered_results))
      write.table(ordered_results, file=paste(getwd(), '/output/', species, '/all_model_AICs_ordered.csv', sep=''), sep=',', col.names=TRUE, row.names=FALSE)
      return (head(ordered_results,20))
}

# Rank the predictors based on mean AIC
score_predictors <- function(species='Acer saccharinum')
{
      a <- read.csv (paste(getwd(), '/output/', species, '/all_model_AICs_ordered.csv', sep=''), as.is = TRUE, header = TRUE)
      scorecard <- numeric()
      for (pred in p)
      {
            pred_grepable <- gsub('\\*', '\\\\*', pred)
            pred_grepable <- gsub('\\:', '\\\\:', pred_grepable)
            scorecard[pred] <- mean(a[grepl(pred_grepable,a$formula),]$aic)
            #scorecard[pred] <- mean(a[grepl(pred,a$formula),]$aic)
      }
      scorecard <- sort(scorecard)
      write.table(scorecard, file=paste(getwd(), '/output/scorecard.csv',sep=''), sep=',', col.names=TRUE, row.names=TRUE)
      return (scorecard)

}

report_common_species <- function()
{
      report_header <- t(c('Species',p[order(p)]))
      write.table(report_header, file=paste(getwd(), '/output/common_species_aic.csv', sep=''), sep=',', col.names=FALSE, row.names=FALSE)
      write.table(report_header, file=paste(getwd(), '/output/common_species_rank.csv', sep=''), sep=',', col.names=FALSE, row.names=FALSE)
      
      create_formulas()
      concat_files()
      for (spp in common_species)
      {
            cat("Processing ",spp, "\n")
            ifelse(!dir.exists(paste(getwd(), '/output/', spp, sep='')), dir.create(paste(getwd(), '/output/', spp, sep='')), FALSE)
            build_models(spp)
            analyze_results(spp)
            s1 <- round(score_predictors(spp),0)
            s1_ordered <- t(s1[order(names(s1))])
            write.table(cbind(species=spp, s1_ordered), file=paste(getwd(), '/output/common_species_aic.csv', sep=''), sep=',', col.names=FALSE, row.names=FALSE, append=TRUE)
            
            s1_ranked <- s1_ordered
            for (pred in p)
            {
                  s1_ranked[1,pred] <- which(names(s1)==pred)
            }
            write.table(cbind(species=spp,s1_ranked), file=paste(getwd(), '/output/common_species_rank.csv', sep=''), sep=',', col.names=FALSE, row.names=FALSE, append=TRUE)
      }
}

