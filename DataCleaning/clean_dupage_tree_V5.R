# source("D:/CRTI/r_projects/Modeling/DataCleaning/clean_dupage_tree_V5.r")
# clean()

# Read the CSV with the tree data 
chicago_tree_data = read.csv ('D:/CRTI/data/Trees_DuPage_with_NDVI_and_crown.csv', as.is = TRUE, header = TRUE, sep=',')

# Define column name mapping
column_names = c (
      'OBJECTID', 'OBJECTID',
      'DATATYPE',   'DATATYPE', 
#      'STREETTREE', 'STREETTREE', 
      'GENUSSPECI', 'GENUSSPECI', 
      'DBH_IN',     'DBH_IN', 
      'FC_cnsd2',    'LU', 
      'Compactnes', 'CROWN_COMPACTNESS',
      'nDSM_max',   'HEIGHT_MAX',
      'nDSM_mean',  'HEIGHT_MEAN',
      'Area',       'CROWN_AREA',
      'B25035e1',   'BLDG_YEAR',
      'HU_DENS',    'HU_DENS',
      'RelBord_tr', 'RELBORDER_TREE',
      'NDVI_MAX',   'NDVI_MAX',
      'NDVI_MEAN',  'NDVI_MEAN',
      'NDVI_STD',   'NDVI_STD'
      ) 
column_names<-matrix(column_names, ncol=2, byrow=TRUE)

clean <- function () 
{

      
      # Get the column names in the source dataset for land use and stree treee      
      lu_name_col = column_names[column_names[,2]=='LU'][1]
#      streettree_col = column_names[column_names[,2]=='STREETTREE'][1]
      streettree_col = 'STREETTREE'
      # Fixup land use numbers to match the land_use.csv file (add one to them)
#      chicago_tree_data[[lu_name_col]] <- chicago_tree_data[[lu_name_col]] + 1
      
      # Coerce all "Street trees=Y" to Land Use #13 (Stree tree)
      chicago_tree_data[lu_name_col][chicago_tree_data[streettree_col]=='Y'] <- 13
      
      # Filter out records where the tree point data is beyond the acceptable distance from the polygon data (15 feet)
#      chicago_tree_data <- chicago_tree_data[chicago_tree_data$Dist_2Join!=-1,]

      # Filter out records where DATATYPE is not "Point"
      chicago_tree_data <- chicago_tree_data[chicago_tree_data$DATATYPE=='Point',]
      
      # Remove unused rows and rename the remaining rows
      for (c in column_names[,1])
      {
            if (!(c %in% colnames(chicago_tree_data))) 
            {
                  print(paste("Column not found in source data frame:", c))
            }
      }
      
      ctree = chicago_tree_data[,column_names[,1]]
      colnames(ctree) = column_names[,2]
      
      # Define the field limits
      limits            <-  matrix(ncol=2, nrow=0)
      colnames(limits)  <- c("min", "max")
      CROWN_AREA        <- c(0, 10000)
      CROWN_COMPACTNESS <- c(0, 10)
      DBH_IN            <- c(0, 70)
      HEIGHT_MAX        <- c(0, 150)
      HEIGHT_MEAN       <- c(0, 100)
      NDVI_MEAN         <- c(0,1)
      NDVI_MAX          <- c(0,1)
      limits            <- rbind (limits, CROWN_AREA, CROWN_COMPACTNESS, DBH_IN, HEIGHT_MAX, HEIGHT_MEAN, NDVI_MEAN, NDVI_MAX)
      
      replace_zeros_with_mean = FALSE
      replace_zeros_with_na = TRUE

      # Replace BLDG_YEAR with BLDG_AGE
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      df <- data.frame(rep(0,nrow(ctree)))
      names(df) <- 'BLDG_AGE'
      ctree <- cbind (ctree, df)
      ctree$BLDG_AGE <- current_year - ctree$BLDG_YEAR
      ctree$BLDG_YEAR <- NULL
      
      # Throw out any records with an unknown land use
      ctree <- ctree[(ctree$LU %in% read.delim('https://don-morrison-2000.github.io/data/land_use.csv', as.is=TRUE, header=FALSE)$V1),]
      
      # Add columns to record whether a record was altered in the cleaing process
      for (v in c('CROWN_AREA', 'CROWN_COMPACTNESS', 'DBH_IN', 'HEIGHT_MAX', 'HEIGHT_MEAN', 'NDVI_MEAN', 'NDVI_MAX'))
      {
            is_cleaned_label = paste('IS_CLEANED', '_', v, sep='')
            df <- data.frame(rep(0,nrow(ctree)))
            names(df) <- is_cleaned_label
            ctree <- cbind (ctree, df)
      }            
      
      
      # Create an empty data frame to record purged records (same format at ctree)
      rejects <- ctree[0,]
      
      # # Get rid of any record with NA in any column
      # for (n in names(ctree))
      # {
      #       is_na <- is.na(ctree[n])
      #       # Record the records that we are going to purge
      #       rejects <- rbind (rejects, ctree[is_na,])
      #       # Purge the records
      #       ctree <- ctree[!is_na,]
      # }

      # Process each variable that needs cleaning
      for (v in c('CROWN_AREA', 'CROWN_COMPACTNESS', 'DBH_IN', 'HEIGHT_MAX', 'HEIGHT_MEAN', 'NDVI_MEAN', 'NDVI_MAX'))
      {
            is_cleaned_label <- paste('IS_CLEANED', '_', v, sep='')
            
            if (nrow(ctree[v]) > 0)
            {
                  # Find records with values outside the limits
                  limit_exceded <- (ctree[[v]] < limits[v,"min"]) | (ctree[[v]] > limits[v,"max"])
                  ctree[limit_exceded, is_cleaned_label] = 1
                  # Record the records that we are going to purge
                  rejects <- rbind (rejects, ctree[limit_exceded,])
                  # Purge the records
                  ctree <- ctree[!limit_exceded,]
      
                  # Attempt to replace zero values with the mean value (by species) TODO: Should we add land use?
                  for(spp in unique(ctree$GENUSSPECI)) 
                  {
                        # Create logical vectors to identify the dirty and clean records
                        clean <- (ctree[[v]] != 0) & (ctree[['GENUSSPECI']] == spp)
                        dirty <- (ctree[[v]] == 0) & (ctree[['GENUSSPECI']] == spp)
                        # Check if we should try to recover the zeros by substituting in the mean
                        if (replace_zeros_with_mean)
                        {
                              # Record that we are going try to clean the dirty records
                              ctree[dirty, is_cleaned_label] = 1                              
                              # Get the mean value for all clean (non-zero) records
                              v_mean <- mean(ctree[clean,v])
                              # Could we compute a mean of the clean records?
                              if (!is.nan(v_mean))
                              {
                                    # Yes, update the zero records with that mean TODO: should we do something to make sure the mean is from a decent sample (eg more then one record?)
                                    ctree[dirty,v] <- v_mean
                              }
                              else
                              {
                                    # No. A mean could not be calculated  from the clean records so we could not fix up the zero records. Purge the records
                                    rejects <- rbind (rejects, ctree[dirty,])
                                    ctree <- ctree[!dirty,]                              
                              }
                        }
                        else if (replace_zeros_with_na)
                        {
                              # Simply update the zero records to make them NAs
                              ctree[dirty,v] <- NA
                        }
                        else
                        {
                              # Do not fix up the zeros. Purge them
                              rejects <- rbind (rejects, ctree[dirty,])
                              ctree <- ctree[!dirty,]    
                        }
                  }
            }
      }
      
      write.csv(ctree, file='D:/CRTI/data/cleaned/dupage_county_accepted_V5.csv', row.names=FALSE)
      write.csv(rejects, file='D:/CRTI/data/cleaned/dupage_county_rejected_V5.csv', row.names=FALSE)
}
