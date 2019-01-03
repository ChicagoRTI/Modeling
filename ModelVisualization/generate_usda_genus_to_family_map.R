# source(paste(getwd(), '/generate_usda_genus_to_family_map.R', sep=''))
# generate_usda_genus_to_family_map()


# Downloads the usda plant list, extracts out the genera and families and removes duplicates. The result
# is a list of all genera and the associated family. The result is written out as a csv file, which can be 
# uploaded to its home Git location: https://don-morrison-2000.github.io/data/usda_genus_to_family_map.csv'
generate_usda_genus_to_family_map <- function(outfile=paste(getwd(), '/data/usda_genus_to_family_map.csv', sep=''))
{
      u <-read.delim('https://plants.usda.gov/java/downloadData?fileName=plantlst.txt&static=true', as.is=TRUE, header=TRUE, sep=",")[,c(3,5)]
      colnames(u) <- c('genus', 'family')
      # Get just the 2 relevant columns from the usda data (genus and family)
      u$genus <- sapply(strsplit(u$genus," "),"[",1)
      # Remove rows where the family name is empty
      u <- u[u$family != '',]
      row.names(u) <- 1:nrow(u)
      # Select the first occurence of each one row for each genus to family mapping
      u <- u[match(sort(unique(u$genus)), u$genus),]
      # Write to csv file
      cat("Writing result to", outfile, sep=" ")
      write.table(u, file=outfile, sep=',', row.names=FALSE, col.names=TRUE)
}


