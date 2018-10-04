#source("D:/CRTI/r_projects/data_cleaning/merge_dupage_data.r")


# trees_dupage <- read.csv ('D:/CRTI/GIS data/Dupage_with_NDVI/csvs/trees_11000-12999/Trees_DuPage.csv', as.is = TRUE, header = TRUE, sep=',')
# polys_near_inv_dupage <- read.csv ('D:/CRTI/GIS data/Dupage_with_NDVI/csvs/trees_11000-12999/DuPage_NDVI_tree_polys_nearInvTrees.csv', as.is = TRUE, header = TRUE, sep=',')
# crowns_intersecting_dupage <- read.csv ('D:/CRTI/GIS data/Dupage_with_NDVI/csvs/trees_11000-12999/DuPage_crowns_intersecting_TreeInventory.csv', as.is = TRUE, header = TRUE, sep=',')

trees_dupage <- read.csv ('D:/CRTI/GIS data/Dupage_with_NDVI/csvs/Trees_DuPage.csv', as.is = TRUE, header = TRUE, sep=',')
polys_near_inv_dupage <- read.csv ('D:/CRTI/GIS data/Dupage_with_NDVI/csvs/DuPage_NDVI_tree_polys_nearInvTrees.csv', as.is = TRUE, header = TRUE, sep=',')
crowns_intersecting_dupage <- read.csv ('D:/CRTI/GIS data/Dupage_with_NDVI/csvs/DuPage_crowns_intersecting_TreeInventory.csv', as.is = TRUE, header = TRUE, sep=',')

df <- merge(x=crowns_intersecting_dupage, y=polys_near_inv_dupage, by.x = 'TreeNum.N.10.0', by.y = 'TreeNum')
df <- merge(x=df, y=trees_dupage, by.x = 'TreeNum.N.10.0', by.y = 'TreeID.N.10.0')

colnames(df)[colnames(df)=='TreeNum.N.10.0'] <- 'TreeNum'
colnames(df)[colnames(df)=='OBJECTID.N.10.0.x'] <- 'OBJECTID_1'
colnames(df)[colnames(df)=='DATATYPE.C.254'] <- 'DATATYPE'
colnames(df)[colnames(df)=='GENUSSPECI.C.254'] <- 'GENUSSPECI'
colnames(df)[colnames(df)=='DBH_IN.N.19.11'] <- 'DBH_IN'
colnames(df)[colnames(df)=='FC_cnsd2.N.10.0'] <- 'FC_cnsd2'
colnames(df)[colnames(df)=='Compactnes.N.19.16'] <- 'Compactnes'
colnames(df)[colnames(df)=='nDSM_max.N.19.16'] <- 'nDSM_max'
colnames(df)[colnames(df)=='nDSM_mean.N.19.16'] <- 'nDSM_mean'
colnames(df)[colnames(df)=='Area.N.19.16'] <- 'Area'
colnames(df)[colnames(df)=='B25035e1.N.10.0'] <- 'MEDAGE'
colnames(df)[colnames(df)=='HU_DENS.N.13.11'] <- 'HousingDen'
colnames(df)[colnames(df)=='RelBord_tr.N.19.16'] <- 'RelBord_tr'
colnames(df)[colnames(df)=='MAX'] <- 'MAX'
colnames(df)[colnames(df)=='MEAN'] <- 'MEAN'
colnames(df)[colnames(df)=='STD'] <- 'STD'

#write.csv(df, file='D:/CRTI/GIS data/Dupage_with_NDVI/csvs/trees_11000-12999/joined.csv', row.names=FALSE)
write.csv(df, file='D:/CRTI/GIS data/Dupage_with_NDVI/csvs/joined.csv', row.names=FALSE)

