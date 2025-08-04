#now we format all the files
setwd('Additional_sites/CLEAN_SITES/FORMATTED')

metadata = read.csv("site.metadata.csv")

filenames <- list.files(pattern="*.xlsx", full.names=TRUE)

library(readxl)
library(dplyr)
store_data = list()
store.names = list()
for(i in 1:length(filenames)){
  print(i)
  name = sub("_form.xlsx", "", filenames[i])
  name = sub("./", "", name)
  load.metadata = metadata[metadata$filename == name,]
  load.metadata = load.metadata[!is.na(load.metadata$SiteN),]
  x = read_xlsx(filenames[i])
  names(x)[1] = 'CalYrBP'
  store.names[[i]] = names(x)
  x$Pollen_sum = rowSums(x[,2:ncol(x)], na.rm = T)
  
  #Correction factors
  x[,which(names(x) %in% c("Larix", "Larix decidua"))] = x[,which(names(x) %in% c("Larix", "Larix decidua"))]*4
  x[,which(names(x) %in% c("Pinus sylvestris","Pinus sylvestris-type","Pinus","Pinus nigra-type",
                           "Pinus pinaster","Pinus subg. Pinus" , "Pinus subg. Pinus-type"))] = x[,which(names(x) %in% c("Pinus sylvestris","Pinus sylvestris-type","Pinus","Pinus nigra-type",
                                                                                                                         "Pinus pinaster","Pinus subg. Pinus" , "Pinus subg. Pinus-type"))]*0.25
  x[,which(names(x) %in% c("Acer","Acer campestre,Acer campestre-type","Acer negundo"))] = x[,which(names(x) %in% c("Acer","Acer campestre,Acer campestre-type","Acer negundo"))]*4
  x[,which(names(x) %in% c("Alnus","Alnus glutinosa",                                 
                           "Alnus glutinosa-type","Alnus incana",                                    
                           "Alnus subg. Alnobetula","Alnus viridis",                                   
                           "Alnus vitis"))] = x[,which(names(x) %in% c("Alnus","Alnus glutinosa",                                 
                                                                       "Alnus glutinosa-type","Alnus incana",                                    
                                                                       "Alnus subg. Alnobetula","Alnus viridis",                                   
                                                                       "Alnus vitis"))]*0.25
  x[,which(names(x) %in% c("Betula","Betula humilis","Betula nana","Betula pubescens","Betula sect. Betula","Betula/Corylus/Myrica"))] = x[,which(names(x) %in% c("Betula","Betula humilis","Betula nana","Betula pubescens","Betula sect. Betula","Betula/Corylus/Myrica"))]*0.25
  x[,which(names(x) %in% c("Corylus","Corylus avellana","Corylus avellana-type","Corylus/Myrica"))] = x[,which(names(x) %in% c("Corylus","Corylus avellana","Corylus avellana-type","Corylus/Myrica"))]*0.25
  x[,which(names(x) %in% c("Ostrya","Ostrya-type" ,"Ostrya carpinifolia","Ostrya carpinifolia/Carpinus orientalis","Ostrya/Carpinus","Ostrya/Carpinus orientalis","Ostrya/Carpinus orientalis-type"))] = x[,which(names(x) %in% c("Ostrya","Ostrya-type" ,"Ostrya carpinifolia","Ostrya carpinifolia/Carpinus orientalis","Ostrya/Carpinus","Ostrya/Carpinus orientalis","Ostrya/Carpinus orientalis-type"))]*0.25
  x[,which(names(x) %in% c("Salix","Salix herbacea-type","Salix polaris-type","Salix undiff."))] = x[,which(names(x) %in% c("Salix","Salix herbacea-type","Salix polaris-type","Salix undiff."))]*4
  x[,which(names(x) %in% c("Tilia","Tilia cordata","Tilia cordata-type","Tilia platyphyllos-type"))] = x[,which(names(x) %in% c("Tilia","Tilia cordata","Tilia cordata-type","Tilia platyphyllos-type"))]*4
  x[,which(names(x) %in% c("Buxus","Buxus sempervirens"))] = x[,which(names(x) %in% c("Buxus","Buxus sempervirens"))]*4
  x[,which(names(x) %in% c("Plantago lanceolata","Plantago lanceolata-type" ))] = x[,which(names(x) %in% c("Plantago lanceolata","Plantago lanceolata-type" ))]*2
  x[,which(names(x) %in% c("Cerealia-type", "Poaceae (Cerealia-type)","Poaceae (Cerealia)", "Poaceae (Cerealia) undiff.",
                           "Poaceae (>37 Âµm)", "Poaceae (annulus >10 Âµm)"))] = x[,which(names(x) %in% c("Cerealia-type", "Poaceae (Cerealia-type)","Poaceae (Cerealia)", "Poaceae (Cerealia) undiff.",
                                                                                                          "Poaceae (>37 Âµm)", "Poaceae (annulus >10 Âµm)"))]*4
  
  x$Pollen_sum.2 = rowSums(x[,2:(ncol(x)-1)], na.rm = T)
  x$LonDD = load.metadata$londd
  x$LatDD = load.metadata$latdd
  x$Region = load.metadata$Region
  x$Country = load.metadata$Country
  x$Name = load.metadata$SiteName
  x$Label = load.metadata$filename
  if(sum(names(x) %in% c('Abies')) != 0){
    new.data = x %>% select(Label, Name, Country, Region, LonDD, LatDD, CalYrBP,Pollen_sum, Pollen_sum.2, Abies)
    new.data$Abies[is.na(new.data$Abies)] = 0
  } else if (sum(names(x) %in% c('Abies alba')) != 0){
    names(x)[which(names(x) %in% c('Abies alba'))] = 'Abies'
    new.data = x %>% select(Label, Name, Country, Region, LonDD, LatDD, CalYrBP,Pollen_sum, Pollen_sum.2, Abies)
    new.data$Abies[is.na(new.data$Abies)] = 0
  } else{
    x$Abies = 0
    new.data = x %>% select(Label, Name, Country, Region, LonDD, LatDD, CalYrBP,Pollen_sum,Pollen_sum.2,Abies)
  }
  #write.csv(new.data, file = paste0('To_merge/Name.',name,'.ready.csv'))
  store_data[[i]] = new.data
}

names = unlist(store.names)
names = unique(names)
names = sort(names)

all.together = do.call(rbind, store_data)
all.together[3948,]

rimuovi_pattern <- function(x) {
  x <- gsub("^--/|/--$", "", x)
  x <- gsub("^--|--$", "", x)
  x <- gsub("^[^/]+/|/[^/]+$", "", x)
  return(x)
}

all.together$CalYrBP.2 <- sapply(all.together$CalYrBP, rimuovi_pattern)
all.together[114,]


write.csv(all.together, 'additiona.sites.formatted.csv')
