#percentages workflow

#last time run: 30.10.2024

library(dplyr)
library(ggplot2)

#Load the datasets
df = read.csv("MAP-DATA_clean_with_Ohrid.csv", sep = ',') #load the Lang et al. 2023 data
monti = read.csv("monticchio_dataset_percentages.csv", sep = ';') #load Lago di Monticchio data 
monti[is.na(monti)] = 0 #set NA as true zeros. Here we are using raw pollen counts, if a pollen was not count in a time step we assumed that it is absent in that time step! 

monti$Source = 'NEO'
monti$Region = 'Mb'
monti$Label = 'MONTI'
monti$LatDD = 40.3564
monti$LonDD = 15.33648
monti$Elevation = 656
monti$Country = 'ITA'
monti$Name = 'Lago Grande di Monticchio'
names(monti)[3]= 'CalYrBP'


#select those taxa that has to be corrected with the correction factors (in both data frames, correction factors: Supplementary table 2)
x <- subset(df, select=c("Label","Name","LatDD","LonDD","CalYrBP", "Pollen.sum", 'Abies',
                         'Larix',"Pinus.sylvestris.t.",'Acer', 'Alnus.glutinosa.t.',
                         'Alnus.viridis','Betula','Corylus',"Ostrya.t.",'Salix','Tilia',
                         'Buxus',"Plantago.lanceolata.t." , "Cerealia.t.",'Fagus' ))
y = subset(monti,  select=c("Label","Name","LatDD","LonDD","CalYrBP", "Pollen.sum", 'Abies',
                            "Larix" , "Pinus.undiff.","Acer" ,"Alnus","Betula","Corylus",'Ostrya',
                            'Salix','Tilia','Buxus',"Plantago","Poaceae..Cerealia.type.",'Fagus'))

x$Alnus = x$Alnus.glutinosa.t. + x$Alnus.viridis #merge alunus species
x$Alnus.glutinosa.t.= NULL
x$Alnus.viridis = NULL

#correction factors
#here we calculate the difference (positive or negative) between the values corrected and not corrected. Then we add this difference to the original pollen sum.

x$Larix_corr = x$Larix*4 - x$Larix 
x$Pinus.sylvestris.t._corr = x$Pinus.sylvestris.t.*0.25 - x$Pinus.sylvestris.t. #as example, Pinus corrected must be less than the original counted (precisely 1/4). So, how much we should remove to achieve this? It is given by the difference
x$Acer_corr = x$Acer *4 - x$Acer #conversely, Acer corrected must be more than the original counted (precisely *4). So, how much we should add to achieve this? It is given by the difference
x$Alnus_corr = x$Alnus*0.25 - x$Alnus
x$Betula_corr = x$Betula *0.25 - x$Betula
x$Corylus_corr = x$Corylus*0.25 - x$Corylus
x$Ostrya.t._corr = x$Ostrya.t.*0.25 - x$Ostrya.t.
x$Salix_corr = x$Salix *4 - x$Salix
x$Tilia_corr = x$Tilia *4 - x$Tilia
x$Buxus_corr = x$Buxus*4 - x$Buxus
x$Plantago.lanceolata.t._corr= x$Plantago.lanceolata.t.*2 - x$Plantago.lanceolata.t.
x$Cerealia.t._corr = x$Cerealia.t.*4 - x$Cerealia.t.

y$Larix_corr = y$Larix*4 - y$Larix
y$Pinus.undiff._corr = y$Pinus.undiff.*0.25 - y$Pinus.undiff.
y$Acer_corr = y$Acer *4 - y$Acer
y$Alnus_corr = y$Alnus*0.25 - y$Alnus
y$Betula_corr = y$Betula*0.25 - y$Betula
y$Corylus_corr = y$Corylus*0.25 - y$Corylus
y$Ostrya_corr = y$Ostrya*0.25 - y$Ostrya
y$Salix_corr = y$Salix*4 - y$Salix
y$Tilia_corr = y$Tilia*4 - y$Tilia
y$Buxus_corr = y$Buxus*4 - y$Buxus
y$Plantago_corr = y$Plantago*2 - y$Plantago
y$Poaceae..Cerealia.type._corr = y$Poaceae..Cerealia.type.*4 - y$Poaceae..Cerealia.type.


#now we adjust the pollen sum based on pollen correction factors
y = y %>% 
  rowwise() %>% 
  mutate(Pollen.sum_adj = sum(c(Pollen.sum,Larix_corr,Pinus.undiff._corr,
                   Acer_corr, Alnus_corr, Betula_corr, Corylus_corr, 
                   Ostrya_corr, Salix_corr, Tilia_corr, Buxus_corr,
                   Plantago_corr, Poaceae..Cerealia.type._corr
                   ), na.rm=TRUE))

x = x %>% 
  rowwise() %>% 
  mutate(Pollen.sum_adj = sum(c(Pollen.sum,Larix_corr,Pinus.sylvestris.t._corr,
                                Acer_corr, Alnus_corr, Betula_corr, Corylus_corr, 
                                Ostrya.t._corr, Salix_corr, Tilia_corr, Buxus_corr,
                                Plantago.lanceolata.t._corr, Cerealia.t._corr
  ), na.rm=TRUE))

#### here you chose the species of interest. In our case 'Abies'

x = subset(x, select = c("Label","Name","LatDD","LonDD","CalYrBP", "Pollen.sum_adj", 'Abies'), na.rm = T)
y = subset(y, select = c("Label","Name","LatDD","LonDD","CalYrBP", "Pollen.sum_adj", 'Abies'), na.rm = T)

names(x)[6] = 'Pollen.sum'
names(y)[6] = 'Pollen.sum'

#we finally bind together monticchio and the dataframe 
MAP = rbind(x, y)
unique(MAP$Label)

is.list(MAP$Label)

MAP = MAP[!MAP$LatDD>60,]
MAP = MAP[!MAP$LonDD>40,]

write.csv(MAP,'old.file.2.csv')
