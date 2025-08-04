#remove sites and adjust the data
library(dplyr)
df = read.csv(file = 'MAP_Abies_percentage_el_add_sites.csv', header  = T)
years = seq(500,20000, by=500)
sort(unique(df$Label))


table(is.na(df$Abies_perc))
df$X = NULL
df$Pollen.sum = NULL

df = df[!is.na(df$Abies_perc),]

trifo = df[df$Label %in% c('TRIFO1','TRIFO2'),]

#merge multiple cores of the same site

df$Label[df$Label %in% c('DURA2', 'DURA1')] = 'DURA'
df$Label[df$Label %in% c('NE274', 'NER2P')] = 'NERP'
df$Label[df$Label %in% c('TARN1', 'TARN2')] = 'TARN'
df$Label[df$Label %in% c('NISIB', 'NISIE')] = 'NISI'
df$Label[df$Label %in% c('TRIFO1', 'TRIFO2')] = 'TRIFO'
df$Label[df$Label %in% c('ABER2', 'ABER1')] = 'ABER'
df$Label[df$Label %in% c('STRA1', 'STRAC')] = 'STRA'

trifo2 = df[df$Label %in% c('TRIFO'),]
df$Name = NULL

df = aggregate(. ~ Label + CalYrBP + LonDD + LatDD, data = df, FUN = mean)
trifo3 = df[df$Label =='TRIFO',]

write.table(df, file = 'df_ready_to_krige_vfinal_corr_fact_add_sites.txt', col.names = T, row.names = F)
