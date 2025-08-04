library(tidyverse)
library(dplyr)
library(sf)
#script to merge all data together. It may require up to 24 G of RAM. Please run precipitation, temperature min and max individually. 
#precipitations---------------------------------
df = read.csv("df_ready_to_krige_vfinal_corr_fact_add_sites.txt", header  = T, sep = ' ')
df$abies_category = NULL
years = seq(500,20000, by=500)

#load data for these points
files_bio = list.files(path = 'Covariates/Covariates.percentages/', recursive = T, pattern = '?pr.txt')
files_bio = files_bio[1:12]
bio01 = read.table(paste0('Covariates/Covariates.percentages/',files_bio[1]))

points_df = unique(df[,c(3,4)])
points_bio = unique(bio01[,c(2,3)])

big_df = data.frame()

for (i in 1:length(files_bio)){
  bio01 = read.table(paste0('Covariates/Covariates.percentages/',files_bio[i]))
  print(ncol(bio01))
  big_df= rbind(big_df, bio01)
}

names(big_df) = c('value','lon','lat','file')
big_df$file <- basename(big_df$file )


big_df$variable =  substr(big_df$file,start =17, stop = 18 )
big_df$month = substr(big_df$file,start =20, stop = 21 )
big_df$year =  substr(big_df$file,start =22, stop = 26 )
big_df$year = gsub("_", "", big_df$year )
big_df$year = gsub("V1.", "", big_df$year )
big_df$year = gsub("V1", "", big_df$year )
big_df$year = gsub("V", "", big_df$year )
big_df$year = gsub("\\.", "-", big_df$year )
big_df$month = gsub("_", "", big_df$month )

#big_df = big_df[,-4]
unique(big_df$year); unique(big_df$variable); unique(big_df$month)


big_df = big_df %>% mutate(year = case_when(
  year=="20" ~ 0,  year=="19" ~ 100,  year=="18" ~ 200,   year== "17" ~300,  year=="16" ~400,  year== "15" ~500,  year== "14" ~600,  year== "13" ~700,
  year== "12" ~800,  year=="11" ~900,  year== "10" ~1000,  year== "9"~1100,  year== "8"~1200,  year=="7" ~1300,  year=="6" ~1400,  year=="5" ~1500,
  year== "4" ~1600,  year== "3" ~1700,  year== "2"  ~1800,  year== "1" ~1900,  year== "0" ~2000,  year=="-1" ~2100,  year=="-2" ~2200,  year== "-3"~2300,
  year=="-4"~2400,  year== "-5"~2500,  year=="-6"~2600,  year== "-7"~2700,  year== "-8"~2800,  year== "-9"~2900,  year== "-10" ~3000,  year== "-11"~3100,
  year== "-12" ~3200,  year== "-13" ~3300,  year== "-14"~3400,  year==  "-15"~ 3500,year==  "-16" ~3600,  year== "-17" ~3700,  year==  "-18"~3800,  year== "-19" ~3900,
  year==  "-20" ~4000,  year==  "-21"~4100,   year== "-22" ~4200 ,   year==  "-23" ~4300 ,  year==  "-24" ~4400 ,  year=="-25"~ 4500  ,  year==  "-26"~4600  ,
  year==  "-27" ~4700  ,  year== "-28" ~4800  ,  year== "-29"~4900 ,  year==  "-30"~ 5000,   year==  "-31" ~ 5100,   year==  "-32"~ 5200,  year==  "-33" ~  5300,  
  year==  "-34" ~5400  ,  year== "-35" ~5500 ,  year==  "-36" ~ 5600,   year==  "-37" ~ 5700 ,  year==  "-38"~ 5800  ,  year== "-39"~5900  ,  year== "-40" ~6000 ,
  year==  "-41" ~ 6100,    year==  "-42" ~6200,  year==  "-43" ~6300 ,  year==  "-44"~ 6400 ,  year==  "-45" ~ 6500 ,   year==  "-46"~6600  ,  year==  "-47" ~6700  ,
  year==  "-48" ~6800  ,  year==  "-49" ~6900 ,  year==  "-50" ~ 7000 ,  year==  "-51" ~ 7100  ,  year==  "-52"~7200 ,  year==  "-53" ~ 7300,    year==  "-54"~7400  ,
  year== "-55"~7500  ,  year==  "-56"~ 7600 ,  year== "-57" ~7700 ,  year==  "-58" ~ 7800,   year== "-59"~ 7900 ,  year== "-60" ~ 8000 ,   year==  "-61"~8100  ,
  year==  "-62"~8200  ,  year==  "-63" ~8300  ,  year==  "-64" ~8400  ,  year==  "-65" ~8500,  year== "-66" ~8600 ,  year== "-67" ~ 8700 ,   year==  "-68" ~8800  ,
  year==  "-69" ~8900  ,  year==  "-70"~9000  ,  year== "-71" ~9100 ,  year== "-72" ~ 9200 ,   year==  "-73" ~9300  ,  year== "-74" ~9400  ,  year== "-75" ~9500  ,
  year== "-76" ~9600  ,  year==  "-77" ~9700  ,  year== "-78" ~9800  ,  year== "-79" ~9900 ,  year== "-80"~10000 ,  year==  "-81"~10100 ,  year==  "-82" ~10200 ,
  year==  "-83"~10300 ,  year==  "-84" ~10400 ,  year==  "-85" ~10500 ,  year==  "-86" ~10600 ,  year==  "-87" ~10700 ,  year==  "-88" ~10800,  year==  "-89"~10900 ,  year==  "-90" ~11000 ,
  year==  "-91" ~11100 ,  year==  "-92" ~11200 ,  year==  "-93" ~11300,  year==  "-94" ~ 11400,   year== "-95"~11500 ,  year== "-96" ~11600 ,  year==  "-97" ~11700,
  year== "-98" ~ 11800 ,  year=="-99"~11900 ,  year=="-100" ~12000 ,  year==  "-101"~12100 ,  year==  "-102"~ 12200,  year==  "-103" ~12300,  year==  "-104" ~12400 ,  year==  "-105" ~12500 ,
  year==  "-106" ~12600 ,  year==  "-107" ~12700 ,  year==  "-108" ~12800,  year==  "-109"  ~12900,   year==  "-110" ~13000 ,  year==  "-111" ~ 13100,  year==  "-112" ~13200 ,
  year== "-113" ~13300 ,  year==  "-114" ~13400 ,  year== "-115" ~13500 ,  year== "-116"~13600,  year== "-117" ~13700,   year==  "-118" ~ 13800,  year== "-119" ~13900,
  year== "-120" ~14000 ,  year== "-121" ~ 14100 ,  year== "-122" ~ 14200 ,  year== "-123" ~14300 ,  year==  "-124" ~14400 ,  year==  "-125" ~14500,  year==  "-126" ~14600 ,
  year==  "-127" ~ 14700 ,  year==  "-128" ~14800 ,  year==  "-129" ~14900,  year==  "-130" ~15000 ,  year==  "-131" ~ 15100 ,  year==  "-132" ~15200 ,  year==  "-133" ~15300 ,
  year==  "-134"~15400,  year==  "-135" ~15500,   year==  "-136" ~15600 ,  year==  "-137" ~15700 ,  year==  "-138" ~15800 ,  year== "-139" ~15900 ,  year==  "-140" ~16000 ,
  year== "-141" ~16100 ,  year== "-142" ~16200,  year== "-143" ~16300,  year== "-144"~ 16400 ,  year=="-145"~16500 ,  year== "-146"~ 16600,   year== "-147" ~16700 ,
  year==  "-148" ~16800 ,  year==  "-149" ~16900 ,  year==  "-150" ~17000 ,  year== "-151" ~17100 ,  year== "-152"~17200 ,  year==  "-153"~ 17300,   year== "-154" ~17400 ,
  year==  "-155" ~17500 ,  year== "-156" ~17600 ,  year== "-157" ~17700,  year== "-158" ~17800 ,  year== "-159" ~17900 ,  year==  "-160" ~18000 ,  year=="-161" ~18100 ,
  year== "-162" ~18200,  year=="-163" ~ 18300 ,  year=="-164" ~18400 ,  year== "-165" ~18500 ,  year==  "-166" ~18600,  year==  "-167" ~ 18700,  year==  "-168" ~ 18800,
  year==  "-169" ~ 18900,  year==  "-170"~ 19000 ,  year== "-171" ~19100 ,  year== "-172" ~19200 ,  year== "-173" ~19300 ,  year== "-174"~19400 ,  year=="-175"~19500,  year=="-176" ~ 19600,   year=="-177" ~19700,
  year=="-178"~ 19800 ,  year=="-179" ~19900 ,  year=="-180" ~20000, year=="-181" ~20100, year=="-182" ~20200,  year=="-183" ~20300,  year=="-184" ~20400,  year=="-185" ~20500,  year=="-186" ~20600,  year=="-187" ~20700,
  year=="-188" ~20800,  year=="-189" ~20900,year=="-190" ~21000,  year=="-191" ~21100,  year=="-192" ~21200,  year=="-193" ~21300,  year=="-194" ~21400,  year=="-195" ~21500,  year=="-196" ~21600,   year=="-197" ~21700,  year=="-198" ~21800,  year=="-199" ~21900,  year=="-200" ~22000
))

unique(big_df$year)
which_na = big_df[is.na(big_df$year),] #check if something went wrong, if it is ok, it is empty!

big_df$grid_cell = rep(seq(1, 7320, by = 1),12*221)

#add label name for each point 
names(df)[c(4,3)] = c('lat','lon')
head(big_df)

names(df); names(big_df)

write.table(big_df, 'Covariates/Covariates.percentages/monthly/pr_total_corr_fact_add_sites.txt')

#temperatures ---------------------------------------------------------------
library(tidyverse)

df = read.csv("df_ready_to_krige_vfinal_corr_fact_add_sites.txt", header  = T, sep = ' ')
df$abies_category = NULL
years = seq(500,20000, by=500)

#load data for these points
files_bio = list.files(path = 'Covariates/Covariates.percentages/', recursive = T, pattern = '?tasmax.txt')
files_bio = files_bio[1:12]
bio01 = read.table(paste0('Covariates/Covariates.percentages/',files_bio[1]))

points_df = unique(df[,c(3,4)])
points_bio = unique(bio01[,c(2,3)])

#big_df = data.frame() #if your dataset is so big that takes forever, consider using list indead of data.frame.
big_df = list()

for (i in 1:length(files_bio)){
  bio01 = read.table(paste0('Covariates/Covariates.percentages/',files_bio[i]))
  #big_df= rbind(big_df, bio01)
  big_df[[i]]= bio01
}

big_df = do.call(rbind, big_df)
names(big_df) = c('value','lon','lat','file')
big_df$file <- basename(big_df$file )


big_df$variable =  substr(big_df$file,start =17, stop = 22 )
big_df$month = substr(big_df$file,start =24, stop = 25 )
big_df$year =  substr(big_df$file,start =26, stop = 30 )
big_df$year = gsub("_", "", big_df$year )
big_df$year = gsub("V1.", "", big_df$year )
big_df$year = gsub("V1", "", big_df$year )
big_df$year = gsub("V", "", big_df$year )
big_df$year = gsub("\\.", "-", big_df$year )
big_df$month = gsub("_", "", big_df$month )

#big_df = big_df[,-4]
unique(big_df$year); unique(big_df$variable); unique(big_df$month)


big_df = big_df %>% mutate(year = case_when(
  year=="20" ~ 0,  year=="19" ~ 100,  year=="18" ~ 200,   year== "17" ~300,  year=="16" ~400,  year== "15" ~500,  year== "14" ~600,  year== "13" ~700,
  year== "12" ~800,  year=="11" ~900,  year== "10" ~1000,  year== "9"~1100,  year== "8"~1200,  year=="7" ~1300,  year=="6" ~1400,  year=="5" ~1500,
  year== "4" ~1600,  year== "3" ~1700,  year== "2"  ~1800,  year== "1" ~1900,  year== "0" ~2000,  year=="-1" ~2100,  year=="-2" ~2200,  year== "-3"~2300,
  year=="-4"~2400,  year== "-5"~2500,  year=="-6"~2600,  year== "-7"~2700,  year== "-8"~2800,  year== "-9"~2900,  year== "-10" ~3000,  year== "-11"~3100,
  year== "-12" ~3200,  year== "-13" ~3300,  year== "-14"~3400,  year==  "-15"~ 3500,year==  "-16" ~3600,  year== "-17" ~3700,  year==  "-18"~3800,  year== "-19" ~3900,
  year==  "-20" ~4000,  year==  "-21"~4100,   year== "-22" ~4200 ,   year==  "-23" ~4300 ,  year==  "-24" ~4400 ,  year=="-25"~ 4500  ,  year==  "-26"~4600  ,
  year==  "-27" ~4700  ,  year== "-28" ~4800  ,  year== "-29"~4900 ,  year==  "-30"~ 5000,   year==  "-31" ~ 5100,   year==  "-32"~ 5200,  year==  "-33" ~  5300,  
  year==  "-34" ~5400  ,  year== "-35" ~5500 ,  year==  "-36" ~ 5600,   year==  "-37" ~ 5700 ,  year==  "-38"~ 5800  ,  year== "-39"~5900  ,  year== "-40" ~6000 ,
  year==  "-41" ~ 6100,    year==  "-42" ~6200,  year==  "-43" ~6300 ,  year==  "-44"~ 6400 ,  year==  "-45" ~ 6500 ,   year==  "-46"~6600  ,  year==  "-47" ~6700  ,
  year==  "-48" ~6800  ,  year==  "-49" ~6900 ,  year==  "-50" ~ 7000 ,  year==  "-51" ~ 7100  ,  year==  "-52"~7200 ,  year==  "-53" ~ 7300,    year==  "-54"~7400  ,
  year== "-55"~7500  ,  year==  "-56"~ 7600 ,  year== "-57" ~7700 ,  year==  "-58" ~ 7800,   year== "-59"~ 7900 ,  year== "-60" ~ 8000 ,   year==  "-61"~8100  ,
  year==  "-62"~8200  ,  year==  "-63" ~8300  ,  year==  "-64" ~8400  ,  year==  "-65" ~8500,  year== "-66" ~8600 ,  year== "-67" ~ 8700 ,   year==  "-68" ~8800  ,
  year==  "-69" ~8900  ,  year==  "-70"~9000  ,  year== "-71" ~9100 ,  year== "-72" ~ 9200 ,   year==  "-73" ~9300  ,  year== "-74" ~9400  ,  year== "-75" ~9500  ,
  year== "-76" ~9600  ,  year==  "-77" ~9700  ,  year== "-78" ~9800  ,  year== "-79" ~9900 ,  year== "-80"~10000 ,  year==  "-81"~10100 ,  year==  "-82" ~10200 ,
  year==  "-83"~10300 ,  year==  "-84" ~10400 ,  year==  "-85" ~10500 ,  year==  "-86" ~10600 ,  year==  "-87" ~10700 ,  year==  "-88" ~10800,  year==  "-89"~10900 ,  year==  "-90" ~11000 ,
  year==  "-91" ~11100 ,  year==  "-92" ~11200 ,  year==  "-93" ~11300,  year==  "-94" ~ 11400,   year== "-95"~11500 ,  year== "-96" ~11600 ,  year==  "-97" ~11700,
  year== "-98" ~ 11800 ,  year=="-99"~11900 ,  year=="-100" ~12000 ,  year==  "-101"~12100 ,  year==  "-102"~ 12200,  year==  "-103" ~12300,  year==  "-104" ~12400 ,  year==  "-105" ~12500 ,
  year==  "-106" ~12600 ,  year==  "-107" ~12700 ,  year==  "-108" ~12800,  year==  "-109"  ~12900,   year==  "-110" ~13000 ,  year==  "-111" ~ 13100,  year==  "-112" ~13200 ,
  year== "-113" ~13300 ,  year==  "-114" ~13400 ,  year== "-115" ~13500 ,  year== "-116"~13600,  year== "-117" ~13700,   year==  "-118" ~ 13800,  year== "-119" ~13900,
  year== "-120" ~14000 ,  year== "-121" ~ 14100 ,  year== "-122" ~ 14200 ,  year== "-123" ~14300 ,  year==  "-124" ~14400 ,  year==  "-125" ~14500,  year==  "-126" ~14600 ,
  year==  "-127" ~ 14700 ,  year==  "-128" ~14800 ,  year==  "-129" ~14900,  year==  "-130" ~15000 ,  year==  "-131" ~ 15100 ,  year==  "-132" ~15200 ,  year==  "-133" ~15300 ,
  year==  "-134"~15400,  year==  "-135" ~15500,   year==  "-136" ~15600 ,  year==  "-137" ~15700 ,  year==  "-138" ~15800 ,  year== "-139" ~15900 ,  year==  "-140" ~16000 ,
  year== "-141" ~16100 ,  year== "-142" ~16200,  year== "-143" ~16300,  year== "-144"~ 16400 ,  year=="-145"~16500 ,  year== "-146"~ 16600,   year== "-147" ~16700 ,
  year==  "-148" ~16800 ,  year==  "-149" ~16900 ,  year==  "-150" ~17000 ,  year== "-151" ~17100 ,  year== "-152"~17200 ,  year==  "-153"~ 17300,   year== "-154" ~17400 ,
  year==  "-155" ~17500 ,  year== "-156" ~17600 ,  year== "-157" ~17700,  year== "-158" ~17800 ,  year== "-159" ~17900 ,  year==  "-160" ~18000 ,  year=="-161" ~18100 ,
  year== "-162" ~18200,  year=="-163" ~ 18300 ,  year=="-164" ~18400 ,  year== "-165" ~18500 ,  year==  "-166" ~18600,  year==  "-167" ~ 18700,  year==  "-168" ~ 18800,
  year==  "-169" ~ 18900,  year==  "-170"~ 19000 ,  year== "-171" ~19100 ,  year== "-172" ~19200 ,  year== "-173" ~19300 ,  year== "-174"~19400 ,  year=="-175"~19500,  year=="-176" ~ 19600,   year=="-177" ~19700,
  year=="-178"~ 19800 ,  year=="-179" ~19900 ,  year=="-180" ~20000, year=="-181" ~20100, year=="-182" ~20200,  year=="-183" ~20300,  year=="-184" ~20400,  year=="-185" ~20500,  year=="-186" ~20600,  year=="-187" ~20700,
  year=="-188" ~20800,  year=="-189" ~20900,year=="-190" ~21000,  year=="-191" ~21100,  year=="-192" ~21200,  year=="-193" ~21300,  year=="-194" ~21400,  year=="-195" ~21500,  year=="-196" ~21600,   year=="-197" ~21700,  year=="-198" ~21800,  year=="-199" ~21900,  year=="-200" ~22000
))

unique(big_df$year)
which_na = big_df[is.na(big_df$year),] #check if something went wrong, if it is ok, it is empty!

big_df$grid_cell = rep(seq(1, 7320, by = 1),12*221)

#add label name for each point 
names(df)[c(4,3)] = c('lat','lon')
head(big_df)

names(df); names(big_df)

write.table(big_df, 'Covariates/Covariates.percentages/monthly/tasmax_total_corr_fact_add_sites.txt')

#temperatures ---------------------------------------------------------------
library(tidyverse)

df = read.csv("df_ready_to_krige_vfinal_corr_fact_add_sites.txt", header  = T, sep = ' ')
df$abies_category = NULL
years = seq(500,20000, by=500)

#load data for these points
files_bio = list.files(path = 'Covariates/Covariates.percentages/', recursive = T, pattern = '?tasmin.txt')
files_bio = files_bio[1:12]
bio01 = read.table(paste0('Covariates/Covariates.percentages/',files_bio[1]))

points_df = unique(df[,c(3,4)])
points_bio = unique(bio01[,c(2,3)])

#big_df = data.frame() #if your dataset is so big that takes forever, consider using list indead of data.frame.
big_df = list()

for (i in 1:length(files_bio)){
  bio01 = read.table(paste0('Covariates/Covariates.percentages/',files_bio[i]))
  #big_df= rbind(big_df, bio01)
  big_df[[i]]= bio01
}

big_df = do.call(rbind, big_df)
names(big_df) = c('value','lon','lat','file')
big_df$file <- basename(big_df$file )


big_df$variable =  substr(big_df$file,start =17, stop = 22 )
big_df$month = substr(big_df$file,start =24, stop = 25 )
big_df$year =  substr(big_df$file,start =26, stop = 30 )
big_df$year = gsub("_", "", big_df$year )
big_df$year = gsub("V1.", "", big_df$year )
big_df$year = gsub("V1", "", big_df$year )
big_df$year = gsub("V", "", big_df$year )
big_df$year = gsub("\\.", "-", big_df$year )
big_df$month = gsub("_", "", big_df$month )

#big_df = big_df[,-4]
unique(big_df$year); unique(big_df$variable); unique(big_df$month)


big_df = big_df %>% mutate(year = case_when(
  year=="20" ~ 0,  year=="19" ~ 100,  year=="18" ~ 200,   year== "17" ~300,  year=="16" ~400,  year== "15" ~500,  year== "14" ~600,  year== "13" ~700,
  year== "12" ~800,  year=="11" ~900,  year== "10" ~1000,  year== "9"~1100,  year== "8"~1200,  year=="7" ~1300,  year=="6" ~1400,  year=="5" ~1500,
  year== "4" ~1600,  year== "3" ~1700,  year== "2"  ~1800,  year== "1" ~1900,  year== "0" ~2000,  year=="-1" ~2100,  year=="-2" ~2200,  year== "-3"~2300,
  year=="-4"~2400,  year== "-5"~2500,  year=="-6"~2600,  year== "-7"~2700,  year== "-8"~2800,  year== "-9"~2900,  year== "-10" ~3000,  year== "-11"~3100,
  year== "-12" ~3200,  year== "-13" ~3300,  year== "-14"~3400,  year==  "-15"~ 3500,year==  "-16" ~3600,  year== "-17" ~3700,  year==  "-18"~3800,  year== "-19" ~3900,
  year==  "-20" ~4000,  year==  "-21"~4100,   year== "-22" ~4200 ,   year==  "-23" ~4300 ,  year==  "-24" ~4400 ,  year=="-25"~ 4500  ,  year==  "-26"~4600  ,
  year==  "-27" ~4700  ,  year== "-28" ~4800  ,  year== "-29"~4900 ,  year==  "-30"~ 5000,   year==  "-31" ~ 5100,   year==  "-32"~ 5200,  year==  "-33" ~  5300,  
  year==  "-34" ~5400  ,  year== "-35" ~5500 ,  year==  "-36" ~ 5600,   year==  "-37" ~ 5700 ,  year==  "-38"~ 5800  ,  year== "-39"~5900  ,  year== "-40" ~6000 ,
  year==  "-41" ~ 6100,    year==  "-42" ~6200,  year==  "-43" ~6300 ,  year==  "-44"~ 6400 ,  year==  "-45" ~ 6500 ,   year==  "-46"~6600  ,  year==  "-47" ~6700  ,
  year==  "-48" ~6800  ,  year==  "-49" ~6900 ,  year==  "-50" ~ 7000 ,  year==  "-51" ~ 7100  ,  year==  "-52"~7200 ,  year==  "-53" ~ 7300,    year==  "-54"~7400  ,
  year== "-55"~7500  ,  year==  "-56"~ 7600 ,  year== "-57" ~7700 ,  year==  "-58" ~ 7800,   year== "-59"~ 7900 ,  year== "-60" ~ 8000 ,   year==  "-61"~8100  ,
  year==  "-62"~8200  ,  year==  "-63" ~8300  ,  year==  "-64" ~8400  ,  year==  "-65" ~8500,  year== "-66" ~8600 ,  year== "-67" ~ 8700 ,   year==  "-68" ~8800  ,
  year==  "-69" ~8900  ,  year==  "-70"~9000  ,  year== "-71" ~9100 ,  year== "-72" ~ 9200 ,   year==  "-73" ~9300  ,  year== "-74" ~9400  ,  year== "-75" ~9500  ,
  year== "-76" ~9600  ,  year==  "-77" ~9700  ,  year== "-78" ~9800  ,  year== "-79" ~9900 ,  year== "-80"~10000 ,  year==  "-81"~10100 ,  year==  "-82" ~10200 ,
  year==  "-83"~10300 ,  year==  "-84" ~10400 ,  year==  "-85" ~10500 ,  year==  "-86" ~10600 ,  year==  "-87" ~10700 ,  year==  "-88" ~10800,  year==  "-89"~10900 ,  year==  "-90" ~11000 ,
  year==  "-91" ~11100 ,  year==  "-92" ~11200 ,  year==  "-93" ~11300,  year==  "-94" ~ 11400,   year== "-95"~11500 ,  year== "-96" ~11600 ,  year==  "-97" ~11700,
  year== "-98" ~ 11800 ,  year=="-99"~11900 ,  year=="-100" ~12000 ,  year==  "-101"~12100 ,  year==  "-102"~ 12200,  year==  "-103" ~12300,  year==  "-104" ~12400 ,  year==  "-105" ~12500 ,
  year==  "-106" ~12600 ,  year==  "-107" ~12700 ,  year==  "-108" ~12800,  year==  "-109"  ~12900,   year==  "-110" ~13000 ,  year==  "-111" ~ 13100,  year==  "-112" ~13200 ,
  year== "-113" ~13300 ,  year==  "-114" ~13400 ,  year== "-115" ~13500 ,  year== "-116"~13600,  year== "-117" ~13700,   year==  "-118" ~ 13800,  year== "-119" ~13900,
  year== "-120" ~14000 ,  year== "-121" ~ 14100 ,  year== "-122" ~ 14200 ,  year== "-123" ~14300 ,  year==  "-124" ~14400 ,  year==  "-125" ~14500,  year==  "-126" ~14600 ,
  year==  "-127" ~ 14700 ,  year==  "-128" ~14800 ,  year==  "-129" ~14900,  year==  "-130" ~15000 ,  year==  "-131" ~ 15100 ,  year==  "-132" ~15200 ,  year==  "-133" ~15300 ,
  year==  "-134"~15400,  year==  "-135" ~15500,   year==  "-136" ~15600 ,  year==  "-137" ~15700 ,  year==  "-138" ~15800 ,  year== "-139" ~15900 ,  year==  "-140" ~16000 ,
  year== "-141" ~16100 ,  year== "-142" ~16200,  year== "-143" ~16300,  year== "-144"~ 16400 ,  year=="-145"~16500 ,  year== "-146"~ 16600,   year== "-147" ~16700 ,
  year==  "-148" ~16800 ,  year==  "-149" ~16900 ,  year==  "-150" ~17000 ,  year== "-151" ~17100 ,  year== "-152"~17200 ,  year==  "-153"~ 17300,   year== "-154" ~17400 ,
  year==  "-155" ~17500 ,  year== "-156" ~17600 ,  year== "-157" ~17700,  year== "-158" ~17800 ,  year== "-159" ~17900 ,  year==  "-160" ~18000 ,  year=="-161" ~18100 ,
  year== "-162" ~18200,  year=="-163" ~ 18300 ,  year=="-164" ~18400 ,  year== "-165" ~18500 ,  year==  "-166" ~18600,  year==  "-167" ~ 18700,  year==  "-168" ~ 18800,
  year==  "-169" ~ 18900,  year==  "-170"~ 19000 ,  year== "-171" ~19100 ,  year== "-172" ~19200 ,  year== "-173" ~19300 ,  year== "-174"~19400 ,  year=="-175"~19500,  year=="-176" ~ 19600,   year=="-177" ~19700,
  year=="-178"~ 19800 ,  year=="-179" ~19900 ,  year=="-180" ~20000, year=="-181" ~20100, year=="-182" ~20200,  year=="-183" ~20300,  year=="-184" ~20400,  year=="-185" ~20500,  year=="-186" ~20600,  year=="-187" ~20700,
  year=="-188" ~20800,  year=="-189" ~20900,year=="-190" ~21000,  year=="-191" ~21100,  year=="-192" ~21200,  year=="-193" ~21300,  year=="-194" ~21400,  year=="-195" ~21500,  year=="-196" ~21600,   year=="-197" ~21700,  year=="-198" ~21800,  year=="-199" ~21900,  year=="-200" ~22000
))

unique(big_df$year)
which_na = big_df[is.na(big_df$year),] #check if something went wrong, if it is ok, it is empty!

big_df$grid_cell = rep(seq(1, 7320, by = 1),12*221)

#add label name for each point 
names(df)[c(4,3)] = c('lat','lon')
head(big_df)

names(df); names(big_df)

write.table(big_df, 'Covariates/Covariates.percentages/monthly/tasmin_total_corr_fact.txt')

#before we continue, see if it is ok what we have done up to now----------------------------------------------------------------------
library(sf)
grid = st_read('Covariates/Covariates.percentages/monthly/polygon_new_50.shp')
grid$cell_id = rownames(grid)
grid$CHELSA_ = NULL
pr = read.table('Covariates/Covariates.percentages/monthly/pr_total_corr_fact_add_sites.txt')
names(pr)[c(1,2,3,8)] = c('value', 'lon', 'lat', 'cell_id')
take_nov = pr[pr$month == 11,]
take_yr0 = take_nov[take_nov$year ==0,]
grid_2 = merge(grid, take_yr0, by = 'cell_id')

library(ggplot2)
ggplot()+
  geom_sf(grid_2, mapping = aes(fill = value))

#it has a shape..well done!


#now we remove the gridcells that contains water or ice, but first we remove the years that we do not need.---------------------------------
library(dplyr)
pr = read.table('Covariates/Covariates.percentages/monthly/pr_total_corr_fact_old.txt')
tasmax = read.table('Covariates/Covariates.percentages/monthly/tasmax_total_corr_fact_old.txt')
tasmin = read.table('Covariates/Covariates.percentages/monthly/tasmin_total_corr_fact_old.txt')

names(pr)[c(1,2,3,8)] = c('value', 'lon', 'lat', 'cell_id')
names(tasmax)[c(1,2,3,8)] = c('value', 'lon', 'lat', 'cell_id')
names(tasmin)[c(1,2,3,8)] = c('value', 'lon', 'lat', 'cell_id')

pr = pr[!pr$year >20000,]
tasmax = tasmax[!pr$year >20000,]
tasmin = tasmin[!tasmin$year >20000,]

#then finally we create the dataset similar to the one that stores the bio variables---------------------------------------------------
sort(unique(pr$year))
#remove the gridcells 
to_remove = read.table(paste0('Covariates/gridcells_to_be_removed_50.txt'))

#now we use those data to reshape the climate data and set as -999 all the cells to remove 

df = read.table(file ='df_ready_to_krige_vfinal_corr_fact_add_sites.txt', header = T)


#df = merge(df,cerealia, by = c('X',  'Pollen.sum', 'CalYrBP', 'Label', 'Name','LatDD','LonDD'))
years = seq(500,20000, by=500)

#now remove zeros and NA entries from the dataset. We do that because we
#will perform the dynamic st kriging
df_3 = df
sites = unique(df_3$Label)
df_3 = df_3[!is.na(df_3$Abies_perc),]
df_zeros = df_3[df_3$Abies_perc ==0,]
df_not_zeros = df_3[df_3$Abies_perc >0,] #we remove the zeros
df_not_zeros = df_not_zeros[!is.na(df_not_zeros$Abies_perc),]
df_not_zeros = df_not_zeros[order(df_not_zeros$Label, df_not_zeros$CalYrBP),] #reorder the data 

hist(df_not_zeros$Abies_perc)
min(df_not_zeros$Abies_perc); max(df_not_zeros$Abies_perc)
df_not_zeros$Abies_perc = df_not_zeros$Abies_perc/100 #our original dataset goes from 0 to 100%, here I just ridimension it to 0-1
df_not_zeros$Abies_perc = log(df_not_zeros$Abies_perc/(1-df_not_zeros$Abies_perc)) #transformation
hist(df_not_zeros$Abies_perc)

df_zeros$Abies_perc = -20
df_3 = rbind(df_not_zeros, df_zeros)

names(df_3)[which(names(df_3) %in% c('LatDD'))] = c('lat')
names(df_3)[which(names(df_3) %in% c('LonDD'))] = c('lon')

#trasform the coordinates in metric (necessary for geomodels)
df_3 = st_as_sf(df_3, coords = c('lon','lat'), crs = st_crs('WGS84'))
df_3 = st_transform(df_3, crs =st_crs('epsg:3035'))
df_3$lon = st_coordinates(df_3)[,1]
df_3$lat = st_coordinates(df_3)[,2]

#this script is necessary to extrapolate data from a given resolution

#load the reference file 
grid = st_read(paste0("Covariates/Covariates.percentages/monthly//polygon_new_50.shp"))
grid = grid %>% st_set_crs("+proj=longlat +datum=WGS84")
grid$CHELSA_ = NULL #remove the variables

grid$cell_id = as.numeric(rownames(grid))
plot(grid['cell_id'])

#transform to metric 
grid <- st_transform(grid, crs = st_crs("epsg:3035"))
grid

#find in which gridcell the points fall
df_4 = df_3
names(df_4)
df_4 = df_4[,-(which(names(df_4) %in% c('Abies','Name','Pollen.sum','lon','lat')))]
grid_match = st_join(df_4, grid)

table(is.na(grid_match$cell_id))

#now we create the dataset of covariables and extract those values for our points, 
#first we load the variables 
big_df = list()
total_df = list()

i = 1
j = 500
k = 1

for (i in 1:12){
  final_bio_data = list()
  n <- vector()
  this_month = tasmax[tasmax$month == i, ] #here you have to change tasmax, tasmax or tasmi
  unique_coords = unique(tasmax[,c('lon','lat')])#here you have to change tasmax, tasmax or tasmi
  for (k in 1:nrow(unique_coords)) {
    take_coords = unique_coords[k,]
    x <- this_month[this_month$lon  == take_coords$lon & this_month$lat  == take_coords$lat,]
    
    time.mid <- seq(250, 19750, 500)
    value.bin <- rep(NA, length(time.mid))
    half.win <- 250
    n <- append(n,values=(time.mid))
    
    for (f in 1:length(time.mid)) {
      age.lo <- time.mid[f] - half.win
      age.hi <- time.mid[f] + half.win
      ageID <- which(x$year >= age.lo & x$year < age.hi)
      value.bin[f] = mean(x$value[ageID], na.rm=TRUE)
    }
    
    final.bin <- as.data.frame(value.bin)
    final.bin$year <- time.mid+250
    final.bin$lat <- rep(x[1,'lat'], length(time.mid))
    final.bin$lon <- rep(x[1,'lon'], length(time.mid))
    final.bin$variable = rep(unique(x$variable), length(time.mid))
    final_bio_data[[k]] =  final.bin
    
  }
  
  final_bio_data = bind_rows(final_bio_data)
  
  bio01 = final_bio_data
  bio01$month = i
  bio01 = st_as_sf(bio01, coords = c('lon','lat'), crs= 'WGS84')
  bio01 <- st_transform(bio01, crs = st_crs("epsg:3035"))
  bio01$lon = st_coordinates(bio01)[,1]
  bio01$lat = st_coordinates(bio01)[,2]
  
  bio_data = data.frame()
  bio_data_2 = data.frame()
  
  for (j in years){
    to_remove_yr = to_remove[to_remove$i ==j,]
    to_remove_yr = to_remove_yr[!to_remove_yr$cell_id %in% grid_match$cell_id,] 
    new_grid = grid
    year_df = bio01[bio01$year ==j,]
    grid_match_2 = st_join(year_df, new_grid)
    names(grid_match_2)[2] = 'CalYrBP'
    names(grid_match_2)[1] = 'value'
    grid_match_2 = as.data.frame(grid_match_2)
    grid_match_2$value[grid_match_2$cell_id %in% to_remove_yr$cell_id] = -999
    values = merge(as.data.frame(grid_match),grid_match_2, by = c('cell_id', 'CalYrBP'))
    
    bio_data = rbind(bio_data, values)
    bio_data_2 = rbind(bio_data_2, grid_match_2)
  }
  
  big_df[[i]]= bio_data
  total_df[[i]] = bio_data_2
  print(i)
} #for loop that extract all covariable data

big_df = do.call(rbind, big_df)
total_df = do.call(rbind, total_df)

head(big_df)

big_df$lon.y = NULL; big_df$lat.y = NULL
big_df$lon.x = NULL; big_df$lat.x = NULL
big_df$geometry.y = NULL
big_df = st_as_sf(big_df)
geometries = st_coordinates(big_df)
big_df$lat= geometries[,2]; big_df$lon= geometries[,1]
big_df = data.frame(big_df)
big_df <- big_df[order(big_df$cell_id, big_df$CalYrBP),]
class(big_df); class(total_df)
big_df$geometry.x = NULL
total_df$geometry = NULL
write.table(big_df, paste0('big_df_50_cerealia_tasmax_corr_fact_add_sites.txt'), col.names = T)
write.table(total_df, paste0('total_df_50_cerealia_tasmax_corr_fact_add_sites.txt'))

#now we create the dataset of covariables and extract those values for our points, 
#first we load the variables 
big_df = list()
total_df = list()

i = 1
j = 500

for (i in 1:12){
  final_bio_data = list()
  n <- vector()
  this_month = tasmin[tasmin$month == i, ] #here you have to change tasmax, tasmax or tasmi
  unique_coords = unique(tasmin[,c('lon','lat')])#here you have to change tasmax, tasmax or tasmi
  for (k in 1:nrow(unique_coords)) {
    take_coords = unique_coords[k,]
    x <- this_month[this_month$lon  == take_coords$lon & this_month$lat  == take_coords$lat,]
    
    time.mid <- seq(250, 19750, 500)
    value.bin <- rep(NA, length(time.mid))
    half.win <- 250
    n <- append(n,values=(time.mid))
    
    for (f in 1:length(time.mid)) {
      age.lo <- time.mid[f] - half.win
      age.hi <- time.mid[f] + half.win
      ageID <- which(x$year >= age.lo & x$year < age.hi)
      value.bin[f] = mean(x$value[ageID], na.rm=TRUE)
    }
    
    final.bin <- as.data.frame(value.bin)
    final.bin$year <- time.mid+250
    final.bin$lat <- rep(x[1,'lat'], length(time.mid))
    final.bin$lon <- rep(x[1,'lon'], length(time.mid))
    final.bin$variable = rep(unique(x$variable), length(time.mid))
    final_bio_data[[k]] =  final.bin
    
  }
  
  final_bio_data = bind_rows(final_bio_data)
  
  bio01 = final_bio_data
  bio01$month = i
  bio01 = st_as_sf(bio01, coords = c('lon','lat'), crs= 'WGS84')
  bio01 <- st_transform(bio01, crs = st_crs("epsg:3035"))
  bio01$lon = st_coordinates(bio01)[,1]
  bio01$lat = st_coordinates(bio01)[,2]
  
  bio_data = data.frame()
  bio_data_2 = data.frame()
  
  for (j in years){
    to_remove_yr = to_remove[to_remove$i ==j,]
    to_remove_yr = to_remove_yr[!to_remove_yr$cell_id %in% grid_match$cell_id,] 
    new_grid = grid
    year_df = bio01[bio01$year ==j,]
    grid_match_2 = st_join(year_df, new_grid)
    names(grid_match_2)[2] = 'CalYrBP'
    names(grid_match_2)[1] = 'value'
    grid_match_2 = as.data.frame(grid_match_2)
    grid_match_2$value[grid_match_2$cell_id %in% to_remove_yr$cell_id] = -999
    values = merge(as.data.frame(grid_match),grid_match_2, by = c('cell_id', 'CalYrBP'))
    
    bio_data = rbind(bio_data, values)
    bio_data_2 = rbind(bio_data_2, grid_match_2)
  }
  
  big_df[[i]]= bio_data
  total_df[[i]] = bio_data_2
  print(i)
} #for loop that extract all covariable data

big_df = do.call(rbind, big_df)
total_df = do.call(rbind, total_df)

head(big_df)

big_df$lon.y = NULL; big_df$lat.y = NULL
big_df$lon.x = NULL; big_df$lat.x = NULL
big_df$geometry.y = NULL
big_df = st_as_sf(big_df)
geometries = st_coordinates(big_df)
big_df$lat= geometries[,2]; big_df$lon= geometries[,1]
big_df = data.frame(big_df)
big_df <- big_df[order(big_df$cell_id, big_df$CalYrBP),]
class(big_df); class(total_df)
big_df$geometry.x = NULL
total_df$geometry = NULL
write.table(big_df, paste0('big_df_50_cerealia_tasmin_corr_fact_add_sites.txt'), col.names = T)
write.table(total_df, paste0('total_df_50_cerealia_tasmin_corr_fact_add_sites.txt'))


#now we create the dataset of covariables and extract those values for our points, 
#first we load the variables 
big_df = list()
total_df = list()

i = 1
j = 500

for (i in 1:12){
  final_bio_data = list()
  n <- vector()
  this_month = pr[pr$month == i, ] #here you have to change tasmax, tasmax or tasmi
  unique_coords = unique(pr[,c('lon','lat')])#here you have to change tasmax, tasmax or tasmi
  for (k in 1:nrow(unique_coords)) {
    take_coords = unique_coords[k,]
    x <- this_month[this_month$lon  == take_coords$lon & this_month$lat  == take_coords$lat,]
    
    time.mid <- seq(250, 19750, 500)
    value.bin <- rep(NA, length(time.mid))
    half.win <- 250
    n <- append(n,values=(time.mid))
    
    for (f in 1:length(time.mid)) {
      age.lo <- time.mid[f] - half.win
      age.hi <- time.mid[f] + half.win
      ageID <- which(x$year >= age.lo & x$year < age.hi)
      value.bin[f] = mean(x$value[ageID], na.rm=TRUE)
    }
    
    final.bin <- as.data.frame(value.bin)
    final.bin$year <- time.mid+250
    final.bin$lat <- rep(x[1,'lat'], length(time.mid))
    final.bin$lon <- rep(x[1,'lon'], length(time.mid))
    final.bin$variable = rep(unique(x$variable), length(time.mid))
    final_bio_data[[k]] =  final.bin
    
  }
  
  final_bio_data = bind_rows(final_bio_data)
  
  bio01 = final_bio_data
  bio01$month = i
  bio01 = st_as_sf(bio01, coords = c('lon','lat'), crs= 'WGS84')
  bio01 <- st_transform(bio01, crs = st_crs("epsg:3035"))
  bio01$lon = st_coordinates(bio01)[,1]
  bio01$lat = st_coordinates(bio01)[,2]
  
  bio_data = data.frame()
  bio_data_2 = data.frame()
  
  for (j in years){
    to_remove_yr = to_remove[to_remove$i ==j,]
    to_remove_yr = to_remove_yr[!to_remove_yr$cell_id %in% grid_match$cell_id,] 
    new_grid = grid
    year_df = bio01[bio01$year ==j,]
    grid_match_2 = st_join(year_df, new_grid)
    names(grid_match_2)[2] = 'CalYrBP'
    names(grid_match_2)[1] = 'value'
    grid_match_2 = as.data.frame(grid_match_2)
    grid_match_2$value[grid_match_2$cell_id %in% to_remove_yr$cell_id] = -999
    values = merge(as.data.frame(grid_match),grid_match_2, by = c('cell_id', 'CalYrBP'))
    
    bio_data = rbind(bio_data, values)
    bio_data_2 = rbind(bio_data_2, grid_match_2)
  }
  
  big_df[[i]]= bio_data
  total_df[[i]] = bio_data_2
  print(i)
} #for loop that extract all covariable data

big_df = do.call(rbind, big_df)
total_df = do.call(rbind, total_df)

head(big_df)

big_df$lon.y = NULL; big_df$lat.y = NULL
big_df$lon.x = NULL; big_df$lat.x = NULL
big_df$geometry.y = NULL
big_df = st_as_sf(big_df)
geometries = st_coordinates(big_df)
big_df$lat= geometries[,2]; big_df$lon= geometries[,1]
big_df = data.frame(big_df)
big_df <- big_df[order(big_df$cell_id, big_df$CalYrBP),]
class(big_df); class(total_df)
big_df$geometry.x = NULL
total_df$geometry = NULL
write.table(big_df, paste0('big_df_50_cerealia_pr_corr_fact_add_sites.txt'), col.names = T)
write.table(total_df, paste0('total_df_50_cerealia_pr_corr_fact_add_sites.txt'))

