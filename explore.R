library(tidyverse)
library(maptree)
require(sp)
require(rgeos)

load("maps.RData")

data = read.delim("BatchCorrOutput_ashy prinia.txt", sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA), row.names=1)

data = read.delim("BatchCorrOutput_ashy prinia1.txt", sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA), row.names=1)

ll = read.delim("Associated coordinates.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA), row.names=1)

temp = ll
coordinates(temp) = ~Longitude + Latitude # convert to SPDF?
#proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,statemap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$id = rownames(temp) # add column to join with the main data
ll$id = rownames(ll)
ll = left_join(temp,ll)

data$id = rownames(data)
data = left_join(data,ll)

data = data %>% group_by(ST_NM) %>%
  mutate(id = if(n( ) > 1) {paste0(ST_NM, row_number( ))} 
         else {paste0(ST_NM)}) %>%
  ungroup


rownames(data) = data$id

data = data %>% select(-id,-ST_NM,-Latitude,-Longitude)

dissimilarity = 1 - data
distance = as.dist(dissimilarity) 

clus = hclust(distance)
op_k = kgs(clus, distance, maxclus = NULL)
plot (names (op_k), op_k, xlab="# clusters", ylab="penalty")

as.integer(names(op_k[which(op_k == min(op_k))]))

plot(clus)



###################### relationship with euclidian distances


ll = ll %>% group_by(ST_NM) %>%
  mutate(id = if(n( ) > 1) {paste0(ST_NM, row_number( ))} 
         else {paste0(ST_NM)}) %>%
  ungroup


ll1 = ll %>% select(-ST_NM,-id)

library(geosphere)
ll2 = distm(ll1[c("Longitude","Latitude")],ll1[c("Longitude","Latitude")])*0.001
rownames(ll2) = ll$id
colnames(ll2) = ll$id

a1 = as.vector(ll2)
a2 = as.vector(as.matrix(data))

plot(a1,a2)
