library(openxlsx)
library(tidyverse)

#import distance data for cities

distdata <- read.xlsx("G:/Gdrive/ETM/ETM_58D/HW3/part2/ilmesafe.xlsx")

distdata
str(distdata)
#remove first row and first two columns, and name the rows, resulting in data frame suitable for MDS

dt1 <- distdata %>%slice(1:nrow(distdata))

row.names(dt1)=dt1[,2]
dt1 <- dt1 %>%select(3:ncol(dt1))

#apply MDS
MDS <- cmdscale(dt1, k=2)

#plot the map

plot(MDS[ ,1], MDS[ ,2], pch=19,
     xlab="", ylab="", main = "Turkey")
text(MDS[ ,1], MDS[ ,2], labels = row.names(MDS))
