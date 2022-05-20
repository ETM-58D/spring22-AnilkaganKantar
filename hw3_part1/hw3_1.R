
#Install openxlsx to Read, Write and Edit xlsx Files and 'rgl' for 3D visualizaton of data and 
#tidyverse for general utiliy and plotting tools
#Installation not needed and can be commented if packages already installed.

# install.packages("rgl")
# install.packages("openxlsx")
#install.packages("tidyverse")

#load the libraries

library("rgl")
library("openxlsx")
library("tidyverse")

#load movement data

mov_X <- read.xlsx("G:/Gdrive/ETM/ETM_58D/HW3/UWave-20220520T122200Z-001/UWave/UWave_TRAIN/uWaveGestureLibrary_X_TRAIN.xlsx", colNames = FALSE)
mov_y <- read.xlsx("G:/Gdrive/ETM/ETM_58D/HW3/UWave-20220520T122200Z-001/UWave/UWave_TRAIN/uWaveGestureLibrary_Y_TRAIN.xlsx", colNames = FALSE)
mov_z <- read.xlsx("G:/Gdrive/ETM/ETM_58D/HW3/UWave-20220520T122200Z-001/UWave/UWave_TRAIN/uWaveGestureLibrary_Z_TRAIN.xlsx", colNames = FALSE)

#Choose a gesture and create subsets of data. Afterwards draw a 3d scatterplot. Trail to be plotted will be chosen randomly from subset.

#choose a gesture tpye by changing the variable between 1 and 8. Subset of data dor that gesture will be created.
 
gesvar <- 1


mov_x1 <- mov_X %>% filter(X1 == gesvar)
mov_y1 <- mov_y %>% filter(X1 == gesvar)
mov_z1 <- mov_z %>% filter(X1 == gesvar)


#Pick a random row and generate the plot for it. Specific row can be choosen by assigning the row number to "pickedrow" variable.

pickedrow <- floor(runif(1, min = 0, max = nrow(mov_x1)))

plot3d(mov_x1[pickedrow,2:ncol(mov_x1)],
       mov_y1[pickedrow,2:ncol(mov_x1)], 
       mov_z1[pickedrow,2:ncol(mov_x1)],
       xlab = "X", ylab = "Y", zlab = "Z",
       col = "red",
       type = "l")
gesturelabels <- paste("Gesture",gesvar)
rowlabels <- paste("Row",pickedrow)
legend3d("topright", c(gesturelabels, rowlabels), cex = 0.8)


