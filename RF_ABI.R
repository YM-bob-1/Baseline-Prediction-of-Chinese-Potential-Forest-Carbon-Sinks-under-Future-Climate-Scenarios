# import R package
library(GWmodel)      ## GW models
library(plyr)         ## Data management
library(sp)           ## Spatial Data management
library(spdep)        ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(raster)       ## spatial data
library(grid)         ## plot
library(gridExtra)    ## Multiple plot
library(ggplot2)      #  plotting
library(tidyverse)    # data 
library(SpatialML)    # Geographically weigted regression
library(caret)

library(h2o)
h2o.init()

# input Data
## The columns of data: FID, pointid, type_1, type_2, X, Y, Tas, Re, Height, Slope, Aspect, Age and ABI;
Data <- read.csv("……data……", header=T)

num <- seq(10, 200, by = 3)
para <- matrix(data=NA, nrow=length(num), ncol=3)

### Random sampling
# Split the training set and test set
set.seed(123) # Random seed
index <- createDataPartition(Data$ABI, p = 0.7, list = FALSE)
Train_Data <- Data[index, ]
Test_Data <- Data[-index, ]

# Create h2o data frame
Train_mf <- Train_Data[, 8:14]
Test_mf <- Test_Data[, 8:14]

Train_hex <- as.h2o(Train_mf)
Test_hex <- as.h2o(Test_mf)

# Define independent and dependent variables
y <- "ABI"
x <- setdiff(names(Train_hex), y)

### Hyperparameter optimization
Ntrees  <- seq(50, 500, by = 50)
Max_depth <- c(5, 10, 15,20,25,30)
Sample_rate <- c(0.1,0.3,0.5,0.7,0.9)

# Create precision output matrix
n <- length(Ntrees)*length(Max_depth)*length(Sample_rate)

para <- matrix(data=NA, nrow=n, ncol=5)
num <- 1

# grid  search
for (i in 1:length(Ntrees)){
  for (j in 1:length(Max_depth)){
    for (e in 1:length(Sample_rate)){
      rf_fit <- h2o.randomForest(x = x,
                                 y = y,
                                 training_frame = Train_hex,
                                 model_id = "rf_fit",
                                 ntrees = Ntrees[i],
                                 max_depth = Max_depth[j],
                                 sample_rate = Sample_rate[e],
                                 min_rows =10,
                                 seed = 1234)
      rf_perf <- h2o.performance(model = rf_fit,
                                 newdata = Test_hex)
      para[num,1] <- Ntrees[i]
      para[num,2] <- Max_depth[j]
      para[num,3] <- Sample_rate[e]
      para[num,4] <- h2o.rmse(rf_perf)
      para[num,5] <- h2o.r2(rf_perf)
      num <- num+1;
    }
  }
}

# View optimal precision parameters
para[which.max(para[, 5]),]

#### Save data
write.csv(Train_Data,file="……data……",row.names=TRUE)
write.csv(Test_Data,file="……data……",row.names=TRUE)
