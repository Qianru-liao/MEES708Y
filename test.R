# library(dplyr)
# library(ggplot2)
# 
# #source("myplot_Qianru.R")
# 
# # Dataset for the source figure
# ad <- read.csv("AD_cluster_3.csv")
# rownames(ad) <- ad[, 1]
# ad <- ad[, -1]
# 
# # Dataset in ts format
# EuStockMarkets
# 
# # Dataset with missing values
# ad_na <- ad
# ad_na$CHOTF[ad$CHOTF == 0] <- NA
# 
# # Dataset of 1 time series (should give an error)
# ts01 <- rnorm(100)
# ts02 <- ts(ts01)
# ts03 <- ad[, 1, drop = FALSE]
# 
# # Check the function
# ## 60 points: Should work in each case
# myplot(ad)
# myplot(EuStockMarkets)
# myplot(ad_na)
# 
# ## 25 points: Consider changing the color and add/remove the line for the average
# ## (the arguments plotmean and color can be named differently by the student)
# myplot(ad, plotmean = TRUE, color = "red")
# 
# ## 15 points: Should not work (give an informative error in each case)
# #myplot(ts01)
# #myplot(ts02)
# #myplot(ts03)
