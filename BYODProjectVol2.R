library(dplyr)
library(tidyr)
library(corrplot)
library(PerformanceAnalytics)
library(bnlearn)
library(tidyverse)
library(foreign)

setwd("C:/Winata/PhD/5_Fall2019/CPSC541 - Regression Analysis/BYODProject/CPSC541BYODProject/CPSC541BYODProject") ##Please change the path

df2 = read.csv("BYODProjectDataVol2.csv") ##Use data with present and absent of farmers' markets


##Change the FarMarPreAbs from numeric/integer to factor
for(i in c(8)){
  df2[,i]<- as.factor(df2[,i])
}

##Plot farmers market vs food environment index
plot1 <- ggplot(data = df2, aes(x = FarMarPresAbs, y = FoodEnvInd, colour = FarMarPresAbs)) + geom_point() 
print(plot1 + ggtitle("Presence and Absence of Farmers' Market Vs Food Environment Index"))
print(plot1 + labs(y="Food Environment Index", x = "Presence and Absence of Farmers' Markets"))
print(plot1 + labs(colour = "Farmers' Markets"))
plot1

##Plot farmers market vs adult obesity
plot2 <- ggplot(data = df2, aes(x = FarMarPresAbs, y = PercentOb16, colour = FarMarPresAbs)) + geom_point() 
print(plot2 + ggtitle("Presence and Absence of Farmers' Market Vs Food Environment Index"))
print(plot2 + labs(y="Percent of Adult Obesity", x = "Presence and Absence of Farmers' Markets"))
print(plot2 + labs(colour = "Farmers' Markets"))
plot2

#Visualize pairwise plots of all variables
colpairs <- c("#3ABEB0")
pairs(df2 [ ,8:12], col = colpairs, pch = 19, labels = c("Presence & Absence of 
      Farmers' Markets", 
                                                       "Percent of Adult Obesity", "Food Environment Index", "Income Ratio in 2016",
                                                       "Percent of Children Living 
      in Poverty in 2016"), lower.panel = NULL, cex.labels=2)
pairs()


pairs(df2[ ,8:12])

plot(df2$FarMarPresAbs)


#Create QQplot for Farmers market



#Visualize the correlations between the variables
corrplot(cor(df2[ ,8:12]))

#Visualize correlation with PerformanceAnalytics package
chart.Correlation(df2[ ,8:12], histogram = TRUE, pch = 19)

##fit1 for farmers market vs food environment index
fit1 <- lm(FoodEnvInd~FarMarPresAbs, data = df2)
summary(fit1)
plot(fit1)

Model1 <- lm(FoodEnvInd~FarMarPresAbs+IncoRatio16+PerChildPov16, data = df2)
summary(Model1)

#fit2 for farmers market vs percent of adult obesity
fit2 <- lm(PercentOb16~FarMarPresAbs, data = df2)
summary(fit2)
plot(fit2)

Model2 <- lm(PercentOb16~FarMarPresAbs+IncoRatio16+PerChildPov16, data = df2)
summary(Model2)


##Install RGL package for 3D scatter
install.packages("rgl")
library(rgl)

install.packages("scatterplot3d")
library(scatterplot3d)

##making 3d scatter for model 1
##Basic 3d plot
scatterplot3d(df2[ ,8:12])
scatterplot3d(df2[ ,8:12,-9])
scatterplot3d(df2[ ,8:12,-10])

scatterplot3d(df2[ ,8:12,-9], angle = 60)
scatterplot3d(df2[ ,8:10], main = "3D Scatter Plot", xlab = "Presence or Absence of Farmers' Markets", ylab = "Food Environment Index", 
              zlab = "Percent of Adult Obesity")

scatterplot3d(df2[ ,8:10], pch = 16, color="steelblue")

##Scatterplot for 3 variables
colors <- c("#56B4E9", "#E69F00")
colors <- colors[as.factor(df2$FarMarPresAbs)]
scatterplot3d(df2[ ,8:10], pch = 16, color = colors, grid = TRUE, box = FALSE, main = "3D Scatter Plot", xlab = "Presence or Absence Farmers' Markets", 
              ylab = "Percent of Adult Obesity", 
              zlab = "Food Environment Index" )



