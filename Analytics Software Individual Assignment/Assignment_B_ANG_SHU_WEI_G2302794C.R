#see existing inbuilt datasets
data()
#remove all objects just in case there are some objects in the global environment
rm(list=ls())
#install.packages("ggplot2") if have not installed yet
library(ggplot2)

#look at the top 6 observations of CO2 dataset
head(CO2)
#overview of the structure of CO2 dataset
str(CO2)

#plot y=uptake vs x=conc datapoints in a plot
ggplot(CO2) + aes(conc, uptake) + geom_point()

#by including the factor of Type in CO2 dataset using colour
#able to identify the points by the 2 factor levels of Type in CO2
ggplot(CO2) + aes(conc, uptake, col = CO2$Type) + geom_point()

#instead of identifying the data using colours on factor levels
#split the dataset into 2 plots instead 
#since there are 2 factor levels of Type - "Quebec","Mississippi"
ggplot(CO2) + aes(conc, uptake) + geom_point() + facet_wrap(CO2$Type)

