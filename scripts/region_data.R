region_data <- read.csv("data/regionwise_accidents.csv")

#Replace "_" by NA
is.na(region_data) <- region_data == "_"
is.na(region_data) <- region_data == "Nil"
is.na(region_data) <- region_data == ""

#Plot missing data values in each variable
library(VIM)
aggr_plot <- aggr(region_data, col=c('purple','green'), numbers=TRUE, border = "blue", sortVars=TRUE, 
                  labels=names(region_data), cex.axis=.7, gap=1, ylab=c("Histogram of missing data","Pattern"))

#Convert data types of all except police_station variable to int
region_data = convertColsToInt(region_data)

library(mice)
imputed_Region_Data <- mice(region_data[,-2], m=5, maxit = 50, method = 'pmm', seed = 500)
region_data_imputed <- complete(imputed_Region_Data, 2)

library(tibble)
region_data <- add_column(region_data_imputed, police_station = region_data$police_station, .after = "year")

#Display customized summary statistics of the dataset
library(fBasics)
basicStats(region_data[,-2])[c("Mean", "Variance", "Stdev","Median", "Minimum",
                             "Maximum", "NAs", "Skewness"),]

library(reshape2)
library(ggplot2)
region_data_melt <- melt(data = region_data, id.vars = "police_station")
ggplot(region_data_melt, aes(x = variable, y = value, color = variable, fill = variable)) + 
  geom_boxplot(notch = TRUE, varwidth = TRUE, outlier.fill = "purple", outlier.colour = "purple", 
               outlier.alpha = 1) + theme_classic() + facet_wrap(~variable, nrow = 3, ncol = 5, scales = "free")

#Density plot
year_data_melt <- melt(data = region_data, id.vars = "year")
ggplot(year_data_melt) + geom_density(aes(x = variable, color = variable, fill = variable, alpha = 0.0)) + 
facet_wrap(~year, nrow = 6, ncol = 4) + theme_light(base_line_size = 0, base_size = 6)
