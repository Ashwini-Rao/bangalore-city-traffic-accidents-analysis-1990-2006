vehicle_data <- read.csv("data/vehiclewise_accidents.csv")

#Remove the empty rows, replace all "_" by NA
vehicle_data <- na.omit(vehicle_data)
is.na(vehicle_data) <- vehicle_data == "_"
is.na(vehicle_data) <- vehicle_data == ""

#Plot missing data values in each variable
library(VIM)
aggr_plot <- aggr(vehicle_data, col=c('purple','green'), numbers=TRUE, border = "blue", sortVars=TRUE, 
                  labels=names(vehicle_data), cex.axis=.7, gap=1, ylab=c("Histogram of missing data","Pattern"))

#Convert data types of all except vehicle_type variable to int type
vehicle_data = convertColsToInt(vehicle_data)

library(mice)
imputed_Vehicle_Data <- mice(vehicle_data[,-2], m=5, maxit = 50, method = 'pmm', seed = 500)
vehicle_data_imputed <- complete(imputed_Vehicle_Data, 2)

library(tibble)
vehicle_data <- add_column(vehicle_data_imputed, vehicle_type = vehicle_data$vehicle_type, .after = "year")

#Display customized summary statistics of the dataset
library(fBasics)
basicStats(vehicle_data[,-2])[c("Mean", "Variance", "Stdev","Median", "Minimum",
                               "Maximum", "NAs", "Skewness"),]

library(reshape2)
library(ggplot2)
vehicle_data_melt <- melt(data = vehicle_data, id.vars = "vehicle_type")
ggplot(vehicle_data_melt, aes(x = variable, y = value, color = variable, fill = variable)) + 
  geom_boxplot(notch = TRUE, varwidth = TRUE, outlier.fill = "purple", outlier.colour = "purple", 
               outlier.alpha = 1) + theme_classic() + facet_wrap(~variable, nrow = 3, ncol = 5, scales = "free")

#Density plot
year_data_melt <- melt(data = vehicle_data, id.vars = "year")
ggplot(year_data_melt) + geom_density(aes(x = variable, color = variable, fill = variable, alpha = 0.0)) + 
  facet_wrap(~year, nrow = 6, ncol = 4) + theme_light(base_line_size = 0, base_size = 6)
