month_data <- read.csv("data/monthwise_accidents.csv")

#Plot missing data values in each variable
library(VIM)
aggr_plot <- aggr(month_data, col=c('purple','green'), numbers=TRUE, border = "blue", sortVars=TRUE, 
                  labels=names(month_data), cex.axis=.7, gap=1, ylab=c("Histogram of missing data","Pattern"))

#Removing NA values
month_data = na.omit(month_data)

#Convert data types of all except month variable to int
month_data = convertColsToInt(month_data)

#Display customized summary statistics of the dataset
library(fBasics)
basicStats(month_data[,-2])[c("Mean", "Variance", "Stdev","Median", "Minimum",
                            "Maximum", "NAs", "Skewness"),]

library(reshape2)
library(ggplot2)

month_data[month_data == "AUGUEST"] <- "AUGUST"
month_data_melt <- melt(data = month_data, id.vars = "month")
ggplot(month_data_melt, aes(x = variable, y = value, color = variable, fill = variable)) + 
  geom_boxplot(notch = TRUE, varwidth = TRUE, outlier.fill = "purple", outlier.colour = "purple", 
               outlier.alpha = 1) + theme_classic() + facet_wrap(~variable, nrow = 3, ncol = 5, scales = "free")

ggplot(month_data_melt) + geom_density(aes(x = variable, color = variable, fill = variable, alpha = 0.01)) + 
  facet_wrap(~month, nrow = 4, ncol = 3) + theme_light(base_line_size = 0, base_size = 6)

year_data_melt <- melt(data = month_data, id.vars = "year")
ggplot(year_data_melt) + geom_density(aes(x = variable, color = variable, fill = variable, alpha = 0.0)) + 
  facet_wrap(~year, nrow = 6, ncol = 4) + theme_light(base_line_size = 0, base_size = 6)
