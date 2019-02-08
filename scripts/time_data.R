time_data <- read.csv("data/timewise_accidents.csv")

#Replace all the "_" and blank cells by NA
is.na(time_data) <- time_data == "_"
is.na(time_data) <- time_data == ""

#Plot missing data values in each variable
library(VIM)
aggr_plot <- aggr(time_data, col=c('purple','green'), numbers=TRUE, border = "blue", sortVars=TRUE, 
                  labels=names(time_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Convert all variables except time interval to int type
convertColToInt <- function(df)
{
  for(i in 1:ncol(df))
  {
    if(i != 2)
    {
      df[,i] <- as.integer(df[,i])
    }
  }
  return(df)
}
time_data <- convertColToInt(time_data)

library(mice)
imputed_Time_Data <- mice(time_data[,-2], m=5, maxit = 50, method = 'pmm', seed = 500)
time_data_imputed <- complete(imputed_Time_Data, 2)

library(tibble)
time_data <- add_column(time_data_imputed, time_interval = time_data$time_interval, .after = "year")

#Display customized summary statistics of the dataset
library(fBasics)
basicStats(time_data[,-2])[c("Mean", "Variance", "Stdev","Median", "Minimum",
                              "Maximum", "NAs", "Skewness"),]

library(reshape2)
library(ggplot2)

time_data_melt <- melt(data = time_data, id.vars = "time_interval")
ggplot(time_data_melt, aes(x = variable, y = value, color = variable, fill = variable)) + 
geom_boxplot(notch = TRUE, varwidth = TRUE, outlier.fill = "purple", outlier.colour = "purple", 
             outlier.alpha = 1) + theme_classic() + 
facet_wrap(~variable, nrow = 3, ncol = 5, scales = "free")

#Density plot
year_data_melt <- melt(data = time_data, id.vars = "year")
ggplot(year_data_melt) + geom_density(aes(x = variable, color = variable, fill = variable, alpha = 0.0)) + 
facet_wrap(~year, nrow = 6, ncol = 4) + theme_light(base_line_size = 0, base_size = 6)

