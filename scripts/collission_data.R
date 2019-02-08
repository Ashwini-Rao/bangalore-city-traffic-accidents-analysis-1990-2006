collision_data <- read.csv("data/collissionwise_accidents.csv")

#Remove all empty rows and replace all "_" by NA
collision_data <- na.omit(collision_data)
is.na(collision_data) <- collision_data == "_"

#Plot missing data values in each variable
library(VIM)
aggr_plot <- aggr(collision_data, col=c('purple','green'), numbers=TRUE, border = "blue", sortVars=TRUE, 
                  labels=names(collision_data), cex.axis=.7, gap=1, ylab=c("Histogram of missing data","Pattern"))

#Convert data types of all except collission_type variable to int type
collision_data = convertColsToInt(collision_data)

library(mice)
imputed_Collision_Data <- mice(collision_data[,-2], m=5, maxit = 50, method = 'pmm', seed = 500)
collision_data_imputed <- complete(imputed_Collision_Data, 2)

library(tibble)
collision_data <- add_column(collision_data_imputed, collission_type = collision_data$collission_type, .after = "year")

#Display customized summary statistics of the dataset
library(fBasics)
basicStats(collision_data[,-2])[c("Mean", "Variance", "Stdev","Median", "Minimum",
                                "Maximum", "NAs", "Skewness"),]

library(reshape2)
library(ggplot2)
collision_data_melt <- melt(data = collision_data, id.vars = "collission_type")
ggplot(collision_data_melt, aes(x = variable, y = value, color = variable, fill = variable)) + 
geom_boxplot(notch = TRUE, varwidth = TRUE, outlier.fill = "purple", outlier.colour = "purple", 
            outlier.alpha = 1) + theme_classic() + 
facet_wrap(~variable, nrow = 3, ncol = 5, scales = "free")

#Density plot
year_data_melt <- melt(data = collision_data, id.vars = "year")
ggplot(year_data_melt) + geom_density(aes(x = variable, color = variable, fill = variable, alpha = 0.0)) + 
facet_wrap(~year, nrow = 6, ncol = 4) + theme_light(base_line_size = 0, base_size = 6)
