day_data <- read.csv("data/daywise_accidents.csv")

#Plot missing data pattern
library(VIM)
aggr_plot <- aggr(day_data, col=c('purple','green'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(day_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Remove the empty rows
day_data <- na.omit(day_data)

convertColsToInt <- function(df)
{
  for(i in 1:ncol(df))
  {
    if(i == 2)
    {
      df[,i] <- as.factor(toupper(as.character(df[,i]))) 
    }
    else
    {
      df[,i] <- as.integer(df[,i])
    }
  }
  return(df)
}

day_data <- convertColsToInt(day_data)

#Display customized summary statistics of the dataset
library(fBasics)
basicStats(day_data[,-2])[c("Mean", "Variance", "Stdev","Median", "Minimum",
                            "Maximum", "NAs", "Skewness"),]

library(reshape2)
library(ggplot2)

day_data_melt <- melt(data = day_data, id.vars = "day")

#Box plot
ggplot(day_data_melt, aes(x = variable, y = value, color = variable, fill = variable)) + 
geom_boxplot(notch = TRUE, varwidth = TRUE, outlier.fill = "purple", outlier.colour = "purple", 
             outlier.alpha = 1) + 
theme_classic(base_size = 6, base_line_size = 0) + 
facet_wrap(~variable, nrow = 3, ncol = 5, scales = "free")
  
#Density plot 
#Days
ggplot(day_data_melt) + geom_density(aes(x = variable, color = variable, fill = variable, alpha = 0.01)) + 
facet_wrap(~day, nrow = 4, ncol = 2) + theme_light(base_line_size = 0, base_size = 6)

#Years
year_data_melt <- melt(data = day_data, id.vars = "year")
ggplot(year_data_melt) + geom_density(aes(x = variable, color = variable, fill = variable, alpha = 0.0)) + 
facet_wrap(~year, nrow = 6, ncol = 4) + theme_light(base_line_size = 0, base_size = 6)

#Trellis plots
p1 <- qplot(year, fatal_total, data = day_data, color = day, facets = ~day, xlab = "Year", ylab = "Fatalities per day")  
p2 <- qplot(year, severe_total, data = day_data, color = day, facets = ~day, xlab = "Year", ylab = "Severities per day")  
p3 <- qplot(year, injured_total, data = day_data, color = day, facets = ~day, xlab = "Year", ylab = "Injuries per day")
p4 <- qplot(year, noninjured_cases, data = day_data, color = day, facets = ~day, xlab = "Year", ylab = "Non-injuries per day") 
p5 <- qplot(year, fatal_male, data = day_data, color = day, facets = ~day, xlab = "Year", ylab = "Male Fatalities per day")  
p6 <- qplot(year, fatal_female, data = day_data, color = day, facets = ~day, xlab = "Year", ylab = "Female Fatalities per day")  
p7 <- qplot(year, severe_male, data = day_data, color = day, facets = ~day, xlab = "Year", ylab = "Male Severities per day") 
p8 <- qplot(year, severe_female, data = day_data, color = day, facets = ~day, xlab = "Year", ylab = "Female Severities per day") 
p9 <- qplot(year, injured_male, data = day_data, color = day, facets = ~day, xlab = "Year", ylab = "Male Injuries per day")
p10 <- qplot(year, injured_female, data = day_data, color = day, facets = ~day, xlab = "Year", ylab = "Female Injuries per day")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, top = "Year v/s Fatality, Severe, Injured and Non-injured on a per day basis")
grid.arrange(p5, p6, nrow = 2, p1, top = "Male vs Female fatalities in Total fatalities per day")
grid.arrange(p7, p8, nrow = 2, p2, top = "Male vs Female severities in Total severities per day")
grid.arrange(p9, p10, nrow = 2, p3, top = "Male vs Female injuries in Total injuries per day")

#Correlation matrix and network
corMat <- cor(day_data[, -2])
print(corMat, 2)

library(GGally)
ggcorr(data = NULL, cor_matrix = res, label = TRUE, label_size = 3, hjust = 0.75, size = 4, 
       nbreaks = 10, name = "Correlation Coefficients", geom = "circle", min_size = 7, max_size = 12)

library(igraph)
library(qgraph)
library(RXKCD)
Groups <- c(rep("Year",1),rep("Fatalities",4),rep("Severities",4),rep("Injuries",4),rep("Non-injuries",1))
Graph_pcor <- qgraph(corMat, graph = "pcor", layout = "spring", usePCH = TRUE, palette = "colorblind", vsize = 10,
                     nodeNames = colnames(day_data[,-2]), details = TRUE, groups = Groups, theme = "classic",
                     legend.cex = 0.4, posCol = "red", negCol = "green", borders = TRUE, title = "Correlation network",
                     negDashed = TRUE, aspect = TRUE)

