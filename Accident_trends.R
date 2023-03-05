install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(dplyr)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("maps")
install.packages("mapproj")
library(maps)
library(mapproj)
idf <- read.csv("/Users/home/Desktop/US_Accidents_Dec21_updated.csv", header = TRUE)
str(idf)
sum(is.na(idf))# the dataset contains 3193068 missing values 
colSums(is.na(idf))#finding number of missing values in each column
colMeans(is.na(idf))*100# The column 'Number' has 61% missing data, while it is less than 20% for all other rows. Therefore dropping 'Number'.
drop <- c("Number")
idf = idf[,!(names(idf) %in% drop)]
colSums(is.na(idf))# successfully removed column 'Number'
#replacing missing values in the column with mean values 
idf$Visibility.mi.[is.na(idf$Visibility.mi.)] <- mean(idf$Visibility.mi.,na.rm = TRUE)
idf$Precipitation.in.[is.na(idf$Precipitation.in.)] <- mean(idf$Precipitation.in.,na.rm = TRUE)
idf$Humidity...[is.na(idf$Humidity...)] <- mean(idf$Humidity...,na.rm = TRUE)
idf$Temperature.F.[is.na(idf$Temperature.F.)] <- mean(idf$Temperature.F.,na.rm = TRUE)
idf$Pressure.in.[is.na(idf$Pressure.in.)] <- mean(idf$Pressure.in.,na.rm = TRUE)
idf$Wind_Speed.mph.[is.na(idf$Wind_Speed.mph.)] <- mean(idf$Wind_Speed.mph.,na.rm = TRUE)
idf$Wind_Chill.F.[is.na(idf$Wind_Chill.F.)] <- mean(idf$Wind_Chill.F.,na.rm = TRUE)
colSums(is.na(idf))# checking if any column has missing values. Successfully replaced missing values with mean of column

#######################################################################################
# Question 1: Geographical heat map to display severity of accidents based on states 
install.packages("usa")
library(usa)
data(states)
head(states)
colnames(idf)[colnames(idf) == "State"] = "abb" #Change column name 'State' to abb to match the column 'abb' in state package
data()
newdata <- merge(idf, states, by="abb")#merging dataframes states and idf based on common column name 'abb'
head(newdata)
mypalette <- brewer.pal(4,"Blues")
x <- newdata$Severity
newdata$colorBuckets <- as.numeric(cut(x , breaks = c(1,2,3,4)))
newdata$colorCode <- mypalette[newdata$colorBuckets]#Matching color bucket number to RGB color code
map(database="state", col=newdata$colorCode, fill= TRUE, resolution = 0)
leg.txt <- c("1-least severe","2","3","4-most severe")
legend ("bottomleft", legend= leg.txt, horiz= FALSE, fill= mypalette, cex = 0.5) #creating the legend 
title(main= "Severity of Accidents in the USA by state", font.main= 4,col.main= "darkblue")
# Montana and Alabama have the highest severity accidnets, followed by Idaho and Colorado. All the other states have the least severity 

#########################################################################################
#Question 2: Finding which top 10 cities in the USA which had the most number of accidents
install.packages("treemap")
library(treemap)
t1 <- idf %>% count(City) %>% top_n(10) # filtering only top 10 states with the highest accidents. Each time the city appears in the table, it represents an accident 
mp <- brewer.pal(10, "Set3")
treemap(t1, index = "City", vSize = "n", palette = mp, type = "index", title = "States with highest number of accidents")#creating the treemap
#It is found that none of the states with the highest incidence of accidents are NOT among states with high severity of accidents
#########################################################################################

# Question 3: Does severity(aka delay) change with other weather parameters? I am doing this my reviewing trends in average severity and other average weather paramemters according to state
keeps <- c("Severity","Wind_Speed.mph.","Humidity...","Pressure.in.","Precipitation.in.", "abb", "Visibility.mi.")#keeping only columns required for the heatmap
fdf <- idf[keeps]
str(fdf)
row.names(fdf) <- make.names(fdf$abb, unique = TRUE)
#fdf = subset(fdf,select = -c(Weather_Condition))
fdf1 <- aggregate(cbind(Severity,Wind_Speed.mph.,Humidity...,Pressure.in.,Precipitation.in., Visibility.mi.) ~ abb, data = fdf, FUN = mean, na.rm = TRUE)#seggregating columns based on states 
fdfo <- fdf1 %>% column_to_rownames(., var = 'abb')#making the first column as state abbreviation i.e 'abb'
fdfo <- fdfo[-1, ]#removing first row of data with unknown state value
matrix_new <- data.matrix(fdfo)#creating the matrix
view(matrix_new)
heatmap(matrix_new, Rowv= NA, Colv= NA, col = brewer.pal(4,'Blues'), scale = "column", main= "Trends in average severity and other weather paramemters depending according to state")
legend(x = "bottomright", legend = c("negligible","low", "medium", "high"),cex = 0.8, fill = colorRampPalette(brewer.pal(8, "Blues"))(4))
# High average severity accidents are accompanied by High speed winds, High precipitation, medium wind_speed and  low visibility. These weather conditions promote highest delaay/wait time in traffic

###########################################################################################
#Question 4: Plot of my Choice: How does distance affected by traffic vary with severity.What does the trend look like? How does Day/Night impact this?
new_labels1 <- c( "Night"= "Night time", "Day"= "Day time" )
ggplot(idf, aes(x = Severity, y= Distance.mi.)) + geom_bar(stat='identity') + facet_wrap(~ Sunrise_Sunset) + facet_wrap( ~Sunrise_Sunset, labeller = labeller(Sunrise_Sunset = new_labels1))#getting unknown value along with Night and Day
colSums(is.na(idf))#detected an unknown value apart from 'Night' and 'Day' for Sunrise_Sunset. Therefore removing values which are not 'Night' or 'Day'
frown <- filter(idf, Sunrise_Sunset == "Night"| Sunrise_Sunset == "Day")
new_labels1 <- c( "Night"= "Night time", "Day"= "Day time" )
ggplot(frown, aes(x = Severity, y= Distance.mi.)) + geom_bar(stat='identity') + facet_wrap(~ Sunrise_Sunset) + facet_wrap( ~Sunrise_Sunset, labeller = labeller(Sunrise_Sunset = new_labels1))
#The distance shows in exponential form because the miles are displayed in decimals. Since we want to observe the trend, it being in decimal form does not matter. 
#The accidents with severity 2 affect the largest distance of the road, especially when accidents happen in the day time.

###########################################################################################
#Question 5: How is distance affected by accident vary with visibility in miles. How does the trend look?How does severity and Day/Night fit into this picture?

myp <- ggplot(data = frown) + geom_jitter(aes(x= Visibility.mi. , y= Distance.mi., size= Severity, colour = Sunrise_Sunset, alpha=0.3))
myp <- myp + xlab("Visibility in miles")+ ylab("Distance in miles")
#It is observed that when the visibility is low the distance affected is higher as compared to higher visibility, irrespective of day or night and severity. Accidents with severity 2 and 3 have the highest affected distance when there is low visibility, especially during the day.
#When the Visibility is high, the distance affected is low, irrespective of severity. 







