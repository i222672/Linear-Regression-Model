# Importing necessary libraries

install.packages("datasets")
#install.packages("ggplot.multistats")
install.packages("readr")
install.packages("dplyr")
install.packages("zoo")
install.packages("lubridate")

library(readr)
library(dplyr)
#library(ggplot.multistats)
library(lubridate)
library(zoo)
library(datasets)

install.packages("ggplot2")
library(ggplot2)
#  TASK 1  
# Importing Dataset (elbasan)
elbasan_data_1 <- read_csv('elbasan2023[1].csv')  #idhr file the directory tho inverted commas mein ""
#view all data
view(elbasan_data_1)
#display number of rows
head(elbasan_data_1)
#displaying dimensions
dim(elbasan_data_1)
#getting the names of the colums
names(elbasan_data_1)

#      TASK 2

summary(elbasan_data_1)


# Filter out rows where prcp is zero
elbasan_data_filtered <- elbasan_data_1 %>%
  filter(prcp >= 1)


# Define a function to calculate the mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# Calculate the mode for specific variables
prcp_mode <- find_mode(elbasan_data_filtered$prcp)
tmax_mode <- find_mode(elbasan_data_1$tmax)
tmin_mode <- find_mode(elbasan_data_1$tmin)
wdir_mode <- find_mode(elbasan_data_1$wdir)
wspd_mode <- find_mode(elbasan_data_1$wspd)
pres_mode <- find_mode(elbasan_data_1$pres)

# Display the modes
cat("Mode of Precipitation:", prcp_mode, "\n")
cat("Mode of Max Temperature:", tmax_mode, "\n")
cat("Mode of Min Temperature:", tmin_mode, "\n")
cat("Mode of Wind Direction:", wdir_mode, "\n")
cat("Mode of Wind Speed:", wspd_mode, "\n")
cat("Mode of Atmospheric Pressure:", pres_mode, "\n")



#     TASK 3

# Updated box-and-whisker plots

#boxplots

p <- ggplot(elbasan_data_filtered, aes(x=factor(tmax),y=prcp))
p + geom_boxplot(fill = "green3") +
  labs(x = "Max Temperature (°C)", y = "Precipitation (mm)") +
  ggtitle("Precipitation vs. Max Temperature")



p<-ggplot(elbasan_data_1,aes(x=factor(wspd),y=tmax))
p+geom_boxplot(fill="orange1")+
  labs(x="Wind Speed(km/h)",y="Max Temperature (°C)")+
  ggtitle("Max Temperature vs. Wind speed")


p<-ggplot(elbasan_data_1,aes(x=factor(pres),y=wspd))
p+geom_boxplot((fill="red"))+
  labs(x="Pressure (Pascals)",y="Wind Speed (km/h)")+
  ggtitle("Wind Speed vs. Pressure (Pascals)")


ggplot(elbasan_data_filtered, aes(x = factor(tmax), y = prcp)) +
  geom_boxplot(fill = "green3") +
  labs(x = "Max Temperature (°C)", y = "Precipitation (mm)") +
  ggtitle("Precipitation vs. Max Temperature")


# Scatterplot with excluded rows where prcp is zero
ggplot(data = elbasan_data_filtered, aes(x = tmax, y = prcp)) +
  geom_point(color = "blue", shape = 1) +
  geom_smooth(method = 'lm') +
  labs(x = "Max Temperature (°C)", y = "Precipitation (mm)") +
  ggtitle("Precipitation vs. Max Temperature")

#prcp vs. wspd
ggplot(data=elbasan_data_filtered,aes(x=wspd,y=prcp))+
  geom_point(color="blue",shape=1)+
  geom_smooth(method='lm')+
  labs(y="Precipitation (mm)",x=" Wind Speed (km/h)")+
ggtitle("Precipitation (mm) vs. Wind Speed (km/h)")

#prcp vs. pres scatterplot
ggplot(data=elbasan_data_filtered,aes(x=pres,y=prcp))+
  geom_point(color="red",shape=1)+
  geom_smooth(method='lm')+
  labs(x="Atmospheric Pressure (Pascals)",y="Precipitation (mm)")+
  ggtitle("Precipitation (mm) vs. Pressure (Pascals)")


  
#TASK 4 run a regression model with 2 independent variables

# Linear Regression Model
MLM <- lm(prcp ~tmax + wsp, data = elbasan_data_1)
summary(MLM)

#TASK 5 run a regression model with 3 or more independent variables
MLM<-lm(prcp~tmax,wspd,widr,pres,tmin,data=elbasan_data_1)
#print the summary
summary(MLM)
  

boxplot(elbasan_data_filtered$prcp)

boxplot(elbasan_data_filtered$wspd) #for wind speed