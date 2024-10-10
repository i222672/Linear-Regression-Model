# Importing necessary libraries

install.packages("datasets")
install.packages("ggplot.multistats")

library(readr)
library(dplyr)
library(ggplot.multistats)
library(lubridate)
library(zoo)
library(datasets)


# Importing Dataset (ELBASAN)

elbasan_data_1 <- read_csv('C:/Users/User/Downloads/FASTNU Semester 4/P&S/archive(1)/archive (1)/data_weather/vlore/vlore2-021.csv')
dim(elbasan_data_1)
elbasan_data_1
names(elbasan_data_1)



#function for computing the mode
find_mode<=function(x){
  alpha<-unique(beta)
  alpha[which.max(tabulate(match(beta,alpha)))]
}


#Task 1 displaying summary for elbasan in
summary(elbasan_data_1);

#getting the mode
prcp_mode<-find_mode(elbasan_data_1$prcp)
tmax_mode<-find_mode(elbasan_data_1$tmax)
tmin_mode<-find_mode(elbasan_data_1$tmin)
wdir_mode<-find_mode(elbasan_data_1$wdir)
wspd_mode<-find_mode(elbasan_data_1$wspd)
pres_mode<-find_mode(elbasan_data_1$pres)

prcp_mode
tmax_mode
tmin_mode
wdir_mode
wspd_mode
pres_mode

# Imputing the null values in the dataset using mean strategy
elbasan_data_1[is.na(elbasan_data_1)] <- lapply(elbasan_data_1, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))


# Converting the date column's data type into date from character
elbasan_data_1$date <- as.Date(elbasan_data_1$date)

# Linear Regression Model
MLM <- lm(prcp ~ tmin + tmax + wdir + wspd + pres, data = elbasan_data_1)
summary(MLM)







#boxplot for precipitation with all other variables
par(mfrow=c(2,3))
boxplot(prcp~tmax,data=elbasan_data_1,main="Precipitation(mm) wrt Max Temperaturhttp://127.0.0.1:21715/graphics/a09c2939-3dd5-41b4-9362-c26f69c972da.pnge(`C)",col="green3")
boxplot(prcp~wspd,data=elbasan_data_1,main="Precipitation(mm) wrt Wind Speed(km/h)",col="antiquewhite1")
boxplot(prcp~tmin,data=elbasan_data_1,main="Precipitation(mm) wrt Min Temperature(`C)",col="lightskyblue4")
boxplot(prcp~pres,data=elbasan_data_1,main="Precipitation(mm) wrt Atmospheric Pressure(Pascals)",col="purple3")




library(ggplot2)

ggplot(data = elbasan_data_1, aes(x = tmax, y = prcp)) +
  geom_point(color = "blue", shape = 1) +
  geom_smooth(method = 'lm') +
  labs(x ="Max Temperature (°C)" , y = "Precipitation (mm)") +
  ggtitle("Precipitation vs. Max Temperature")


ggplot(data = elbasan_data_1, aes(x = pres, y = tmax)) +
  geom_point(color = "blue", shape = 1) +
  geom_smooth(method = 'lm') +
  labs(y ="Max Temperature (°C)" , x = "presure (pascals)") +
  ggtitle("tmax vs. pres")




