# Importing necessary libraries

install.packages("datasets")
install.packages("ggplot2")
install.packages("readr")
install.packages("lubridate")



library(readr)
library(lubridate)
library(datasets)
library(ggplot2)

#    TASK 1  
# Importing Dataset (vlore)
vlore_data_1 <- read_csv('C:/Users/User/Downloads/FASTNU Semester 4/P&S/archive(1)/archive (1)/data_weather/vlore/vlore2021.csv')
#view all data
view(vlore_data_1)
#display number of rows
head(vlore_data_1)
#displaying dimensions
dim(vlore_data_1)
#getting the names of the colums
names(vlore_data_1)

#      TASK 2

summary(vlore_data_1)


# Filter out rows where prcp is zero
vlore_data_filtered <- vlore_data_1 %>%
  filter(prcp >= 1)

install.packages("ggplot2")

# Define a function to calculate the mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# Calculate the mode for specific variables
prcp_mode <- find_mode(vlore_data_filtered$prcp)
tmax_mode <- find_mode(vlore_data_1$tmax)
tmin_mode <- find_mode(vlore_data_1$tmin)
wdir_mode <- find_mode(vlore_data_1$wdir)
wspd_mode <- find_mode(vlore_data_1$wspd)
pres_mode <- find_mode(vlore_data_1$pres)

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

p <- ggplot(vlore_data_filtered, aes(x=factor(tmax),y=prcp))
p + geom_boxplot(fill = "green3") +
  labs(x = "Max Temperature (°C)", y = "Precipitation (mm)") +
  ggtitle("Precipitation vs. Max Temperature")



p<-ggplot(vlore_data_1,aes(x=factor(wspd),y=tmax))
p+geom_boxplot(fill="orange1")+
  labs(x="Wind Speed(km/h)",y="Max Temperature (°C)")+
  ggtitle("Max Temperature vs. Wind speed")

#boxplot
p<-ggplot(vlore_data_1,aes(x=factor(pres),y=wspd))
p+geom_boxplot((fill="red"))+
  labs(x="Pressure (Pascals)",y="Wind Speed (km/h)")+
  ggtitle("Wind Speed vs. Pressure (Pascals)")

#boxplot tmax and prcp
ggplot(vlore_data_filtered, aes(x = factor(tmax), y = prcp)) +
  geom_boxplot(fill = "green3") +
  labs(x = "Max Temperature (°C)", y = "Precipitation (mm)") +
  ggtitle("Precipitation vs. Max Temperature")

boxplot(prcp~pres,data=vlore_data_filtered,main="Precipitation by Pressure",col="pink")
boxplot(tmax~wspd,data=vlore_data_filtered,main="Precipitation by Pressure",col="violet")
boxplot(prcp~wspd,data=vlore_data_filtered,main="Precipitation(mm) vs Wspd(km/h",col="red4")

# Scatterplot with excluded rows where prcp is zero
ggplot(data = vlore_data_filtered, aes(x = tmax, y = prcp)) +
  geom_point(color = "blue", shape = 1) +
  geom_smooth(method = 'lm') +
  labs(x = "Max Temperature (°C)", y = "Precipitation (mm)") +
  ggtitle("Precipitation vs. Max Temperature")

#prcp vs. wspd
ggplot(data=vlore_data_filtered,aes(x=wspd,y=prcp))+
  geom_point(color="blue",shape=1)+
  geom_smooth(method='lm')+
  labs(y="Precipitation (mm)",x=" Wind Speed (km/h)")+
ggtitle("Precipitation (mm) vs. Wind Speed (km/h)")

#prcp vs. pres scatterplot
ggplot(data=vlore_data_filtered,aes(x=pres,y=prcp))+
  geom_point(color="red",shape=1)+
  geom_smooth(method='lm')+
  labs(x="Atmospheric Pressure (Pascals)",y="Precipitation (mm)")+
  ggtitle("Precipitation (mm) vs. Pressure (Pascals)")

boxplot(vlore_data_filtered$prcp)

  
#TASK 4 run a regression model with 2 independent variables

# Linear Regression Model
MLM <- lm(prcp ~tmax + wspd, data = vlore_data_filtered)
summary(MLM)

#TASK 5 run a egresson model with 3 or more independent variables
MLM<-lm(prcp~tmax+wspd+wdir+pres+tmin,data=vlore_data_filtered)
#print the summary
summary(MLM)
  
SLM<-lm(prcp~wspd,data=vlore_data_filtered)
summary(SLM)
cor_co<-cor(vlore_data_filtered$prcp,vlore_data_filtered$wspd)
print(cor_co)

SLM<-lm(prcp~pres,data=vlore_data_filtered)
summary(SLM)
cor_co<-cor(vlore_data_filtered$prcp,vlore_data_filtered$pres)
print(cor_co)



# Select the variables for the scatter plot matrix
selected_vars <- vlore_data_filtered[, c("prcp", "wspd", "pres", "tmax")]

# Create the scatter plot matrix
pairs(selected_vars,col="purple4")

