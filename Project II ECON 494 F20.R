################################################
#### DESCRIPTIVE ANALYTICS PROJECT FA2020"######
################################################

###IMPORT DATASET####
housing <- read.csv("Downloads/housing.csv")
install.packages("dplyr")
library(dplyr)
library(plyr)
install.packages("reshape")
library(reshape2)
library(ggplot2)

###PREPROCESSING####
View(housing)
head(housing)
tail(housing)
summary(housing) ##check for any NA's and confirm numeric variables and categorical variables have proper outputs
##Median and mean for house value are different so indicates skewness (outliers)
class(housing) ##data.frame 
class(housing$median_house_value) ##numeric 
class(housing$ocean_proximity) ##factor, confirms that this is a categorical variable
length(levels(housing$ocean_proximity)) ###lists the number of classes of categorical variable, 5 elements 
plot(housing$population,housing$households) ## 2 outliers
plot(housing$total_rooms,housing$total_bedrooms) 
plot(housing$total_rooms,housing$population)
plot(housing$median_house_value,housing$median_income,col=factor(housing$ocean_proximity))
hist(housing$population)
hist(housing$median_house_value,
     main="Median House Value",
     xlab = "Median House Value, in USD",
     ylab = "Count",
   ) ###figure 3, outliers cut off at 500000
hist(housing$housing_median_age) ### farily normally distrubuted 
hist(housing$households) ##indicates outliers, cut off at 2000
hist(housing$median_income) ###indicates outliers, cut off at 10
hist(housing$housing_median_age)
hist(housing$median_house_value)###mean house value is $206,855.80
mean(housing$housing_median_age)###mean house age is 28.64 years
mean(housing$median_income)### mean income is $38706.71
install.packages('tseries') #installs the tseries package if not already installed
library(tseries) #loads "tseries" library - need to first install "tseries" package
#conducts a hypothesis test for normality called the Jarque-Bera test
jarque.bera.test(housing$median_house_value)#null hypothesis: data is distributed normally
ggplot(data = melt(housing), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')
sns.set()
housing %>% ggplot(aes(x = ocean_proximity)) +
  geom_bar() #histogram of number of data points for each location
class(housing$ocean_proximity) ##FACTOR VARIABLE
ggplot(housing, aes(median_house_value, ocean_proximity)) +
  geom_violin(aes(fill=ocean_proximity)) ###REALLY GOOD VIOLIN PLOT!!!####

ggplot(housing, aes(median_house_value, fill = ocean_proximity)) +
  geom_histogram(aes(y=..density..), position = "identity", binwidth=100000) +
  facet_wrap(~ocean_proximity, nrow = 1) ##REALLY GOOD HISTOGRAM!!###


########################
####CLEANING PROCESS####
########################

###Deletion
dim(housing)
housing_deletion<-subset(housing,population<= 5500 & households <=2000 & median_house_value <=500000 & median_income <=10)
(dim(housing)[1]-dim(housing_deletion)[1])/dim(housing)[1] ##lost 6.21% of data 
plot(housing_deletion$population,housing_deletion$households)
plot(housing_deletion$median_house_value,housing_deletion$median_income)
housing_deletion$total_bedrooms[is.na(housing_deletion$total_bedrooms)] = median(housing_deletion$total_bedrooms , na.rm = TRUE) ##removes NAs from total bedrooms variable
summary(housing_deletion)
dim(housing_deletion)
housing_deletion$mean_bedrooms = housing_deletion$total_bedrooms/housing_deletion$households ###changes the total bedrooms to average number of bedrooms 
housing_deletion$mean_rooms = housing_deletion$total_rooms/housing_deletion$households ###changes total rooms to average number of rooms 
drops = c('total_bedrooms', 'total_rooms') 
housing_deletion = housing_deletion[ , !(names(housing_deletion) %in% drops)]
plot(housing_deletion$mean_rooms,housing_deletion$mean_bedrooms) ###indicates a few outliers
housing_deletion1<-subset(housing_deletion,mean_rooms<=11 & mean_bedrooms<=2.5 & mean_bedrooms>=0.5) ##cleaned outliers from mean rooms and mean bedrooms
plot(housing_deletion1$mean_rooms,housing_deletion1$mean_bedrooms) ##no correlation 
ggplot(housing_deletion1, aes(mean_rooms)) +
  geom_histogram(aes(y=..density..), position = "identity", binwidth = 0.5)+
  geom_density() ###follows normal distribution
(dim(housing)[1]-dim(housing_deletion1)[1])/dim(housing)[1] ##total 7.31% data lost after deletion
dim(housing_deletion1)
summary(housing_deletion1)
summary(housing)

##CREATING A NEW BINARY VARIABLE CALLED "LOCATION" FOR OCEAN PROXIMITY 
housing_deletion1$location <- NA #INITIALIZE NEW COLUMN (YOU WILL GET A WARNING OTHERWISE)
for (i in 1:length(housing_deletion1$ocean_proximity)) {
  if (housing_deletion1$ocean_proximity[i]=='INLAND') {
    housing_deletion1$location[i] <- "INLAND"
  } else {
    if (housing_deletion1$ocean_proximity[i]=='ISLAND') {
      housing_deletion1$location[i] <- "ISLAND"
    }else {
        if (housing_deletion1$ocean_proximity[i]=='<1H OCEAN') {
          housing_deletion1$location[i] <- "<1H OCEAN"
        } else {
        if (housing_deletion1$ocean_proximity[i]=='NEAR BAY') {
          housing_deletion1$location[i] <- "NEAR BAY"
        } else { housing_deletion1$location[i] <- "NEAR OCEAN"}
          }
        }
      }
 
  }
View(housing_deletion1)
head(housing_deletion1)


colnames(housing_deletion1)
drops1 = c('ocean_proximity')
housing_cleaned<- housing_deletion1[ , !(names(housing_deletion1) %in% drops1)]
head(housing_cleaned)
write.csv(housing_cleaned,"TIDY_Housing_Dataset.csv") ###exports tidy dataset to computer 

summary(housing_deletion1)
summary(housing_cleaned)

###EXPLORATORY PROCESS####
install.packages('ggplot2')
library(ggplot2)
library(plyr)
class(housing_cleaned1$location) ##character 
housing_cleaned$location<- as.factor(housing_cleaned$location)
class(housing_cleaned$location) ##successfully converted to factor 


###PLOT A- scatterplot of median house value and income colored by ocean proximity
plot_a<-ggplot(housing_cleaned, aes(median_house_value, median_income)) +
  geom_point(aes(colour = location)) + 
  xlab('Median House Value (within a housing block)') + 
  ylab('Median Income (within a housing block)') +
  ggtitle('Relationship Between House Value and Income')  
plot_a


###PLOT B- density curve histogram for population
plot_b<-ggplot(housing_cleaned, aes(population)) +
  geom_histogram(aes(y=..density..), position = "identity", binwidth = 100) + 
  geom_density(alpha=.5) +
  ggtitle('Density Curve Histogram for Population')
plot_b

###PLOT C- creating pie chart of population based on ocean proximity 
library(scales)

plot_c<- ggplot(housing_cleaned, aes(x="", y=population, fill=location)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  ggtitle('Population Colored By Location')
plot_c

###PLOT D- scatterplot between rooms and bedrooms (expect them to be positively correlated)
plot_d<-ggplot(housing_cleaned, aes(mean_rooms)) +
  geom_histogram(aes(y=..density..), position = "identity", binwidth = 0.5) + 
  geom_density(alpha=.5) +
  ggtitle('Density Curve Histogram for Average Rooms')
plot_d
jarque.bera.test(housing_cleaned$mean_rooms)
#### mean rooms is normally distributed 



################################
######PREDICTIVE MODELING#######
################################

##Linear regressions on all variables##
LR1<-lm(median_house_value ~ median_income, housing_cleaned)
summary(LR1) 
LR2<-lm(median_house_value ~ housing_median_age, housing_cleaned)
summary(LR2)
LR3<-lm(median_house_value ~ ocean_proximity, housing_deletion1)
summary(LR3)
LR4<-lm(median_house_value ~ mean_rooms, housing_cleaned)
summary(LR4)
LR5<-lm(median_house_value ~ mean_bedrooms, housing_cleaned)
summary(LR5)
LR6<-lm(median_house_value ~ population, housing_cleaned)
summary(LR6)
LR7<-lm(median_house_value ~ households, housing_cleaned)
summary(LR7)
hist(LR1$residuals) ##histogram of residuals for LR1
jarque.bera.test(LR1$residuals) ### normally distributed, fail to reject hypothesis 
plot_e<-ggplot(housing_cleaned, aes(x = median_house_value, y = median_income)) + 
  geom_point() +
  geom_smooth(method ='lm')+
  ggtitle
plot_e
plot_f<-ggplot(housing_cleaned, aes(x = median_house_value, y = median_income, color = location)) + 
  geom_point() +
  geom_smooth(method ='lm')+
  ggtitle('Plot F: Residuals of Median House Value and Median Income, Colored by Location')
plot_f

#####PARTITIONING DATA#######
p<-0.7
obs_count<-dim(housing_cleaned)[1]
obs_count*.7
training_size <- (p * obs_count)
set.seed(1234)
train_ind <- sample(obs_count, size = training_size)
Training <- housing_cleaned[train_ind, ] ###13391 observations 12 variables 
View(train_ind)
Testing <- housing_cleaned[-train_ind, ] ###5739 observations 12 variables 
dim(Training)
dim(Testing)


######PLOTTING THE TRAINING AND TESTING PARTITIONS####
plot(median_house_value ~ median_income, Testing)
plot(median_house_value ~ median_income, Training)
points(Training$median_income, Training$median_house_value) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$median_income, Testing$median_house_value) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION


###BUILDING MODEL FROM TRAINING DATA ONLY#######
housing_cleaned$median_income2<-housing_cleaned$median_income^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
housing_cleaned$median_income3<-housing_cleaned$median_income^3 #CUBIC TRANSFORMATION (3rd ORDER)
housing_cleaned$ln_median_income<-log(housing_cleaned$median_income) #A LOGARITHMIC TRANSFORMATION OF MEDIAN INCOME
View(housing_cleaned)
M1<-lm(median_house_value ~ median_income, Training)
summary(M1)

#####GENERATING PREDICTIONS ON TRAINING DATA#######
PRED_1_IN <- predict(M1, Training) ##in-sample predictions for M1
View(PRED_1_IN)
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$median_house_value)^2)/length(PRED_1_IN)) ###computes in-sample error for M1
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$median_house_value)^2)/length(PRED_1_OUT)) ###computes out-of-sample error for M1
RMSE_1_IN ###$74,833.89 off from predicting median house value from median income 
RMSE_1_OUT ###$74,107.72 off from predicting median house value from median income 

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(median_income=x_grid))
plot(Training$median_house_value ~ Training$median_income, col='blue')
points(Testing$median_house_value ~ Testing$median_income, col='red', pch=3)
lines(x_grid, predictions, col='green', lwd=3) ###puts in line within data
#BUILDING THE QUADRATIC MODEL FROM THE TRAINING DATA
M2 <- lm(median_house_value ~ median_income + median_income2, Training)
summary(M2) #generates summary diagnostic output
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$median_income)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$median_income)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR $20,1737.7
RMSE_2_OUT #OUT-OF-SAMPLE ERROR $20,2591.8

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(median_income=x_grid, median_income2=x_grid^2))
plot(Training$median_house_value ~ Training$median_income, col='blue')

points(Testing$median_house_value ~ Testing$median_income, col='red', pch=3)
lines(x_grid, predictions, col='green', lwd=3)

#BUILDING THE CUBIC MODEL FROM THE TRAINING DATA, adding more variables to see if it will improve in/out sample displacement cubed 
M3 <- lm(median_house_value ~ median_income + median_income2 + median_income3, Training)
summary(M3) #generates summary diagnostic output,quardratic and linear effect is no longer significant but the cubic term is,
###r-squared increases slightly,adjusted r-squared.Model 3 better at fitting the data 

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$median_house_value)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$median_house_value)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M3, list(median_income=x_grid, median_income2=x_grid^2, median_income3=x_grid^3))
plot(Training$median_house_value ~ Training$median_income, col='blue')
points(Testing$median_house_value ~ Testing$median_income, col='red', pch=3)
lines(x_grid, predictions, col='green', lwd=3)

#BUILDING THE LOGARITHMIC MODEL FROM THE TRAINING DATA
M4 <- lm(median_house_value ~ ln_median_income, Training)
summary(M4) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$median_house_value)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$median_house_value)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M4, list(ln_median_income=log(x_grid)))
plot(Training$median_house_value ~ Training$median_income, col='blue')
points(Testing$median_house_value ~ Testing$median_income, col='red', pch=3)
lines(x_grid, predictions, col='green', lwd=3)
