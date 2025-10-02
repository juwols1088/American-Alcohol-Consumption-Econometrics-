library(tidyverse)
library(mdsr)
library(caret)
library(readr)

#unemployment data
UNRATE <- read_csv("C:/Users/jolie/Downloads/UNRATE.csv")
View(UNRATE)


buyalc <- read_csv("C:/Users/jolie/Desktop/Econometrics folda/PROJECT DATA FILES/buyalc.csv")
#View(buyalc)
ggplot(aes(x = ALCBEVCQ), data = buyalc)+geom_histogram(bins = 300)
summary(buyalc$ALCBEVCQ)
cpitime <- read_csv("C:/Users/jolie/Desktop/Econometrics folda/PROJECT DATA FILES/time files/cpitime.csv")
#(cpitime)
cpitime$year<-as.numeric(cpitime$year)


#buyalcd table has been adjusted for date and dummy variable
buyalcd = buyalc %>% 
  left_join(cpitime, join_by(YEAR == year, QUARTER == quarter)) %>% 
  mutate(dummyALC = ifelse(ALCBEVCQ > 0, 1, 0)) %>% 
  mutate(dummybuy = ifelse(ALCBEVCQ > 20, 1, 0))

summary(buyalc$ALCBEVCQ)
#View(buyalcd)


salary <- read_csv("C:/Users/jolie/Desktop/Econometrics folda/PROJECT DATA FILES/salary.csv")
#View(salary)


salaryd = salary %>% 
  left_join(cpitime, join_by(YEAR == year, QUARTER == quarter) )
#View(salaryd)
#load in age
age <- read_csv("C:/Users/jolie/Desktop/Econometrics folda/PROJECT DATA FILES/age.csv")
#View(age)
aged = age %>% 
  left_join(cpitime, join_by(YEAR == year, QUARTER == quarter))
#View(aged)
  



#load in cpi data

CPISTUFF <- read_csv("C:/Users/jolie/Desktop/Econometrics folda/CPISTUFF.csv")
View(CPISTUFF)
CPISTUFF = CPISTUFF %>% 
  rename(cpi = CUSR0000SAF116)
View(CPISTUFF)

#load in sexmarital
sexmarital <- read_csv("C:/Users/jolie/Desktop/Econometrics folda/PROJECT DATA FILES/sexmarital.csv")
#View(sexmarital)

smd = sexmarital %>% 
  left_join(cpitime, join_by(YEAR == year, QUARTER == quarter)) %>% 
  mutate(dummysex = ifelse(SEX_REF >= 2, 1, 0)) %>% 
  mutate(dummymar = ifelse(FAM_TYPE > 4, 1, 0)) %>%
  rename(goaway = SEX_REF) %>% 
  select(NEWID,date, dummysex, dummymar)#mary = 1-4->0, not married=5-8 -> 1 #edit it 
#FEMALE = 2 -> 1
  #MALE = 1 -> 0
#View(smd)
#Load in raceemp
raceemp <- read_csv("C:/Users/jolie/Desktop/Econometrics folda/PROJECT DATA FILES/raceemp.csv")
View(raceemp)
red = raceemp %>% 
  left_join(cpitime, join_by(YEAR == year, QUARTER == quarter))

class(red$MEMBRACE)
red$MEMBRACE<-as.character(red$MEMBRACE)
class(red$MEMBRACE)
library(caret)

#define one-hot encoding function
dummy <- dummyVars(~MEMBRACE + NEWID, data=red)

#perform one-hot encoding on data frame
finalred <- data.frame(predict(dummy, newdata=red))

#view final data frame
View(finalred)
#load certainty
CERTAINTY <- read_csv("C:/Users/jolie/Desktop/Econometrics folda/PROJECT DATA FILES/CERTAINTY.csv")
View(CERTAINTY)

#joining the data all together:
cool = buyalcd %>% 
  full_join(salaryd, join_by(NEWID==NEWID)) %>% 
  full_join(aged, join_by(NEWID==NEWID)) %>% 
  left_join(CPISTUFF, join_by(date.x == DATE)) %>% 
  left_join(CERTAINTY, join_by(date.x == DATE)) %>% 
  full_join(finalred, join_by(NEWID==NEWID)) %>% 
  left_join(smd, join_by(NEWID==NEWID)) %>% 
  left_join(UNRATE, join_by(date.x == DATE)) %>% 
  select(NEWID,dummybuy,date.x,FSALARYM,VALUE,cpi,dummyALC, USEPUINDXD, `dummysex`, dummymar,MEMBRACE1,MEMBRACE2,MEMBRACE3,MEMBRACE4,MEMBRACE5, MEMBRACE6, dummybuy,UNRATE)

#Viewing combined data 
View(cool)

#Editing the data frame - no duplicates or missing variables 
cool[!duplicated(cool), ]
cool$dater <- ifelse(cool$date.x > as.Date("2020-01-01"), 1, 0)
cool<-na.omit(cool)
cool = na.omit(cool)
View(cool)

#Preparing to train the model
set.seed(412024) #set seed
train.index <- createDataPartition(cool$dummyALC, p= 0.005, list = FALSE) 
train <- cool[train.index,]
secondpartition <- cool[-train.index,]
train2.index<- createDataPartition(secondpartition$dummyALC, p = 0.001, list = FALSE)
test<- secondpartition[train2.index,]
nodata<- secondpartition[-train2.index,]

#create logistic regression 
logistic = glm(data = train, dummyALC~VALUE++FSALARYM+dummysex+dummymar+MEMBRACE2+MEMBRACE3+MEMBRACE4+MEMBRACE5+MEMBRACE6 dummybuy+dater, family = "binomial")
LOGISTIC = glm(data = train, dummyALC~VALUE+FSALARYM+cpi+USEPUINDXD+dummysex+dummymar+MEMBRACE2+MEMBRACE3+MEMBRACE4+MEMBRACE5+MEMBRACE6+ dummybuy+dater, family = "binomial")
summary(logistic)
summary(LOGISTIC)
#checking for residuals 
residuals = residuals(LOGISTIC)
plot_data <- data.frame(
  Fitted = fitted(LOGISTIC),
  Residuals = residuals(LOGISTIC)
)

# Plot
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
cor(train)

library(car)
vif(LOGISTIC)
library(bstats)

#exporting a table
csv_filename <- "logisticregressiontime!econometrics.R"
write.csv(train, "revised.csv", row.names = FALSE)

#looking at correlations between variables specifically CPI and Unemployment rate
cor(train$cpi,train$UNRATE)

# Install Hmisc if not already installed
install.packages("Hmisc")

# Load Hmisc
library(Hmisc)

# Compute pairwise correlations
result <- rcorr(train$cpi, train$UNRATE)
View(result)
summary(result)

correlation_matrix <- result$r   # Correlation coefficients
p_value_matrix <- result$P       # P-values

print(p_value_matrix)
##we can see that only the variables that are correlated with each other are 
##inherently correlated because of the nature of the economy (like unemployment rate and CPI)

#Plotting Accuracy & Creating a confusion matric
testresponse = test$dummyALC
library(caret)
predicted<- predict(LOGISTIC, test, type = "response")
View(predicted)
predicted <- unlist(predicted)
predicted <- ifelse(predicted>0.7, 1, 0) 
#if the predicted probability is higher  than 0.7 then 
#it is a "Will buy alcohol", if not it is "will not buy alcohol"
confusionMatrix(test$dummyALC, predicted)
test$dummyALC <- as.factor(test$dummyALC)
predicted <- as.factor(predicted)

#constraining the logistic regression to see if CPI and Unexpectancy index is important to the model
constrainedlogistic = glm(data = train, dummyALC~VALUE+FSALARYM+cpi+USEPUINDXD+dummysex+dummymar+ dummybuy+dater, family = "binomial")
constrainedlogistic2 = glm(data = train,dummyALC~VALUE+FSALARYM+dummysex+dummymar+ dummybuy+dater )
library(lmtest)
lrtest(logistic, constrainedlogistic2)
lrtest(LOGISTIC, constrainedlogistic)
View(train)
##Although CPI and UNexpectancy is not statistically signficant,
##the inclusion of these variables DO impact the outcome

#Viewing age squared to see if age follows a non-linear pattern
trainsquared =  train %>% 
  mutate(age_squared = VALUE^2)
View(trainsquared)

logisticsquared =glm(data = trainsquared, dummyALC~VALUE+FSALARYM+cpi+USEPUINDXD+dummysex+dummymar+age_squared+MEMBRACE1+MEMBRACE2+MEMBRACE3+MEMBRACE4+MEMBRACE5+ dummybuy+dater, family = "binomial") 
summary(logisticsquared)


