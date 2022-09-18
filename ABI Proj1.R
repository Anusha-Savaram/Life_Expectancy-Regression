library(XML)

url1<- "/Users/anusha._s/Desktop/Rutgers/Sem 1/ABI/Project 1/Life Expectancy by Country and in the World (2021) - Worldometer.html"
LifeExpec<- readHTMLTable(url1, which = 2, stringsAsFactors = FALSE,colClasses =c("numeric","character",rep("numeric",5)))
View(LifeExpec)
url2<- "/Users/anusha._s/Desktop/Rutgers/Sem 1/ABI/Project 1/Population by Country (2021) - Worldometer.html"
Population<-readHTMLTable(url2, which = 1, stringsAsFactors = FALSE, coClasses= c("numeric","character", rep("numeric",10)))
View(Population)


####### Multiple Regression#####
x<- LifeExpec$`Females  Life Expectancy `+LifeExpec$`Males  Life Expectancy`
model<-lm(LifeExpec$`Life Expectancy  (both sexes) `~x)
summary(model)
plot(LifeExpec$`Life Expectancy  (both sexes) `~x, col='blue')
abline(model,lwd=3,col="red")


########Simple Linear Regression######
model_simple=lm(LifeExpec$`Life Expectancy  (both sexes) `~LifeExpec$`Females  Life Expectancy `)
summary(model_simple)
plot(LifeExpec$`Life Expectancy  (both sexes) `~LifeExpec$`Females  Life Expectancy `)
abline(model_simple,lwd=3,col="red")

/*Simple Regression model- machine prediction*/
MachinePredictedVariable<-predict(model_simple)
simple_original<-LifeExpec$`Life Expectancy  (both sexes) `
plot(MachinePredictedVariable~simple_original)


install.packages("fpp2")
library(fpp2)
accuracy(MachinePredictedVariable, simple_original)

install.packages("Metrics")
library(Metrics)
rmse(simple_original,MachinePredictedVariable)
mae(simple_original,MachinePredictedVariable)
mape(simple_original,MachinePredictedVariable)

########Multiple Linear Regression#########
/*Multiple Regression model- machine prediction*/
MultipleMachinePredictedVariable<-predict(model)
Multiple_original<-LifeExpec$`Life Expectancy  (both sexes) `
plot(MultipleMachinePredictedVariable~Multiple_original)
accuracy(Multiple_original,MultipleMachinePredictedVariable)

######Boston #########

numberOfNA <- length(which(is.na(Boston)==T))
if(numberOfNA>0) {
  Boston <- Boston[complete.cases(Boston),]
}

str(Boston)
//chas is factor. We turn it into numeric
Boston$chas<-as.numeric(Boston$chas)

pairs(~ medv + ptratio + black + lstat + dis + rm + crim, data = Boston, main = "Boston Data")

########Preparing Boston Dataset######
train<-Boston[1:253,]
test<-Boston[254:506,]

/*correlation plot for medv and other attributes in Boston*/
install.packages("corrplot")
library(corrplot)
library(MASS)
library(lattice) #for visualisation
library(ggplot2) #for visualisation
install.packages("caTools")
library(caTools) #for splittind data into testing and training data
library(dplyr) #manipulating dataframe
install.packages("plotly")
library(plotly) #converting ggplot to plotly
corrplot(cor(Boston))
corrplot(cor(as.numeric(Boston)))
View(Boston)

summary(Boston)

/*Correlation Plot 2*/
  library("GGally")
ggpairs(Boston[,-1])

/*Box Plot*/
par(mfrow = c(1, 4))
boxplot(Boston$crim, main='crim',col='Sky Blue') 
boxplot(Boston$zn, main='Residential Land zones',col='Sky Blue')
boxplot(Boston$rm, main='Average Number of rooms per dwelling',col='Sky Blue')
boxplot(Boston$black, main='Proportion of blacks in town Age',col='Sky Blue')


/*Multiple Regression*/
  
Iteration 1
Boston_Multiple<-lm(medv~lstat+rm,data=train)
summary(Boston_Multiple)
test$pred_1=predict(Boston_Multiple,newdata = test)

library(plotly)
pl1 <-test %>% 
  ggplot(aes(medv,pred_1)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of medv') +
  ylab('Predicted value of medv') +
  theme_bw()
ggplotly(pl1)

accuracy(test$medv,test$pred_1)

/residuals

plot(Boston_Multiple)
Boston1_residual <- data.frame('Residuals' = Boston_Multiple$residuals)
ggplot(Boston1_residual, aes(x=Residuals)) + geom_histogram(color='black', fill='skyblue') + ggtitle('Histogram of Residuals')

View(test)

train1<-train

Iteration 2

Boston_Multiple2 <- lm(medv~rm*lstat,data=train)
summary(Boston_Multiple2)

test$pred_2=predict(Boston_Multiple2,newdata = test)

library(plotly)
pl1 <-test %>% 
  ggplot(aes(medv,pred_2)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of medv') +
  ylab('Predicted value of medv') +
  theme_bw()
ggplotly(pl1)

accuracy(test$medv,test$pred_2)

Boston2_residuals <- data.frame('Residuals' = Boston_Multiple2$residuals)
ggplot(Boston2_residuals, aes(x=Residuals)) + geom_histogram(color='black', fill='skyblue') + ggtitle('Histogram of Residuals')

plot(Boston_Multiple2)
Iteration 3

Boston_Multiple3<-lm(medv~.-indus-age-zn+rm*lstat-black+lstat*rad, data=train)
summary(Boston_Multiple3)
test$pred_3=predict(Boston_Multiple3,newdata = test)

library(plotly)
pl1 <-test %>% 
  ggplot(aes(medv,pred_3)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of medv') +
  ylab('Predicted value of medv') +
  theme_bw()
ggplotly(pl1)

accuracy(test$medv, test$pred_3)
plot(Boston_Multiple3)

/*Studying Residuals*/
Boston3_residuals <- data.frame('Residuals' = Boston_Multiple3$residuals)
ggplot(Boston3_residuals, aes(x=Residuals)) + geom_histogram(color='black', fill='skyblue') + ggtitle('Histogram of Residuals')


par(mfrow = c(1, 1))
/*Life Expectancy Problem 1, Attempt 2*/
  
/*We are preapring the tables to merge*/
  
str(LifeExpec)
Population<-Life_Expectancy_per_Country_
str(Population)
//Population[,-1]=as.numeric(Population[,-1]) 
View(LifeExpec) 
//We are removing Hash column in LifeExpec and Population
LifeExpec<-LifeExpec[,-1]
Population<-Population[-1,]
Population<-Population[,-1]
View(Population)
/*Column name is changed to country from Country(or dependency) in the population table*/
colnames(Population)[1]<-"Country"

/*Merging Life Expectancy and Population Table*/
Population_Story<-merge(LifeExpec,Population,by="Country")
View(Population_Story)

/*Preparing Dataset for analysis*/
train<-Population_Story[1:97,]
test<-Population_Story[98:195,]
str(Population_Story)

/*Correlation of Population Story
corrplot(cor(Population_Story[,-1]))

/*Iteration 1*/
Population_Multiple<-lm(Life_Expectancy_Both_Sexes~Female_Life_Expectancy+Male_Life_Expectancy, data = train)
View(train)
summary(Population_Multiple)
test$pred_pop=predict(Population_Multiple,newdata = test)
library(plotly)
pl1 <-test %>% 
  ggplot(aes(Life_Expectancy_Both_Sexes,pred_pop)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of Life Expectancy  (both sexes)') +
  ylab('Predicted value of Life Expectancy  (both sexes)') +
  theme_bw()
ggplotly(pl1)

View(train)
colnames(train)[2]<-"Life_Expectancy_Both_Sexes"
colnames(test)[2]<-"Life_Expectancy_Both_Sexes"
colnames(train)[3]<-"Female_Life_Expectancy"
colnames(test)[3]<-"Female_Life_Expectancy"
colnames(train)[4]<-"Male_Life_Expectancy"
colnames(test)[4]<-"Male_Life_Expectancy"

Iterration 2

is.na(Population)

numberOfNA <- length(which(is.na(Population_Story)==T))
if(numberOfNA>0) {
  Population_Story <- Population_Story[complete.cases(Population_Story),]
}

Population_Multiple2<-lm(Life_Expectancy_Both_Sexes~Population-Migrants+Fert.+Density,data=train)
summary(Population_Multiple2)

accuracy(test$Life_Expectancy_Both_Sexes, test$pred_pop2)

Population2_residuals <- data.frame('Residuals' = Population_Multiple2$residuals)
ggplot(Population2_residuals, aes(x=Residuals)) + geom_histogram(color='black', fill='skyblue') + ggtitle('Histogram of Residuals')

par(mfrow = c(1, 1))
plot(Population_Multiple2)

test$pred_pop2=predict(Population_Multiple2,newdata = test)
library(plotly)
pl1 <-test %>% 
  ggplot(aes(Life_Expectancy_Both_Sexes,pred_pop2)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of Life Expectancy  (both sexes)') +
  ylab('Predicted value of Life Expectancy  (both sexes)') +
  theme_bw()
ggplotly(pl1)


Iteration 3
Population_Multiple3<-lm(Life_Expectancy_Both_Sexes~Population+Migrants+Fert.+Med.+Population*Fert.,data=train)
summary(Population_Multiple3)
summary(Population_Story)
test$pred_pop3=predict(Population_Multiple3,newdata = test)
library(plotly)
pl1 <-test %>% 
  ggplot(aes(Life_Expectancy_Both_Sexes,pred_pop3)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of Life Expectancy  (both sexes)') +
  ylab('Predicted value of Life Expectancy  (both sexes)') +
  theme_bw()
ggplotly(pl1)

plot(Population_Multiple3)


accuracy(test$Life_Expectancy_Both_Sexes, test$pred_pop3)

par(mfrow = c(1, 4))
boxplot(Population_Story$Density, main='Density',col='Sky Blue') 
boxplot(Population_Story$Population, main='Population',col='Sky Blue')
boxplot(Population_Story$Fert., main='Fertility',col='Sky Blue')
boxplot(Population_Story$Med., main='Median Age',col='Sky Blue')


summary(Population_Story)

Population3_residuals <- data.frame('Residuals' = Population_Multiple3$residuals)
ggplot(Population3_residuals, aes(x=Residuals)) + geom_histogram(color='black', fill='skyblue') + ggtitle('Histogram of Residuals')


/Residual plot for iteration 3*/


Iteration 4
Population_Multiple4<-lm(Life_Expectancy_Both_Sexes~Population*Migrants+Fert.*Med.*Population*Fert.,data=train)
summary(Population_Multiple4)
test$pred_pop4=predict(Population_Multiple4,newdata = test)
library(plotly)
pl1 <-test %>% 
  ggplot(aes(Life_Expectancy_Both_Sexes,pred_pop4)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of Life Expectancy  (both sexes)') +
  ylab('Predicted value of Life Expectancy  (both sexes)') +
  theme_bw()
ggplotly(pl1)
accuracy(test$Life_Expectancy_Both_Sexes, test$pred_pop4)

Iteration 5

Population_Multiple5<-lm(Life_Expectancy_Both_Sexes~Population*Migrants*Fert.*Med.,data=train)
summary(Population_Multiple5)

library(fpp2)
accuracy(test$Life_Expectancy_Both_Sexes, test$pred_pop5)

test$pred_pop5=predict(Population_Multiple5,newdata = test)
library(plotly)
pl1 <-test %>% 
  ggplot(aes(Life_Expectancy_Both_Sexes,pred_pop5)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of Life Expectancy  (both sexes)') +
  ylab('Predicted value of Life Expectancy  (both sexes)') +
  theme_bw()
ggplotly(pl1)

Iteration 6
x<-log(train$Migrants*train$Fert.*train$Med.)
Population_Multiple6<-lm(Life_Expectancy_Both_Sexes~Population*x,data=train)

library("GGally")
ggpairs(Population_Story[,-1])


sum(is.na(Boston))