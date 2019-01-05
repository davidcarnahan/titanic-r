##-----------------------------------------------------
## TITANIC REGRESSION ANALYSIS USING R
## Created By: David Carnahan
## Created On: 5 January 2018
## Last Modified: 5 January 2018
## Sprint: MDS 1
##-----------------------------------------------------


# pull dataset from url
library(RCurl)
url <- getURL("http://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")
titanic <- read.csv(textConnection(url), header=T)

# check structure of titanic dataset
str(titanic)
head(titanic)

# change variable names
names(titanic) <- c("survived", "class", "name", "sex", "age", "sibsp", "parchi", "fare")

# summary statistics
summary(titanic)

# create numerical dataframe for regression and summary stats
titanic$sex_cat <- ifelse(titanic$sex == 'female',1,0)
titanic2 <- titanic[ , c(1,2,5:8,12)]
str(titanic2)

# determine average survival, age, number of siblings/spouses, number of children, and average fare by class
library(dplyr)
titansum <- titanic2 %>% 
  group_by(class) %>% 
  summarise(mSurv = mean(survived), mAge = mean(age), mSibs = mean(sibsp), mParents = mean(parchi), mFare = mean(fare))
titansum

##----------------- univariate & bivariate factors -----------------##

# check for significance of sex and class
tbl <- table(titanic$sex, titanic$class); tbl
chisq.test(tbl)

# histogram of age and sex
hist(titanic$age)  # near normal distribution
hist(titanic$fare) # skewed rightward 

# violin plots of class and age
library(ggplot2)
titanic$fclass <- factor(titanic$class, labels = c("first", "second", "third"))
ggplot(titanic, aes(x = fclass, y = age)) + geom_violin() + xlab("Class")

# look at relationship of age and fare using scatterplot
plot(titanic$age, titanic$fare, main = 'Scatter Plot of Age and Fare')


##----------------- univariate factors x survival -----------------##

# check for significance of sex and survival
tb_sexsurv <- table(titanic$sex, titanic$survived); tb_sexsurv
chisq.test(tb_sexsurv)

# check for significance of sex and survival
tb_classsurv <- table(titanic$class, titanic$survived); tb_classsurv
chisq.test(tb_classsurv)


##----------------- bivariate factors x survival -----------------##

# violin plots of survival and age
titanic$fsurv <- factor(titanic$survived, labels = c("survived", "passed"))
ggplot(titanic, aes(x = fsurv, y = age)) + geom_violin() + xlab("Survival Status")

# violin plots of survival and age by sex
titanic$fsex <- factor(titanic$sex, labels = c("female", "male"))
ggplot(titanic, aes(x = fsurv, y = age)) + geom_violin() + xlab("Survival Status") + facet_grid(sex~.)

# proportions survival by class table
surv_tbl <- table(titanic[ , c(1,2)])
margin.table(surv_tbl, 2)
prop.table(surv_tbl, 2)

# look for correlation between factors for possible colinearity
cor(titanic2)

##----------------- regression analysis -----------------##

# all inclusive model
model = glm(survived ~.,family=binomial(link='logit'), data=titanic2)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# diagnostic plots
par(mfrow=c(2,2)) # Plot 4 plots in same screen
plot(model)

# drop number of parent/children variable and fare variable given not statistically signficant
model2 = glm(survived ~ class + age + sibsp + sex_cat,family=binomial(link='logit'), data=titanic2)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

# diagnostic plots
par(mfrow=c(2,2)) # Plot 4 plots in same screen
plot(model2)

# try different package -- RMS -- to get pseudoR2
# explanations found here: 
# https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression

library(rms)
model3 <- lrm(survived ~ class + age + sibsp + sex_cat, data=titanic2)
print(model3)
