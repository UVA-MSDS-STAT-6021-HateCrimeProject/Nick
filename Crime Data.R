#STAT 6021 Crime Project 
#mmn4sv - Matt Nicklas
#Using the data set from FiveThirtyEight to see which variables contribute to hate crimes measure as rate per 100K residents 

# #########################################################################
# We will produce a model using our full suite of methods pertaining to multiple 
# linear regression (MLR). We will focus on variable and data exploration, including 
# performing any data transformations necessary to meet the assumptions of MLR; 
# outlier/influential-point analysis; ANOVA and partial F tests to determine predictive power 
# and inclusion of variables; and general model selection procedures learned throughout the semester. 
# #########################################################################

# #########################################################################
#DATA ANALYSIS
# #########################################################################

library(glmnet)
library(faraway)
library(leaps)
library(tidyverse)

hate_DF<-read.csv('hate_crimes_full_v2.csv', row.names = 'state_abbrev')
colnames(hate_DF)

#Need to set categorical variables so R can recognize them as such 
hate_DF$confederate<-factor(hate_DF$confederate)
is.factor(hate_DF$confederate)
contrasts(hate_DF$confederate) #Just to verify that R has the right reference class set
hate_DF$permit<-factor(hate_DF$permit) #State has permit to purchase law 1 = yes
hate_DF$universl<-factor(hate_DF$universl) #State has universal background checks 1 = yes
hate_DF$con_uni_combo<-factor(hate_DF$con_uni_combo)

#Looking at making a dataframe only including the variables left after removing these below
hate_DF<-hate_DF[ , !(names(hate_DF) %in% c('fbi_2019_per100k', 'hate_crimes_per_100k_splc', 'state_full', 'FIP', 'Year', 
                                      'hate_group_count_2019', 'FIP	Year',	'HFR_se',	'BRFSS',	'GALLUP',	'GSS',	'PEW',	'HuntLic',	'GunsAmmo',	
                                      'BackChk',	'PewQChng',	'BS1',	'BS2',	'BS3', 'pk_count', 'Number.of.participating.agencies',
                                      'Agencies.submitting.incident.reports', 'pop_covered', 'population', 'incidents', 'pk_percap',
                                      'confederate', 'universl'))]

#Setting the reference level to be neither for universal and confederate combo. Should have 3 sepereate equations from this
levels(hate_DF$con_uni_combo)
hate_DF$con_uni_combo<-relevel(hate_DF$con_uni_combo, ref = "Neither")
levels(hate_DF$con_uni_combo)

#Used to remove all the rows that include NA values 
hate_DF<-hate_DF %>% drop_na(avg_hatecrimes_per_100k_fbi)
#hate_DF<-hate_DF[!(row.names(hate_DF) %in% c('DC')),] #REMOVE THE DC NEED TO DO THAT

nrow(hate_DF)

pairs(hate_DF, lower.panel=NULL, main="Scatterplots of Predictors")

#Can see that a lot of variables have large amount of correlation 

boxplot(hate_DF$avg_hatecrimes_per_100k_fbi~hate_DF$confederate)
boxplot(hate_DF$avg_hatecrimes_per_100k_fbi~hate_DF$universl)
boxplot(hate_DF$avg_hatecrimes_per_100k_fbi~hate_DF$permit)

result<-lm(avg_hatecrimes_per_100k_fbi~., data = hate_DF)

vif(result) # HFR along with male and female suicide rate is highly correlated (18+ VIF)

summary(result)

# #########################################################################
#Remove the variables I don't want cause of high correlation from VIF calculation

hate_DF<-hate_DF[ , !(names(hate_DF) %in% c('median_household_income', 'share_population_with_high_school_degree', 'Fem_FS_S', 'HFR', 'share_voters_voted_trump', 
                                      'share_population_in_metro_areas', 'share_non_white', 'share_non_citizen', 'Male_FS_S'))]

# #########################################################################

hate_DF
#Intercept only model to set up for selection process
regnull <- lm(avg_hatecrimes_per_100k_fbi~1, data=hate_DF)

#Model with all 17 predictors for our model
regfull <- lm(avg_hatecrimes_per_100k_fbi~., data=hate_DF)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

#Forward selection model
forward_result <- lm(formula = avg_hatecrimes_per_100k_fbi ~ confederate + gini_index, 
                     data = hate_DF)
summary(forward_result)
#Backward selection model suicide rate becomes important when going with this approach
backward_result <- lm(formula = avg_hatecrimes_per_100k_fbi ~ confederate + gini_index, 
                      data = hate_DF)
summary(backward_result)
#Both selections model
both_result<-lm(formula = avg_hatecrimes_per_100k_fbi ~ gini_index + con_uni_combo + 
                  hate_group_count_2016 + elasticity + share_unemployed_seasonal, 
                data = hate_DF)
summary(both_result)
# #########################################################################

#Full Model
result<-lm(avg_hatecrimes_per_100k_fbi~., data = hate_DF)
summary(result)

#Model chosen from backwards selection
resultsmall<-lm(formula = avg_hatecrimes_per_100k_fbi ~ gini_index + con_uni_combo + 
                  hate_group_count_2016 + elasticity + share_unemployed_seasonal, 
                data = hate_DF)
summary(resultsmall)

# ANSWER: The null hypothesis says that all missing variables are equal to 0.  The alternative 
# states that each of their slopes are nonzero so good for our model fit.

anova(resultsmall,result)
anova(result)

#Every sum of squares is given based on the fact that the previous predictor is in the model

#ANSWER: Go with the smaller model because you fail to reject the null since p value
#is greater than your significance level of 0.05

#NEED TO COMPLETE

result2<-lm(formula = avg_hatecrimes_per_100k_fbi ~ gini_index + con_uni_combo + 
              hate_group_count_2016 + elasticity + share_unemployed_seasonal, 
            data = hate_DF)
summary(result2)

#Model chosen from backwards selection
resultsmall2<-lm(formula = avg_hatecrimes_per_100k_fbi ~ gini_index + con_uni_combo + 
                   hate_group_count_2016 + elasticity, data = hate_DF)
summary(resultsmall2)

# ANSWER: The null hypothesis says that all missing variables are equal to 0.  The alternative 
# states that each of their slopes are nonzero so good for our model fit.

anova(resultsmall2,result2)
anova(result2)

library(MASS)
boxcox(resultsmall2)
boxcox(resultsmall2, lambda = seq(-0.5, 1.5, 0.2))

#Residual Plot of the smaller model from backwards selection
plot(resultsmall2$fitted.values,resultsmall2$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

#Took the square root because the boxcox and residual plot indicate the linear transformations aren't met
final1 <- lm(formula = sqrt(avg_hatecrimes_per_100k_fbi) ~ gini_index + con_uni_combo + 
                            hate_group_count_2016 + elasticity, data = hate_DF)

summary(final1)

library(MASS)
boxcox(final1)
boxcox(final1, lambda = seq(-0.5, 1.5, 0.2))

#Residual Plot of the smaller model from backwards selection
plot(final1$fitted.values,final1$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

#Next take the transformation data and perfrom another partial F test 

#Full Model
final1 <- lm(formula = sqrt(avg_hatecrimes_per_100k_fbi) ~ gini_index + con_uni_combo + 
               hate_group_count_2016 + elasticity, data = hate_DF)
summary(final1)

#Model chosen from backwards selection
final2 <- lm(formula = sqrt(avg_hatecrimes_per_100k_fbi) ~ gini_index + con_uni_combo + 
               hate_group_count_2016, data = hate_DF)
summary(final2)

# ANSWER: The null hypothesis says that all missing variables are equal to 0.  The alternative 
# states that each of their slopes are nonzero so good for our model fit.

anova(final2,final1)

#ANOTHER PARTIAL F TEST 

final2 <- lm(formula = sqrt(avg_hatecrimes_per_100k_fbi) ~ gini_index + con_uni_combo + 
               hate_group_count_2016, data = hate_DF)
summary(final2)

#Model chosen from backwards selection
final3 <- lm(formula = sqrt(avg_hatecrimes_per_100k_fbi) ~ gini_index + con_uni_combo, data = hate_DF)
summary(final3)

# ANSWER: The null hypothesis says that all missing variables are equal to 0.  The alternative 
# states that each of their slopes are nonzero so good for our model fit.

anova(final3,final2)
# #########################################################################

#LOOKS LIKE THE FINAL MODEL WITH DC WILL JUST BE GINI INDEX and Confederate only???????
#ASK GROUP 

















#Performed transformation just to make the linear assumptions a bit better

new.avg_hatecrimes_per_100k_fbi<-log(hate_DF$avg_hatecrimes_per_100k_fbi)
result.new<-lm(new.avg_hatecrimes_per_100k_fbi ~ share_non_citizen + 
                 gini_index + share_non_white + HateGroupCount, data = hate_DF)

hate_DF$new.avg_hatecrimes_per_100k_fbi=new.avg_hatecrimes_per_100k_fbi
hate_DF

library(MASS)
boxcox(result.new)
boxcox(result.new, lambda = seq(-0.5, 1.5, 0.2)) #Use the visual and it shows that it's pretty good and the boxcox plot isnt perfect


#ACF plot of residuals
acf(resultsmall$residuals)

#QQ plot of residuals
qqnorm(resultsmall$residuals)
qqline(resultsmall$residuals, col="red")




#Perform all possible regressions (1st order) gets you which predictors are best for certain situations
allreg <- regsubsets(avg_hatecrimes_per_100k_fbi ~., data=hate_DF, nbest=9)

##create a "data frame" that stores the predictors in the various models considered as well as their various criteria
best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(hate_DF)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

#Sort by various criteria
best[order(best$r2),]     
best[order(best$adjr2),]  
best[order(best$mse),]    
best[order(best$cp),]     
best[order(best$bic),] 






































