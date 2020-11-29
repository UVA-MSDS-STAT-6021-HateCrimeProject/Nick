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

hc_df<-read.csv('hate_crimes_full.csv', row.names = 'state_abbrev')
colnames(hc_df)

#Need to set categorical variables so R can recognize them as such 
hc_df$confederate<-factor(hc_df$confederate)
is.factor(hc_df$confederate)
contrasts(hc_df$confederate) #Just to verify that R has the right reference class set
hc_df$permit<-factor(hc_df$permit) #State has permit to purchase law 1 = yes
hc_df$universl<-factor(hc_df$universl) #State has universal background checks 1 = yes

#Looking at making a dataframe only with SPLC data first 
hc_df<-hc_df[ , !(names(hc_df) %in% c('hate_crimes_per_100k_splc', 'state_full', 'FIP', 'Year', 
                                      'FIP	Year',	'HFR_se',	'BRFSS', 'GALLUP',	'GSS',	'PEW',	'HuntLic',	'GunsAmmo',	
                                      'BackChk',	'PewQChng',	'BS1',	'BS2',	'BS3', 'X'))]

hc_df<-hc_df[!is.na(c(hc_df$avg_hatecrimes_per_100k_fbi, 
                      hc_df$share_non_citizen)),]

#Used to remove all the rows that include NA values 
hc_df<-hc_df[complete.cases(hc_df), ] 

nrow(hc_df)

x<-model.matrix(avg_hatecrimes_per_100k_fbi~share_unemployed_seasonal+
                  share_population_with_high_school_degree+
                  share_white_poverty+
                  share_non_white+
                  median_household_income+
                  share_population_in_metro_areas+
                  share_non_citizen+
                  gini_index+
                  gini_index+
                  elasticity+
                  confederate+
                  HFR+
                  universl+
                  permit+
                  HateGroupCount+
                  Male_FS_S+
                  Fem_FS_S, hc_df)[,-1] # remove the first column of 1s representing the intercept
y<-hc_df$avg_hatecrimes_per_100k_fbi

#Make sure I still have the number of rows I suspect to have 
nrow(x)

pairs(x, lower.panel=NULL, main="Scatterplots of Predictors")

#Correlation just to see if any of the variables have potential
cor(x)

boxplot(hc_df$avg_hatecrimes_per_100k_fbi~hc_df$confederate)
boxplot(hc_df$avg_hatecrimes_per_100k_fbi~hc_df$universl)
boxplot(hc_df$avg_hatecrimes_per_100k_fbi~hc_df$permit)

result<-lm(avg_hatecrimes_per_100k_fbi~., data = hc_df)

vif(result) # HFR along with male and female suicide rate is highly correlated (18+ VIF)

summary(result)

# #########################################################################

#Perform all possible regressions (1st order) gets you which predictors are best for certain situations
allreg <- regsubsets(avg_hatecrimes_per_100k_fbi ~., data=hc_df, nbest=9)

##create a "data frame" that stores the predictors in the various models considered as well as their various criteria
best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(hc_df)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

#Sort by various criteria
best[order(best$r2),]     
best[order(best$adjr2),]  
best[order(best$mse),]    
best[order(best$cp),]     
best[order(best$bic),]    

#Intercept only model to set up for selection process
regnull <- lm(avg_hatecrimes_per_100k_fbi~1, data=hc_df)

#Model with all 17 predictors for our model
regfull <- lm(avg_hatecrimes_per_100k_fbi~., data=hc_df)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

#Forward selection model
forward_result <- lm(formula = avg_hatecrimes_per_100k_fbi ~ Male_FS_S + share_non_white + 
                       share_non_citizen, data = hc_df)

#Backward selection model suicide rate becomes important when going with this approach
backward_result <- lm(formula = avg_hatecrimes_per_100k_fbi ~ share_non_citizen + 
     gini_index + share_non_white + HateGroupCount, data = hc_df)

#Both selections model
both_result<-lm(formula = avg_hatecrimes_per_100k_fbi ~ share_non_white + 
                  share_non_citizen, data = hc_df)

# #########################################################################

#Full Model
result<-lm(avg_hatecrimes_per_100k_fbi~., data = hc_df)
summary(result)

#Model chosen from backwards selection
resultsmall<-lm(formula = avg_hatecrimes_per_100k_fbi ~ share_non_citizen + 
                  gini_index + share_non_white + HateGroupCount, data = hc_df)
summary(resultsmall)

# ANSWER: The null hypothesis says that all missing variables are equal to 0.  The alternative 
# states that each of their slopes are nonzero so good for our model fit.

anova(resultsmall,result)
anova(result)

#Every sum of squares is given based on the fact that the previous predictor is in the model

#ANSWER: Go with the smaller model because you fail to reject the null since p value
#is greater than your significance level of 0.05

# This tells you that you reject the null Hypothesis which means you must keep interaction terms in the model also the 
# adjusted R squared value is greater in the model that includes interaction with color. 

# #########################################################################

#Residual Plot of the smaller model from backwards selection
plot(resultsmall$fitted.values,resultsmall$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

library(MASS)
boxcox(resultsmall)
boxcox(resultsmall, lambda = seq(-0.5, 1.5, 0.2))










#Performed transformation just to make the linear assumptions a bit better

new.avg_hatecrimes_per_100k_fbi<-log(hc_df$avg_hatecrimes_per_100k_fbi)
result.new<-lm(new.avg_hatecrimes_per_100k_fbi ~ share_non_citizen + 
                 gini_index + share_non_white + HateGroupCount, data = hc_df)

hc_df$new.avg_hatecrimes_per_100k_fbi=new.avg_hatecrimes_per_100k_fbi
hc_df

library(MASS)
boxcox(result.new)
boxcox(result.new, lambda = seq(-0.5, 1.5, 0.2)) #Use the visual and it shows that it's pretty good and the boxcox plot isnt perfect


#ACF plot of residuals
acf(resultsmall$residuals)

#QQ plot of residuals
qqnorm(resultsmall$residuals)
qqline(resultsmall$residuals, col="red")

plot(nitrogen,yield,xlab="Nitrogen Pounds per Acre", ylab="Corn yield in Bushels per Acre", 
     main="Plot of defects against weeks")
result<-lm(yield~nitrogen)
abline(result,col="blue")






































