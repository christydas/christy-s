##------------------------------------------------
#           Load the packages
##-------------------------------------------------
library(dplyr) 
library(caret)
library(ROCR)

##------------------------------------------------
#           Read the Data
##-------------------------------------------------
## in the data missing values are represened by "?". convert these values as NA while reading the data
adult <- read.csv("adult.csv", header = FALSE, na.strings = " ?")
colnames(adult) <- c("age", "workclass", "fnlwgt", "education", 
                        "education_num", "marital_status", "occupation",
                        "relationship", "race", "sex", "capital_gain", 
                        "capital_loss", "hours_per_week", "native_country", "income")

##------------------------------------------------
#           Data Exploration and Analysis
##-------------------------------------------------
## lets remove the data with missing values in it
adult<- adult %>% filter(complete.cases(.))

# For simplicity of this analysis, the weighting factor is discarded. 
# Total number of years of education can represent by the highest education level completed. 
# Role in the family can be assessed from gender and marital status. 
# Thus, the following 3 variables are deleted education, relationship, and fnlwgt.
adult$education <- NULL
adult$fnlwgt <- NULL
adult$relationship <- NULL

# The first variable age is a continuous variable. As an initial step, two histograms are plotted.
# histogram of age by income group
ggplot(adult) + aes(x=as.numeric(age), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')

# It is noticed that majority of the observations make less than $50,000 a year. 
# For those do make over $50,000 annually, they are mainly in middle age group

# histogram of age by gender group
ggplot(adult) + aes(x=as.numeric(age), group=sex, fill=sex) + 
  geom_histogram(binwidth=1, color='black')

# The variable workclass stands for the industry in which the responding unit is employed.
table(adult$workclass)

## many values can be clubbed further for better decision
# 1. convert     all govt class (Federal-gov,Local-gov,State-gov) into single class (Government)
adult<- adult %>% mutate(workclass=ifelse(workclass %in% c(" Federal-gov"," Local-gov"," State-gov"),"Government",workclass))
# 2. # combine Self-emp-inc, Self-emp-not-inc into Sele-Employed job
adult<- adult %>% mutate(workclass=ifelse(workclass %in% c(" Self-emp-not-inc"," Self-emp-inc"),"Sele-Employed",workclass)) %>% 
                  mutate(workclass=as.factor(workclass))

## To explore the relationship between industry and income
industry_df<- adult %>% group_by(workclass,income) %>% summarise(count=n())

# # bar plot of counts by industry with in group proportions 
ggplot(industry_df, aes(x = workclass, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  ggtitle('Income by Industry')

# Since education_num is a continuous representation of education, 
# a stacked bar plot is used to visualize the relationship between education_num 
# and income, in-group proportions are calculated as well.
education_df<- adult %>% group_by(income,education_num) %>% summarise(count=n())

# bar plot of counts by years of education with in group proportions 
ggplot(education_df, aes(x = education_num, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  ggtitle('Income Level with Years of Education')
# It is not hard to notice that the in group proportion of making greater than $50,000 a year increase as the years of education increases.

## lets explore occupation
table(adult$occupation)

#For simplicity of the model, occupation is also blocked into several groups, namely Blue-Collar, Professional, Sales, Service, and White-Collar
adult<- adult %>% mutate(occupation=ifelse(occupation %in% c(" Adm-clerical"," Exec-managerial"),"White-Collar",occupation)) %>%
                  mutate(occupation=ifelse(occupation %in% c(" Craft-repair"," Farming-fishing"," Handlers-cleaners"," Machine-op-inspct"," Transport-moving"),"Blue-Collar",occupation)) %>%
                  mutate(occupation=ifelse(occupation %in% c(" Other-service"," Priv-house-serv"," Protective-serv"," Tech-support"," Transport-moving"),"Service",occupation)) %>%
                  mutate(occupation=ifelse(occupation %in% c(" Armed-Forces"," Prof-specialty"),"Professional",occupation)) %>%
                  mutate(occupation=as.factor(occupation))

# create a dataframe to visualise the data 
occupation_df<- adult %>% group_by(income,occupation) %>% summarise(count=n())

# bar plot of counts by occupation with in group proportions 
ggplot(occupation_df, aes(x = occupation, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  ggtitle('Income Level with Different Occupations')
# It is noticed that income varies greatly across different occupations. Nearly half of Professional occupation makes greater than $50,000 a year.


## explore marital_status, is a categorical variable with 7 categories indicating the marital status of observations. In fact, it can be blocked into a few categories as well.
table(adult$marital_status)

adult<- adult %>% mutate(marital_status=ifelse(marital_status %in% c(" Married-AF-spouse"," Married-civ-spouse"," Married-spouse-absent"),"Married",marital_status)) %>%
  mutate(marital_status=ifelse(marital_status %in% c(" Never-married"),"Single",marital_status)) %>%
  mutate(marital_status=as.factor(marital_status))

# create a dataframe to visualise the data 
marital_df<- adult %>% group_by(income,marital_status) %>% summarise(count=n())

# bar plot of counts by marital status with in group proportions 
ggplot(marital_df, aes(x = marital_status, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  ggtitle('Income Level with Marital Status')
# For those who are married, nearly half of them are making greater than $50,000 a year.

# capital_gain and capital_loss are two continuous variables describing income and loss from financial investments. 
# Histograms show that the distributions of these two variables are both highly screwed.
# histogram of capital_gain
ggplot(adult) + aes(x=as.numeric(capital_gain), group=income, fill=income) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Gain')

# histogram of capital_loss
ggplot(adult) + aes(x=as.numeric(capital_loss), group=income, fill=income) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Loss')

# percentage of observatiosn with no capital gain or loss
sum(adult$capital_gain == 0)/length(adult$capital_gain)
sum(adult$capital_loss == 0)/length(adult$capital_loss)

# In fact, most observations have zero capital_gain and/or capital_loss. 
# Similarly, there native_country displays high skewness as most observations are from United States. 
# Therefore, these three variables are excluded from the analysis as well.
adult$capital_gain <- NULL
adult$capital_loss <- NULL
adult$native_country <- NULL

# race is a categorical variable. 
# create a dataframe to visualise the data 
race_df<- adult %>% group_by(income,race) %>% summarise(count=n())

# bar plot of counts by marital status with in group proportions 
ggplot(race_df, aes(x = race, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  ggtitle('Income Level by Race')
# Bar plot shows that White and Asian-Pacific Islander have high earning potentials - over 25% of the observations of these 2 races make above $50,000 annually.


# Now, we are left with 9 variables for the analysis. Below is a table of summary statistics.
summary(adult)

## lets convert our outcome variab;le as 1 and 0. >50K:1, <=50K:0
adult<- adult %>% mutate(income=factor(ifelse(income==" >50K",1,0)))

##------------------------------------------------
#           Model Fitting - Logistic Regression
##-------------------------------------------------
# 80% of the original data is used as the training set, while the rest 20% is used as test set.
sz <- round(.8 * dim(adult)[1])  # training set size
training_set <- adult[1:sz,]
testing_set <- adult[-(1:sz),]

# A logistic regression using income as the response variable, and all other 8 variables as predictors is fitted. 
# Its parameter estimates and confidence intervals are reported as below.
m1 <- glm(income ~ ., data = training_set, family = binomial())
summary(m1)

#This logistic regression model appears to fit the data well. 
# To explore the possibility of a parsimonious model, both the forward and backward stepwise selection algorithms using AIC are performed.
m_full <- m1  # full model is the model just fitted
m_null <- glm(income ~ 1, data = training_set, family = binomial('logit'))

# backward selection
step(m_full, trace = F, scope = list(lower=formula(m_null), upper=formula(m_full)),
     direction = 'backward')
# forward selection
step(m_null, trace = F, scope = list(lower=formula(m_null), upper=formula(m_full)),
     direction = 'forward')

# Both the forward and backward stepwise selection algorithms give the same model as the initial fit. 
# Thus, the original model is chosen and model diagnostics are to follow. 
# As expected for a good model, most of the residuals are falling within ?3:
# create a data frame to store information regarding deviance residuals
index <- 1:dim(training_set)[1]
dev_resid <- residuals(m1)
income <- training_set$income
dff <- data.frame(index, dev_resid, income)

ggplot(dff, aes(x = index, y = dev_resid, color = income)) +
  geom_point() + 
  geom_hline(yintercept = 3, linetype = 'dashed', color = 'blue') +
  geom_hline(yintercept = -3, linetype = 'dashed', color = 'blue') +
  ggtitle('Plot of Deviance Residuals')

## make prediction on the test data
# Logistic regression is modeling the probability that an individual makes more than $50,000 annually.
# In another word, a response closer to 1 indicates higher chance of making over $50,000, while a response closer to 0 indicates a higher chance of making less than $50,000. 
# Thus, a threshold of 0.5 is used to determine whether an individual is predicted to make more than $50,000 annually or not. 
# A confusion matrix is presented to evaluate how well the model predicts income.
prob <- predict(m1, testing_set)
# confusion matrix 
tb <- table(testing_set$income,prob>.5)
tb

Accuracy<-round(sum(diag(tb))/sum(tb),2)
Accuracy
# The prediction result has an accuracy of ~81%.

# create a prediction object
pr <- prediction(prob, testing_set$income)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

# create a data frame for TP and FP rates
dd <- data.frame(FP = prf@x.values[[1]], TP = prf@y.values[[1]])

# plot ROC curve for logistic regression
ggplot() + 
  geom_line(data = dd, aes(x = FP, y = TP)) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
  ggtitle('ROC Curve') + 
  labs(x = 'False Positive Rate', y = 'True Positive Rate') 

## value of AUC
auc_value<-performance(pr,"auc")@y.values[[1]]
auc_value
## We get the accuracy from the Test data to be 88%.
