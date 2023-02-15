
# import college admission data file
data = read.csv(file.choose())

# exploring the data 
head(data)
tail(data)

str(data)
summary(data)

### Based on above data, we can identify variables as follows:
### Target variable : admit
### Continuous variable : gre and gpa
### Categorical variables : ses, gender_male, race, rank

# Converting categorical variables to factor datatype
data$ses = as.factor(data$ses)
data$Gender_Male = as.factor(data$Gender_Male)
data$Race = as.factor(data$Race)
data$rank = as.factor(data$rank)

str(data)


#==========================================================================
# find missing values
sum(is.na(data))
### No missing values found.


#============================================================================
# FIND OUTLIERS
###using boxplot to identify outliers in continuous variables

#outlier detection for GRE
boxplot(data$gre, main="Outliers in gre", horizontal=TRUE)

greoutlier = boxplot(data$gre)$out
length(greoutlier) #### hence, there are 4 outliers in gre

# transform this data with its mean value
#mean(data$gre)

replacevalues = which(data$gre %in% greoutlier)
data$gre = replace(data$gre, replacevalues, mean(data$gre))

str(data)

boxplot(data$gre,main="Outliers in gre", horizontal=TRUE)


##### outlier detection for GPA 
boxplot(data$gpa,main="Outliers in gpa", horizontal=TRUE)

gpaoutlier = boxplot(data$gpa, horizontal = T)$out
length(gpaoutlier) ####only 1 outlier, we can drop this

data = data[-c(gpaoutlier),]
str(data) #### now we see 399 observations as compared to 400 earlier

#========================================================================
# find structure of data : normal distribution 

density_data = density(data$gre)
plot(density_data)

plot(density(data$gpa))
### hence it follows the normal distribution

#=========================================================================

head(data)

### GRE and GPA are continuous variables, but not in the same range 
### Hence scaling is required
### Creating a copy of data 

scaledData = data 

scaledData$gre = scale(scaledData$gre, center = T, scale = T)
scaledData$gpa = scale(scaledData$gpa, center = T, scale = T)
head(scaledData)

#==========================================================================
#Use variable reduction techniques to identify significant variables.

set.seed(12)
train_indices = sample(1:nrow(scaledData), 0.70 * nrow(scaledData))


trainData = scaledData[train_indices, ]
testData = scaledData[-train_indices,]
test_without_admit = test[,-1]

#Using logistic regression

log_reg = glm(admit ~ . , data = trainData, 
               family = 'binomial')

summary(log_reg)
# GRE GPA and rank are significant variables - affecting admission process

#==========================================================================

# Calculate the accuracy of the model and run validation techniques
# Using the result of logistic regresssion model to calculate the accuracy

head(data)

# Using only the significant variables for Building models
df = data[c(1,2,3,7)]

head(df)


set.seed(12)
tr_idx <- sample(1:nrow(df), 0.70 * nrow(df))
Train <- df[tr_idx, ]
Test <- df[-tr_idx,]
test_without_admit <- Test[,-1]


log_reg <- glm(admit ~ . , data = Train, 
               family = 'binomial')

summary(log_reg)

# Importing library MASS and CARET
# Predicting on test data and calculating the accuracy using confusion matrix

library(MASS)
library(caret)
pred_class = predict(dTree, newdata = Test[,-1], type = 'vector')
caret::confusionMatrix(as.factor(pred_class), as.factor(Test$admit), positive = '0')











