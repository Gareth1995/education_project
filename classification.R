# Classification for school open or closed
library(caTools)
library(dplyr)
library(e1071)

# load data
major_df = read.csv('data/major_merged_schoolist.csv')

# select variables
major_df = major_df %>% select(correspondence.language.x, sector.x, status.x,
                               institution.type.x, quintile.x, fee.status.x,
                               magisterial.district.x)

# factorise variables
major_df$status.x = as.factor(major_df$status.x)
major_df$correspondence.language.x = as.factor(major_df$correspondence.language.x)
major_df$sector.x = as.factor(major_df$sector.x)
major_df$institution.type.x = as.factor(major_df$institution.type.x)
major_df$quintile.x = as.factor(major_df$quintile.x)
major_df$fee.status.x = as.factor(major_df$fee.status.x)
major_df$magisterial.district.x = as.factor(major_df$magisterial.district.x)

# split data in training and test sets
set.seed(123)
split = sample.split(major_df$status.x, SplitRatio = 0.75)

training_set = subset(major_df, split == TRUE)
test_set = subset(major_df, split == FALSE)

# fit svm
classifier = svm(formula = status.x ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')


# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)


