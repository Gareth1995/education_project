# Script to run linear regression for number of learners enrolled

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

merged_masterlist = read.csv('data/merged_masterlist.csv')

# select variables
merged_subset = merged_masterlist %>% select(education.district, correspondence.language.x,
                             institution.type.x, fee.status.x, quintile.x,
                             magisterial.district.x, bus.route.learners,
                             feeding.scheme.learners, mobile.school, connectivity, total.learner.enrolled)



# get data into correct format
merged_subset$education.district = as.factor(merged_subset$education.district)
merged_subset$correspondence.language.x = as.factor(merged_subset$correspondence.language.x)
merged_subset$institution.type.x = as.factor(merged_subset$institution.type.x)
merged_subset$fee.status.x = as.factor(merged_subset$fee.status.x)
merged_subset$quintile.x = as.factor(merged_subset$quintile.x)
merged_subset$mobile.school = as.factor(merged_subset$mobile.school)
merged_subset$connectivity = as.factor(merged_subset$connectivity)
merged_subset$magisterial.district.x = as.factor(merged_subset$magisterial.district.x)

# normalise data
merged_subset$total.learner.enrolled = scale(merged_subset$total.learner.enrolled)
merged_subset$bus.route.learners = scale(merged_subset$bus.route.learners)
merged_subset$feeding.scheme.learners = scale(merged_subset$feeding.scheme.learners)

# apply linear model
lm_model = lm(total.learner.enrolled ~ ., data = merged_subset)
summary(lm_model)






