# Classification for school open or closed
library(caTools)
library(dplyr)
library(e1071)
library(caret)
library(nnet)
library(tidyr)

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

training_set = na.omit(subset(major_df, split == TRUE))
test_set = na.omit(subset(major_df, split == FALSE))

# fit svm
classifier = svm(formula = status.x ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')


# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# building the confusion matrix
confusionMatrix(test_set[,3], y_pred)

# displaying variable importance in SVM
# M <- fit(status.x~., data=training_set, model="svm", kpar=list(sigma=0.10), C=2)
# svm.imp <- Importance(M, data=training_set)

# -------------------Logistic Regression (to see importance of each variable)------------

# Fit the model (quintile and institution type play biggest role in regression)
training_set$status.x = relevel(training_set$status.x, ref = 'Open')
training_set$institution.type.x = relevel(training_set$institution.type.x, ref='Preprimary School')
training_set$fee.status.x = relevel(training_set$fee.status.x, ref = 'No Fee')
training_set$correspondence.language.x = relevel(training_set$correspondence.language.x, ref = 'ENGLISH')
training_set$sector.x = relevel(training_set$sector.x, ref = 'ECD')

# fit multinomial with all variables
multi_nomial_mod = multinom(status.x~., data = training_set)
summary(multi_nomial_mod)

y_preds = predict(multi_nomial_mod, type='class', newdata = test_set)
confusionMatrix(test_set[,3], y_preds)

# Fit multinomial with just quintile
multi_nom_q = multinom(status.x~quintile.x, data = training_set)
summary(multi_nom_q)

q_preds = predict(multi_nom_q, type='class', newdata = test_set)
confusionMatrix(test_set[,3], q_preds)

# get probabilities of school closure given quintile
q_probs = predict(multi_nom_q, type='prob', newdata = test_set)
q_prob_df = cbind(test_set, q_probs)

# df for bar graph of quintile and closure probability
prob_spec_df = q_prob_df %>%
  pivot_longer(cols = c(Open, Closed), names_to="status", values_to="prob") %>%
  select(quintile.x, status, prob) %>%
  unique()

# plot chart of probabilities of school closure given an NQ score
ggplot(prob_spec_df, aes(y=prob, x=quintile.x, fill=status)) +
  geom_bar(position = 'stack', stat = 'identity') +
  labs(x = 'quintile', title = 'probability of school closure given per quintile')

ggplot(prob_spec_df, aes(y=prob, x=quintile.x, fill=status)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(x = 'quintile', title = 'probability of school closure given per quintile')


# Fit multinomial with just institution type
multi_nom_inst = multinom(status.x~institution.type.x, data = training_set)
summary(multi_nom_inst)

inst_preds = predict(multi_nom_inst, type='class', newdata = test_set)
confusionMatrix(test_set[,3], inst_preds)

# get probabilities of school closure given quintile
inst_probs = predict(multi_nom_inst, type='prob', newdata = test_set)
inst_prob_df = cbind(test_set, inst_probs)



# df for bar graph of institution type and closure probability
prob_spec_df_inst = inst_prob_df %>%
  pivot_longer(cols = c(Open, Closed), names_to="status", values_to="prob") %>%
  select(institution.type.x, status, prob) %>%
  unique()



# plot chart of probabilities of school closure given a institution type
ggplot(prob_spec_df_inst, aes(y=prob, x=institution.type.x, fill=status)) +
  geom_bar(position = 'stack', stat = 'identity') +
  labs(x = 'institution type', title = 'probability of school closure given per institution type')

ggplot(prob_spec_df_inst, aes(y=prob, x=institution.type.x, fill=status)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(x = 'institution type', title = 'probability of school closure given per institution type')


# plot multi bar graph of closures per quintile level
closure_per_quintile = major_df %>%
  group_by(quintile.x, status.x) %>%
  count() %>%
  na.omit() %>%
  filter(status.x != 'Pending Closure')

ggplot(closure_per_quintile, aes(y=n, x=quintile.x, fill=status.x)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(x = 'quintile', y = 'number of closures', title = 'number of school closures per quintile')


# plot multibar graph of closures per institution type
closure_per_inst = major_df %>%
  group_by(institution.type.x, status.x) %>%
  count() %>%
  na.omit() %>%
  filter(status.x != 'Pending Closure')

ggplot(closure_per_inst, aes(y=n, x=institution.type.x, fill=status.x)) +
  geom_bar(position = 'stack', stat = 'identity') +
  labs(x = 'Institution type', y = 'Number of closures', title = 'Number of school closures per institution type') +
  theme(axis.text.x = element_text(angle = 90))

#------------------------------Life Span classification----------------------------------

# select variables
lifespan_df = major_df %>% select(correspondence.language.x, sector.x, life_bin,
                               institution.type.x, quintile.x, fee.status.x,
                               magisterial.district.x)

# factorise variables
lifespan_df$life_bin = as.factor(lifespan_df$life_bin)
lifespan_df$correspondence.language.x = as.factor(lifespan_df$correspondence.language.x)
lifespan_df$sector.x = as.factor(lifespan_df$sector.x)
lifespan_df$institution.type.x = as.factor(lifespan_df$institution.type.x)
lifespan_df$quintile.x = as.factor(lifespan_df$quintile.x)
lifespan_df$fee.status.x = as.factor(lifespan_df$fee.status.x)
lifespan_df$magisterial.district.x = as.factor(lifespan_df$magisterial.district.x)

# split data in training and test sets
set.seed(123)
split = sample.split(lifespan_df$life_bin, SplitRatio = 0.75)

training_set = na.omit(subset(lifespan_df, split == TRUE))
test_set = na.omit(subset(lifespan_df, split == FALSE))

# fit svm
classifier = svm(formula = life_bin ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')


# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# building the confusion matrix
confusionMatrix(test_set[,3], y_pred)


