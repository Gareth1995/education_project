# Script for EDA plots
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(lubridate)

# load data
merged_masterlist = read.csv('data/merged_masterlist.csv')

# plot number of learners distribution
hist(merged_masterlist$total.learner.enrolled, breaks=100, main = "Distribution of number of enrolled students", xlab = "number of students")

#-----------------------------------------------------------------------------------------------
#----------------------Scatter plots----------------------------------------------------------------

# plotting function
scatter_plot <- function(school_data, x, y, title, x_lab, y_lab){
  scPlot = ggplot(school_data, aes(x=x, y=y)) +
    geom_point() +
    labs(title=title, x=x_lab, y=y_lab)
  
  # geom_text(label = merged_masterlist$`magisterial district.x`,
  #           nudge_x = 0.25,
  #           nudge_y = 0.25, 
  #           check_overlap = T)
  
  return (scPlot)
}

# plot learners enrolled vs feeding scheme
learner_v_feeding = ggplot(merged_masterlist, aes(x=feeding.scheme.learners, y=total.learner.enrolled, color = quintile.x)) +
  geom_point() +
  scale_color_manual(values=c("black", "orange", "blue", "red", "purple", "green"))+
  labs(x='number of learners on feeding scheme',
       y='total learners enrolled',
       title='relationship between school enrollment and number of children on feeding scheme',
       fill = 'Quintile')

learner_v_feeding

learner_v_feeding = ggplot(merged_masterlist, aes(x=feeding.scheme.learners, y=total.learner.enrolled, color = quintile.x)) +
  geom_point() +
  scale_color_manual(values=c("black", "orange", "blue", "red", "purple", "green"))+
  labs(x='number of learners on feeding scheme',
       y='total learners enrolled',
       title='relationship between school enrollment and number of children on feeding scheme',
       fill = 'Quintile')


# plot learners enrolled vs bus routes
learner_v_bus = ggplot(merged_masterlist, aes(x=bus.route.learners, y=total.learner.enrolled, color = quintile.x)) +
  geom_point() +
  scale_color_manual(values=c("black", "orange", "blue", "red", "purple", "green"))+
  labs(x='number of learners taking bus routes',
       y='total learners enrolled',
       title='relationship between school enrollment and number of learners that take bus transport')
  
learner_v_bus

# plot number learners vs hostel
learners_v_hostel = ggplot(merged_masterlist, aes(x=hostel.learners, y=total.learner.enrolled, color = quintile.x)) +
  geom_point() +
  scale_color_manual(values=c("black", "orange", "blue", "red", "purple", "green"))+
  labs(x='number of learners in hostel',
       y='total learners enrolled',
       title='relationship between school enrollment and number of learners in hostel')

learners_v_hostel

# plot feeding scheme vs bus routes
feeding_v_bus = ggplot(merged_masterlist, aes(x=feeding.scheme.learners, y=bus.route.learners, color = quintile.x)) +
  geom_point() +
  scale_color_manual(values=c("black", "orange", "blue", "red", "purple", "green"))+
  labs(x='number of learners on feeding scheme',
       y='number of learners taking bus routes',
       title='relationship between learners on feeding scheme and number of children that take bus transport')

feeding_v_bus


# plot hostel vs feeding scheme
hostel_v_feeding = ggplot(merged_masterlist, aes(x=hostel.learners, y=feeding.scheme.learners, color = quintile.x)) +
  geom_point() +
  scale_color_manual(values=c("black", "orange", "blue", "red", "purple", "green"))+
  labs(x='number of learners in hostel',
       y='number of learners on feeding scheme',
       title='relationship between learners in hostel and number of learners on feeding scheme')

hostel_v_feeding 

#-----------------------------------------------------------------------------------------------
#----------------------Bar plots----------------------------------------------------------------

###### Based on quintiles ##############
# total number of students per quintile 
q_learner_total = merged_masterlist %>% 
  group_by(quintile.x) %>%
  summarise(total_learners = sum(total.learner.enrolled, na.rm = T))

# mean number of students per quintile 
q_learner_mean = merged_masterlist %>% 
  group_by(quintile.x) %>%
  summarise(mean_learners = mean(total.learner.enrolled, na.rm = T))

# number of schools per quintile
num_q =  merged_masterlist %>% 
  group_by(quintile.x) %>%
  summarise(n = n())

# plot frequency of quintile and food schemes
q_feeding = merged_masterlist %>% 
  group_by(quintile.x) %>%
  summarise(total_feeding = sum(feeding.scheme.learners, na.rm = T))

# number of bus route learners per quintile
q_bus = merged_masterlist %>% 
  group_by(quintile.x) %>%
  summarise(total_bus = sum(bus.route.learners, na.rm = T))

# plot bar plots
# plot of number of schools per quintile 
num_q_plot = ggplot(num_q, aes(x=quintile.x, y=n)) +
  geom_bar(stat = 'identity') +
  labs(x = 'quintile', y = 'n', title = 'number of schools per quintile')

# plot of total learners per quintile
q_learner_total_plot = ggplot(q_learner_total, aes(x=quintile.x, y=total_learners)) +
  geom_bar(stat='identity') +
  labs(x = 'quintile', y = 'total learners', title = 'total learners per quintile')

# plot of mean learners per quintile
q_learner_mean_plot = ggplot(q_learner_mean, aes(x=quintile.x, y=mean_learners)) +
  geom_bar(stat='identity') +
  labs(x = 'quintile', y = 'mean learners', title = 'mean number of learners per quintile')

# plot barplots in a grid
grid.arrange(num_q_plot,
             q_learner_total_plot,
             q_learner_mean_plot,
             nrow = 2)

#### Based on location ####

merged_masterlist$quintile.x = as.factor(merged_masterlist$quintile.x)

# plot number of learners per education district
merged_masterlist %>% 
  group_by(education.district) %>%
  summarise(mean_learners = mean(total.learner.enrolled, na.rm = T))

# plot number of different quintiles per education district
s = merged_masterlist %>% 
  group_by(education.district, quintile.x) %>%
  count()

# plot number of learners per insitution type

# plot number of different quintiles per institution type


#### Misc ####

# number learners in school according to correspondence language
merged_masterlist %>% 
  group_by(correspondence.language.x) %>%
  summarise(total_learners = sum(total.learner.enrolled, na.rm = T))

# number of learners in schools with internet
merged_masterlist %>% 
  group_by(connectivity) %>%
  summarise(total_learners = sum(total.learner.enrolled, na.rm = T))

