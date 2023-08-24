# modelling drop out rate
library(readxl)
library(tidyr)
library(dplyr)

# load in enrollment data
enrollment_data = read_xlsx("data/enrollment data.xlsx", sheet = "Clean_PO_Detail")

# group by education district and year
# sum each grade
ed = enrollment_data %>% group_by(`EDUC DISTRICT`, YEAR) %>% 
  summarize(`TOTAL 1` = sum(`TOTAL 1`),
            `TOTAL 2` = sum(`TOTAL 2`),
            PREGRR = sum(PREGRR),
            GRR = sum(PREGRR),
            GR1 = sum(GR1),
            GR2 = sum(GR2),
            GR3 = sum(GR3),
            GR4 = sum(GR4),
            GR5 = sum(GR5),
            GR6 = sum(GR6),
            GR7 = sum(GR7),
            GR8 = sum(GR8),
            GR9 = sum(GR9),
            GR10 = sum(GR10),
            GR11 = sum(GR11),
            GR12 = sum(GR12)) %>%
  filter(`EDUC DISTRICT` == 'METRO EAST')


# save resulting dataframe
write.csv(ed, 'data/dropout_data.csv', row.names = FALSE)

# use excel to create column of dropout numbers over the years

# load in edited dataframe in python

# run time series and save output

