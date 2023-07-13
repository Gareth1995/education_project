# R script to clean the education data
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(lubridate)

# ---------------Masterlist EDA-------------------------------------------------------
masterlist = read_excel("data/masterlist_open_closed.xlsx")
public_school_list = read_excel("data/Masterlist of Public Schools.xlsx")

# change the column names
new_names_masterlist = c('education district', 'circuit', 'emis_no', 'school_name',
                         'component', 'lolt', 'correspondence language', 'sector',
                         'control', 'status', 'institution type', 'num learners',
                         'quintile', 'fee status', 'tel code', 'tell_no', 'cell', 'postal address_1',
                         'postal address_2', 'postal address_3', 'postal code', 'physical address_1',
                         'physical address_2', 'physical address_3', 'physical postal code',
                         'suburb', 'town', 'fax code', 'fax', 'email', 'principal',
                         'principal mobile', 'wcg email', 'magisterial district', 'district council',
                         'municipality', 'ward', 'subcouncil', 'long', 'lat', 'section21', 'exam centre',
                         'lowest grade', 'highest grade', 'indp subsidised', 'date open', 'date closed',
                         'e_file', 'hostel', 'exam authority', 'curriculum offered', 'ed director',
                         'circuit manager')

colnames(masterlist) = new_names_masterlist
masterlist = masterlist %>% select(emis_no, 2:53) # get emis_no as first column

# change variable types to appropriate type
masterlist$circuit = as.factor(masterlist$circuit)
masterlist$lolt = as.factor(masterlist$lolt)
masterlist$`correspondence language`= as.factor(masterlist$`correspondence language`)
masterlist$sector = as.factor(masterlist$sector)
masterlist$control = as.factor(masterlist$control)
masterlist$status = as.factor(masterlist$status)
masterlist$`institution type` = as.factor(masterlist$`institution type`)
masterlist$quintile = as.factor(masterlist$quintile)
masterlist$`fee status` = as.factor(masterlist$`fee status`)
masterlist$section21 = as.factor(masterlist$section21)
masterlist$town = as.factor(masterlist$town)
masterlist$municipality = as.factor(masterlist$municipality)
masterlist$`exam authority` = as.factor(masterlist$`exam authority`)
masterlist$`date open` = as.character(masterlist$`date open`)
masterlist$`date closed` = as.character(masterlist$`date closed`)

# convert na's to correct value type
masterlist[masterlist == "N"|masterlist == "N/A"|masterlist == "UNKNOWN"|masterlist == "."] <- NA

# save masterlist
write.csv(masterlist, 'data/masterlist.csv', row.names = FALSE)

# ----------------------------------------------------------------------------------------------------------------------------------------

# ----------------------------------Open public school list eda---------------------------------------------------------------------------
new_names_public_sch = c('emis_no', 'education district', 'circuit', 'school_name',
                         'lolt', 'correspondence language', 'sector', 'control', 'status',
                         'institution type', 'fee status', 'quintile', 'magisterial district',
                         'district council', 'municipality', 'ward', 'subcouncil', 'long', 'lat',
                         'lowest grade', 'highest grade', 'hostel', 'total learner enrolled',
                         'learner grouping', 'focused learner grouping', 'bus route learners',
                         'feeding scheme learners', 'check', 'hostel learners', 'section21',
                         'leased school', 'mobile school', 'plankie school', 'connectivity',
                         'bb speed', 'designated site', 'current tech')
colnames(public_school_list) = new_names_public_sch

# convert na to NO for mobile school, plankie and connectivity
public_school_list$`mobile school`[is.na(public_school_list$`mobile school`)] <- 'NO'
public_school_list$`plankie school`[is.na(public_school_list$`plankie school`)] <- 'NO'
public_school_list$connectivity[is.na(public_school_list$connectivity)] <- 'NO'
public_school_list$`leased school`[is.na(public_school_list$`leased school`)] <- 'NO'

# change variables to appropriate type
public_school_list$circuit = as.factor(public_school_list$circuit)
public_school_list$lolt = as.factor(public_school_list$lolt)
public_school_list$`correspondence language` = as.factor(public_school_list$`correspondence language`)
public_school_list$sector = as.factor(public_school_list$sector)
public_school_list$control = as.factor(public_school_list$control)
public_school_list$status = as.factor(public_school_list$status)
public_school_list$`institution type` = as.factor(public_school_list$`institution type`)
public_school_list$`fee status` = as.factor(public_school_list$`fee status`)
public_school_list$quintile = as.factor(public_school_list$quintile)
public_school_list$hostel = as.factor(public_school_list$hostel)
public_school_list$check = as.factor(public_school_list$check)
public_school_list$section21 = as.factor(public_school_list$section21)
public_school_list$`leased school` = as.factor(public_school_list$`leased school`)
public_school_list$`mobile school` = as.factor(public_school_list$`mobile school`)
public_school_list$`plankie school` = as.factor(public_school_list$`plankie school`)
public_school_list$connectivity = as.factor(public_school_list$connectivity)
public_school_list$`bb speed` = as.factor(public_school_list$`bb speed`)
public_school_list$`designated site` = as.factor(public_school_list$`designated site`)
public_school_list$`current tech` = as.factor(public_school_list$`current tech`)
public_school_list$`district council` = as.factor(public_school_list$`district council`)
public_school_list$municipality = as.factor(public_school_list$municipality)
public_school_list$`learner grouping` = as.factor(public_school_list$`learner grouping`)

# converting nas to correct type
public_school_list$`current tech`[public_school_list$`current tech` == 0] <- NA

# convert unknown types to known types
public_school_list$`focused learner grouping`[public_school_list$`focused learner grouping` == "≥150"] <- ">=150" 

# convert na to 0 for bus route learners, feeding scheme and hostel learners
public_school_list$`bus route learners`[is.na(public_school_list$`bus route learners`)] <- 0
public_school_list$`feeding scheme learners`[is.na(public_school_list$`feeding scheme learners`)] <- 0
public_school_list$`hostel learners`[is.na(public_school_list$`hostel learners`)] <- 0

# save public school list
write.csv(public_school_list, 'data/open_school_list.csv', row.names = FALSE)

# ---------------------------------------------------------------------------------------------------

# --------------------------------------------merging the datasets-----------------------------------
# merge using emis_no
merged_masterlist = merge(public_school_list, masterlist, by='emis_no', all.x=T)
# remove useless variables
merged_masterlist = merged_masterlist %>% select(emis_no:`current tech`)

write.csv(merged_masterlist, 'data/merged_masterlist.csv', row.names = F)
# -----------------------------------------------------------------------------------------------------


