#=================================================================
# Install Packages and Set Directory
#=================================================================

####Install####
list_packages = c("tidyverse","data.table")
install.packages(list_packages)
library(tidyverse)
library(data.table)
rm(list = ls())
wd = "C:/Users/Qi zixuan/Desktop/Econ 613/Assignment 1"
setwd(wd)

#=================================================================
# Import Data
#=================================================================

####Import####
file_dathh = list.files("dathh")  
dir_dathh = paste("./dathh/", file_dathh, sep="")  
n_dathh = length(dir_dathh)

file_datind = list.files("datind")  
dir_datind = paste("./datind/", file_datind, sep="")  
n_datind = length(dir_datind)

datalist_dathh <- vector("list", n_dathh)
datalist_datind <- vector("list", n_datind)

for (i in 1:16){
  datalist_dathh[[i]] <- fread(dir_dathh[i], header = T)
  datalist_datind[[i]] <- fread(dir_datind[i], header = T)
  datalist_datind[[i]]$idind <- as.character(datalist_datind[[i]]$idind)
  datalist_dathh[[i]]$idind <- as.character(datalist_dathh[[i]]$idind)
  datalist_datind[[i]]$idmen <- as.character(datalist_datind[[i]]$idmen)
  datalist_dathh[[i]]$idmen <- as.character(datalist_dathh[[i]]$idmen)
}

#=================================================================
# Exercise 1: Basic Statistics
#=================================================================

####Question 1####
# Define the data dathh2007 
# Rename the first column
# Change the form of idmen as character
# Delete the same identified idmen
# Calculate the number of identified id
dathh2007 <- datalist_dathh[[4]] %>%
  rename(Rows = 'V1') %>%
  mutate(idmen = as.character(idmen)) %>%
  distinct(idmen, .keep_all = TRUE)
Num_hh_2007 <- dim(dathh2007)[1]
Num_hh_2007

####Question 2####
# Define the data dathh2005_cou_Kid 
# Filter when the mstatus == couple, with kids
# Change the form of idmen as character
# Delete the same identified idmen
# Calculate the number of identified id
dathh2005_cou_kid <- datalist_dathh[[2]] %>%
  filter(mstatus == "Couple, with Kids") %>%
  mutate(idmen = as.character(idmen)) %>%
  distinct(idmen, .keep_all = TRUE)
Num_hh_2005_cou_kid <- dim(dathh2005_cou_kid)[1]
Num_hh_2005_cou_kid

####Question 3####
# Define the data datind2008 
# Rename the first column
# Change the forms of idmen and idind as character
# Delete the same identified idind
# Calculate the number of identified idind
datind2008 <- datalist_datind[[5]] %>%
  rename(Rows = 'V1') %>%
  mutate(idmen = as.character(idmen),
         idind = as.character(idind)) %>%
  distinct(idind, .keep_all = TRUE)
Num_ind_2008 <- dim(datind2008)[1]
Num_ind_2008

####Question 4####
# Define the data dathh2006_age 
# Filter when the age is from 25 to 35
# Change the form of idmen and idind as character
# Delete the same identified idind
# Calculate the number of identified idind
datind2016_age <- datalist_datind[[13]] %>%
  filter(age >= 25 & age <= 35) %>%
  mutate(idind = as.character(idind),
         idmen = as.character(idmen)) %>%
  distinct(idind, .keep_all = TRUE)
Num_ind_2016_age <- dim(datind2016_age)[1]
Num_ind_2016_age

####Question 5####
# Construct cross table 
datind2009_cross_gp <- table(datalist_datind[[6]]$gender,
                             datalist_datind[[6]]$profession)
print(datind2009_cross_gp)

####Question 6####
# Ginn Coefficient: The difference between the income share of  
# the top 20% of earners and the income share of the bottom 20% 
# Note: 10% quantile of wage is equal to 0, thus ratio is infinity
datind2005_STA <- datalist_datind[[2]] %>%
  drop_na(wage) %>%
  summarise(mean_wage = mean(wage),
            sd_wage   = sd(wage),
            ratio     = quantile(wage,0.9)/quantile(wage,0.1),
            Ginn      = sum(wage[wage >= quantile(wage,0.8)])/sum(wage)-
                        sum(wage[wage <= quantile(wage,0.2)])/sum(wage))

datind2019_STA <- datalist_datind[[16]] %>%
  drop_na(wage) %>%
  summarise(mean_wage = mean(wage),
            sd_wage   = sd(wage),
            ratio     = quantile(wage,0.9)/quantile(wage,0.1),
            Ginn      = sum(wage[wage >= quantile(wage,0.8)])/sum(wage)-
                        sum(wage[wage <= quantile(wage,0.2)])/sum(wage))

####Question 7####
datind2010 <- datalist_datind[[7]]
# Combined histogram 
p1 <- ggplot(datind2010,aes(age,fill=gender))+geom_histogram()
# Separable histogram
p2 <- ggplot(datind2010,aes(age,fill=gender))+
  geom_histogram()+
  facet_wrap(~gender)
# Save figures
ggsave(p1,filename = "p1.png")
ggsave(p2,filename = "p2.png")

####Question 8####
dathh2011 <- datalist_dathh[[8]] %>%
  mutate(idmen = as.character(idmen))
# Merge datind2011 and dathh2011 to filter the Paris and
# delete the repeated identified idind and sum the number of rows
datind2011_paris <- datalist_datind[[8]] %>%
  mutate(idind = as.character(idind),
         idmen = as.character(idmen)) %>%
  left_join(dathh2011, by='idmen') %>%
  filter(location == 'Paris') %>%
  distinct(idind.x, .keep_all = TRUE)
Num_ind_2007_paris <- dim(datind2011_paris)[1]
Num_ind_2007_paris

#=================================================================
# Exercise 2: Merge Datasets
#=================================================================

####Question 1####
datind_all <- datalist_datind[[1]]
for (i in 2:16){
  datind_all <- rbind(datind_all, datalist_datind[[i]])
}
datind_all <- datind_all %>%
  rename(Rows = 'V1') %>%
  mutate(idind = as.character(idind),
         idmen = as.character(idmen))
head(datind_all,5)

####Question 2####
dathh_all <- datalist_dathh[[1]]
for (i in 2:16){
  dathh_all <- rbind(dathh_all, datalist_dathh[[i]])
}
dathh_all <- dathh_all %>%
  rename(Rows = 'V1') %>%
  mutate(idmen = as.character(idmen))
head(dathh_all,5)

####Question 3####
simu_pre <- semi_join(dathh_all,datind_all)

####Question 4####
datind_hh_all <- datind_all %>%
  full_join(dathh_all, by=c('idmen','year')) %>%
  rename(idind = "idind.x",
         Rows  = "Rows.x") %>%
  select(1:10,12:16)
head(datind_hh_all,2)

####Question 5####
Num_hh_four <- datind_hh_all %>%
  group_by(idmen, year) %>%
  #distinct(idind, .keep_all = TRUE) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count > 4) %>%
  distinct(idmen, .keep_all = TRUE)
Num_hh_four_row <- dim(Num_hh_four)[1]
Num_hh_four_row

####Question 6####
Num_hh_unem <- datind_hh_all %>%
  group_by(idmen, year) %>%
  #distinct(idind, .keep_all = TRUE) %>%
  #mutate(count_unem = sum(empstat == 'Unemployed')) %>%
  filter(empstat == 'Unemployed') %>%
  ungroup() %>%
  #filter(count_unem >= 1) %>%
  distinct(idmen, .keep_all = TRUE)
Num_hh_unem_row <- dim(Num_hh_unem)[1]
Num_hh_unem_row

####Question 7####
Num_hh_sam_pro <- datind_hh_all %>%
  drop_na(profession) %>%
  group_by(idmen, year, profession) %>%
  #distinct(idind, .keep_all = TRUE) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count > 1) %>%
  distinct(idmen, .keep_all = TRUE)
Num_hh_sam_pro_row <- dim(Num_hh_sam_pro)[1]
Num_hh_sam_pro_row

####Question 8####
Num_ind_cou_kids <- datind_hh_all %>%
  filter(mstatus == "Couple, with Kids") %>%
  distinct(idind, .keep_all = TRUE)
Num_ind_cou_kids_row <- dim(Num_ind_cou_kids)[1]
Num_ind_cou_kids_row

####Question 9####
Num_ind_paris <- datind_hh_all %>%
  filter(location == "Paris") %>%
  distinct(idind, .keep_all = TRUE)
Num_ind_paris_row <- dim(Num_ind_paris)[1]
Num_ind_paris_row

####Question 10####
Most_ind <- datind_hh_all %>%
  group_by(idmen, year) %>%
  distinct(idind, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by(idmen) %>%
  mutate(count = n()) %>%
  arrange(desc(count)) %>%
  distinct(idmen, .keep_all = TRUE)
Most_ind$idmen[1]

####Question 11####
num_hh_2010_2011 <- datind_hh_all %>%
  filter(year == 2010 | year == 2011) %>%
  group_by(year) %>%
  distinct(idmen, .keep_all = TRUE) %>%
  ungroup()
num_hh_2010_2011_row <- dim(num_hh_2010_2011)[1]
num_hh_2010_2011_row


#=================================================================
# Exercise 3: Migration
#=================================================================

####Question 1####
enter_and_exist <- datind_hh_all %>%
  arrange(idmen, year) %>%
  group_by(idmen) %>%
  mutate(interval = max(year) - min(year)) %>%
  ungroup() %>%
  distinct(idmen, .keep_all = TRUE)
p3 <- ggplot(enter_and_exist, aes(x=interval)) + 
      geom_histogram(fill="steelblue")
ggsave(p3,filename = "p3.png")

####Question 2####
move_or_not <- datind_hh_all %>%
  drop_na(datent)
move_or_not$dwelling <- ifelse(move_or_not$year == move_or_not$datent, 
                               1, 0)
move_or_not <- move_or_not %>%
  group_by(year) %>%
  mutate(share = sum(dwelling != 0)/sum(dwelling >= 0)) %>%
  ungroup() %>%
  select(2:4, 11, 16, 17)
head(move_or_not, 10)
move_or_not_share <- move_or_not %>%
  distinct(year, .keep_all = TRUE)
p4 <- ggplot(move_or_not_share, aes(x=year, y=share)) + 
      geom_line()
ggsave(p4,filename = "p4.png")

####Question 3####
migrate_myear <- datind_hh_all %>%
  filter(year <= 2014) %>%
  filter(!is.na(myear)) 
# if myear is equal to year, then move = 2
migrate_myear$move <- ifelse(migrate_myear$year == migrate_myear$myear, 
                             2, 1)
migrate_move <- datind_hh_all %>%
  filter(year > 2014) %>%
  filter(!is.na(move))  
#merge move and myear data to complement the missing move data
migrate <- rbind(migrate_myear, migrate_move)
migrate <- migrate %>%
  group_by(year) %>%
  mutate(share = sum(move == 2)/sum(move > 0)) %>%
  ungroup() %>%
  select(2:4, 12, 14, 16)
head(migrate, 10)
migrate_share <- migrate %>%
  distinct(year, .keep_all = TRUE)
p5 <- ggplot(migrate_share, aes(x=year, y=share)) + 
  geom_line()
ggsave(p5,filename = "p5.png")

####Question 4####
comp_share <- move_or_not_share %>%
  full_join(migrate_share, by = "year") %>%
  select(3,6,11)
p6 <- ggplot(comp_share) + 
      geom_line(mapping = aes(x=year, y=share.x, color = "dwelling")) +
      geom_line(mapping = aes(x=year, y=share.y, color = "migration"))
ggsave(p6,filename = "p6.png")

####Question 5####
#filter households who migrate
move_data <- datind_hh_all %>%
  drop_na(datent)
move_data$dwelling <- ifelse(move_data$year == move_data$datent, 
                               1, 0)
#compare the profession/empstat this year with previous year
move_data <- move_data %>%
  group_by(idmen) %>%
  mutate(check = ifelse(1 %in% dwelling, 1, 0)) %>%
  ungroup() %>%
  filter(check == 1) %>%
  filter(!is.na(profession) | !is.na(empstat)) %>%
  group_by(idmen, year) %>%
  distinct(idind, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(idmen, idind) %>%
  group_by(idmen,idind) %>%
  mutate(lag_pro = lag(profession, 1, order_by = year),
         lag_emp = lag(empstat, 1, order_by = year)) %>%
  ungroup() %>%
  mutate(change = ifelse(profession != lag_pro|empstat != lag_emp,
                           1, 0)) %>%
  group_by(idmen) %>%
  mutate(at_least = sum(!is.na(change) & change == 1)) %>%
  ungroup() %>%
  filter(at_least >= 1) %>%
  distinct(idmen, .keep_all = TRUE)
move_data_row <- dim(move_data)[1]
move_data_row

#=================================================================
# Exercise 4: Attrition
#=================================================================

####Question####
# calculate the number of reducing and increasing individuals each year
# then conclude proportion
# delete the year 2004 and 2019
red_inc <- datind_hh_all %>%
  arrange(idind, year) %>%
  group_by(idind) %>%
  mutate(max_year = max(year),
         min_year = min(year)) %>%
  ungroup() %>%
  distinct(idind, year, .keep_all = TRUE)
attrition <- datind_hh_all %>%
  left_join(red_inc, by = "idind") %>%
  mutate(leave = ifelse(year.x == max_year, 1, 0),
         entry = ifelse(year.x == min_year, 1, 0)) %>%
  filter(year.x != 2004 & year.x != 2019) %>%
  group_by(year.x) %>%
  mutate(count_leave = sum(leave == 1),
         count_entry = sum(entry == 1),
         proportion  = count_leave/count_entry) %>%
  ungroup() %>%
  distinct(year.x, .keep_all = TRUE) %>%
  rename(year = "year.x") %>%
  select(4,36)
attrition