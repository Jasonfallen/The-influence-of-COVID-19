#### Preamble ####
# Purpose: prepare for R
# Author: HaoCheng Xu
# Data: 27 April 2022
# Contact: haocheng.xu@mail.utoronto.ca
# License: MIT




#### Workspace setup ####

# Load packages
library(reshape2)
library(stringi)
library(pointblank)
library(tidyverse)

###tables###

table1 <- table(df1$Age_Group)
knitr::kable(table1, caption = "The total infected people in distinct age group")

table2 <- table(df1$Source_of_Infection)
knitr::kable(table2, caption = "The total infected people from distinct sources of infection")


df1 %>%
  mutate(year_month = format(as.Date(`Reported_Date`), "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=year_month, y = count))+
  geom_point() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))

# Barplot for distribution of outcome for infected people in distinct age groups.
df1 %>%  
  ggplot(mapping = aes(x = `Age_Group`,fill= Outcome)) +
  geom_bar()+
  theme_minimal()+scale_x_discrete(labs(x="Age_Group of infected persons", y="Infected persons",fill="Outcome",title = "Distribution of age-groups, and outcome for infected people, in 2020 in Toronto",caption = "Source: 2020 provincial Case & Contact Management System "))+
  coord_flip()

# Barplot for total people who infected by distinct sources in Toronto.
df1%>%  
  ggplot(mapping = aes(x = `Source_of_Infection`)) +
  geom_bar()+
  theme_minimal()+
  scale_x_discrete(labs(x="Source of infection", y="count",title = "total infected by distinct sources in Toronto since 2020",caption = "Source: 2020 provincial Case & Contact Management System ")) +
  theme(axis.text=element_text(size=8))+coord_flip()

# Pie plot for proportion of outbreak associated and sporadic case of total case.
table(df1$`Outbreak Associated`)
pieplot <- data.frame(x = c("Outbreak Associated", "Sporadic"), p = c(13.25, 86.75))

pieplot %>% 
  ggplot(aes(x=factor(1), y = p, fill= x))+ 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(p, "%")), position = position_stack(vjust=0.5)) +
  labs(x =NULL, y = NULL, fill = NULL) +
  ggtitle("Proportion of outbreak associated and sporadic case of total covid-19 cases")

#tables for number of people who ever or currently in ICU, ever or currently hospitalized, ever or currently intubated in different age groups.
agegroup <- table(df1$Age_Group)

table3 <- df1 %>%
  filter(`Ever_in_ICU` == "Yes"|`Currently_in_ICU`=='Yes') %>%
  group_by(`Age_Group`) %>%
  summarize(count = n()) %>%
  mutate(prop = round(count/agegroup,3))
knitr::kable(table3, caption = "The number and proportion of people ever/currently in ICU in different age group")

table4 <- df1 %>%
  filter(`Ever_Hospitalized` == "Yes"|`Currently_Hospitalized`=='Yes') %>%
  group_by(`Age_Group`)%>%
  summarize(count = n()) %>%
  mutate(prop = round(count/agegroup,3))
knitr::kable(table4, caption = "The number and proportion of people ever/currently hospitalized in different age group")

table5 <- df1 %>%
  filter(`Ever_Intubated` == "Yes"|`Currently_Intubated`=='Yes') %>%
  group_by(`Age_Group`)%>%
  summarize(count = n()) %>%
  mutate(prop = round(count/agegroup,3))
knitr::kable(table5, caption = "The number and proportion of people ever/currently intubated in different age group")

# Log regression for age,sex,source of infection and ever in ICU.
lm_df1 <- df1 %>%
  mutate(ICU = as.numeric((`Currently_in_ICU` == "Yes" | `Ever_in_ICU` == "Yes" )))
model = lm(ICU ~ `Age_Group` + `Source_of_Infection` + `Client_Gender`, data = lm_df1)

model_logit <- glm(ICU ~ `Age_Group` + `Source_of_Infection` + `Client_Gender`, family = "binomial", data = lm_df1) 
knitr::kable(summary(model_logit)$coefficients, caption = "The coefficient estimates and their P-values of logistic regression")

exp(4.14) # 80-89 age group has 62.8 fold of odds of in ICU compared to the under 19 age group
exp(0.569) # males has 1.77 fold of odds of in ICU compared to females
exp(4.36238) # 70-79 age group has 62.8 fold of odds of in ICU compared to the under 19 age group
exp(0.25910) # infected from community group has 1.3 fold of odds of in ICU compared to the infected from close contact group
exp(-1.03445) # infected from healthcare group has 1.3 fold of odds of in ICU compared to the infected from close contact group