setwd("D:/Deepak/OneDrive/OD/Deepak/IITK/Quarter 2/MBA937 - Causal Inference Models/5 Presentation/Fertility")

install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
# install.packages('MatchIt')
install.packages('Matching')
install.packages("tidyverse")
install.packages("modelsummary")
install.packages("fixest")


library(readxl)

fertility_data <- read_excel("Input_Data.xlsx")


library(dplyr)

filtered_data <- filter(fertility_data, Year >= 1973, Year <= 2022)

# Create a dummy variable to identify developed countries
filtered_data$Developed = ifelse(filtered_data$Entity == "France" | 
                                   filtered_data$Entity == "United Kingdom" | 
                                   filtered_data$Entity == "United States", 1, 0)

# Create a dummy variable to fertility level
filtered_data$Fertility_Level = ifelse(filtered_data$Fertility_Rate <2.5, "Fertility_Low", "Fertility_High")

head(filtered_data)
colnames(filtered_data)


filtered_developed <- filter(filtered_data, Entity %in% c("France", "United Kingdom", "United States"))
filtered_developing <- filter(filtered_data, Entity %in% c("Bangladesh", "India", "Pakistan"))


library(ggplot2)

ggplot(filtered_data, aes(x = Year, y = Fertility_Rate, color = Entity)) +
  geom_line() +
  labs(x = "Year", y = "Fertility Rate", title = "Fertility Rate by Year") +
  theme_minimal()

ggplot(filtered_developed, aes(x = Year, y = Fertility_Rate, color = Entity)) +
  geom_line() +
  labs(x = "Year", y = "Fertility Rate", title = "Fertility Rate by Year") +
  theme_minimal()

ggplot(filtered_developing, aes(x = Year, y = Fertility_Rate, color = Entity)) +
  geom_line() +
  labs(x = "Year", y = "Fertility Rate", title = "Fertility Rate by Year") +
  theme_minimal()


###############################################################
#####      Linear Regression Models with Interactions     #####
###############################################################

model_filtered_data <- lm(Fertility_Rate ~ Female_Labor_Participation + Mean_Years_of_Schooling + Child_Mortality_Rate + Unmet_Contraception_Need + (Female_Labor_Participation * Mean_Years_of_Schooling) + (Child_Mortality_Rate * Unmet_Contraception_Need), data = filtered_data)
summary(model_filtered_data)

model_filtered_data_with_developed <- lm(Fertility_Rate ~ Developed + Female_Labor_Participation + Mean_Years_of_Schooling + Child_Mortality_Rate + Unmet_Contraception_Need + (Female_Labor_Participation * Mean_Years_of_Schooling) + (Child_Mortality_Rate * Unmet_Contraception_Need) + (Developed * Female_Labor_Participation) + (Developed * Mean_Years_of_Schooling) + (Developed * Child_Mortality_Rate) + (Developed * Unmet_Contraception_Need), data = filtered_data)
summary(model_filtered_data_with_developed)



model_filtered_developed <- lm(Fertility_Rate ~ Female_Labor_Participation + Mean_Years_of_Schooling + Child_Mortality_Rate + Unmet_Contraception_Need + (Female_Labor_Participation * Mean_Years_of_Schooling) + (Child_Mortality_Rate * Unmet_Contraception_Need), data = filtered_developed)
summary(model_filtered_developed)

model_filtered_developing <- lm(Fertility_Rate ~ Female_Labor_Participation + Mean_Years_of_Schooling + Child_Mortality_Rate + Unmet_Contraception_Need + (Female_Labor_Participation * Mean_Years_of_Schooling) + (Child_Mortality_Rate * Unmet_Contraception_Need), data = filtered_developing)
summary(model_filtered_developing)


###############################################################
#####                       Matching                      #####
###############################################################

library(dplyr)
library(Matching)
# library(MatchIt)

data_for_matching <- filtered_data

colnames(data_for_matching)


# Outcome
Y <- data_for_matching %>% pull(Fertility_Level)

# Treatment
D <- data_for_matching %>% pull(Developed)

# Matching variables
# X <- data_for_matching %>% select(Female_Labor_Participation, Mean_Years_of_Schooling, Child_Mortality_Rate, Unmet_Contraception_Need) %>% as.matrix()
X <- as.matrix(data_for_matching[, c("Female_Labor_Participation", "Mean_Years_of_Schooling", "Child_Mortality_Rate", "Unmet_Contraception_Need")])

# Run Mahalanobis distance matching
M <- Match(Y, D, X, Weight=2,caliper=1)

# Seetreatmenteffectestimate
summary(M)


###############################################################
#####                    Fixed Effects                    #####
###############################################################

library(tidyverse)
library(modelsummary)
library(fixest)

fixed_effect_filtered <- feols(Fertility_Rate ~ Female_Labor_Participation + Mean_Years_of_Schooling + Child_Mortality_Rate + Unmet_Contraception_Need + (Female_Labor_Participation * Mean_Years_of_Schooling) + (Child_Mortality_Rate * Unmet_Contraception_Need) | Entity + Year, data = filtered_data)
msummary(fixed_effect_filtered, stars = c('*' =.1,'**' =.05,'***' =.01))

fixed_effect_developed <- feols(Fertility_Rate ~ Female_Labor_Participation + Mean_Years_of_Schooling + Child_Mortality_Rate + Unmet_Contraception_Need + (Female_Labor_Participation * Mean_Years_of_Schooling) + (Child_Mortality_Rate * Unmet_Contraception_Need) | Entity + Year, data = filtered_developed)
msummary(fixed_effect_developed, stars = c('*' =.1,'**' =.05,'***' =.01))

fixed_effect_developing <- feols(Fertility_Rate ~ Female_Labor_Participation + Mean_Years_of_Schooling + Child_Mortality_Rate + Unmet_Contraception_Need + (Female_Labor_Participation * Mean_Years_of_Schooling) + (Child_Mortality_Rate * Unmet_Contraception_Need) | Entity + Year, data = filtered_developing)
msummary(fixed_effect_developing, stars = c('*' =.1,'**' =.05,'***' =.01))


###############################################################
#####    Difference-In-Difference (DID)  Pre-Post 2003    #####
###############################################################


did_data <- filtered_data

# DID for up to and after year 2002 (as GDP spurt happened from year 2003)

# Create a dummy variable to identify year 2003
did_data$Time = ifelse(did_data$Year > 2002, 1, 0)

# Create a dummy variable to identify Developed
did_data$Treated = ifelse(did_data$Developed == 1, 1, 0)

# Create an interaction between time and treated
did_data$DID = did_data$Time * did_data$Treated

# Estimating the DID estimator (coefficient)
model_did = lm(Fertility_Rate ~ Treated + Time  + DID, data = did_data)
summary(model_did)


# DID for India and Pakistan up to and after year 2002 (as GDP spurt happened from year 2003)

did_data_ind_pak <- filter(did_data, Entity %in% c("India", "Pakistan"))

# Create a dummy variable to identify year 2003
did_data_ind_pak$Time1 = ifelse(did_data_ind_pak$Year > 2002, 1, 0)

# Create a dummy variable to identify India
did_data_ind_pak$Treated1 = ifelse(did_data_ind_pak$Entity == "India", 1, 0)

# Create an interaction between time and treated
did_data_ind_pak$DID1 = did_data_ind_pak$Time1 * did_data_ind_pak$Treated1

# Estimating the DID estimator (coefficient)
model_did_ind_pak = lm(Fertility_Rate ~ Treated1 + Time1 + DID1, data = did_data_ind_pak)
summary(model_did_ind_pak)


# DID for India and Bangladesh up to and after year 2002 (as GDP spurt happened from year 2003)

did_data_ind_ban <- filter(did_data, Entity %in% c("Bangladesh", "India"))

# Create a dummy variable to identify year 2003
did_data_ind_ban$Time2 = ifelse(did_data_ind_ban$Year > 2002, 1, 0)

# Create a dummy variable to identify India
did_data_ind_ban$Treated2 = ifelse(did_data_ind_ban$Entity == "India", 1, 0)

# Create an interaction between time and treated
did_data_ind_ban$DID2 = did_data_ind_ban$Time2 * did_data_ind_ban$Treated2

# Estimating the DID estimator (coefficient)
model_did_ind_ban = lm(Fertility_Rate ~ Treated2 + Time2 + DID2, data = did_data_ind_ban)
summary(model_did_ind_ban)


###############################################################
#####                       END                           #####
###############################################################

