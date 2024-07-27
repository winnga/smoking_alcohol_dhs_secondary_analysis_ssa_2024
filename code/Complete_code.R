
# Load necessary libraries
library(tidyverse)
library(readxl)
ssa_tobacco_alcohol_use_dsh_extraction_2024_davis <- read_excel("data/ssa_tobacco_alcohol_use_dsh_extraction_2024_davis.xlsx", 
                                                                skip = 1)
# Create the data frame
data <- ssa_tobacco_alcohol_use_dsh_extraction_2024_davis

# Print the data frame
print(data)


#Descriptive statistics
# Summary statistics for tobacco usage
tobacco_summary <- data %>%
  summarise(
    avg_women_tobacco_any = mean(womentobaccoany, na.rm = TRUE),
    avg_men_tobacco_any = mean(mentobaccoany, na.rm = TRUE)
  )

print(tobacco_summary)

# Summary statistics for alcohol consumption (assuming the columns for alcohol are similarly named as given in the image)
alcohol_summary <- data %>%
  summarise(
    avg_alco1drinkwomen = mean(alco1drinkwomen, na.rm = TRUE),
    avg_alco2drinkwomen = mean(alco2drinkwomen, na.rm = TRUE),
    avg_alco1drinkmen = mean(alco1drinkmen, na.rm = TRUE),
    avg_alco2drinkmen = mean(alco2drinkmen, na.rm = TRUE)
  )

print(alcohol_summary)


#Data visualisations
# Bar plot for tobacco usage
ggplot(data, aes(x = Country)) +
  geom_bar(aes(y = womentobaccoany), stat = "identity", fill = "blue", alpha = 0.5) +
  geom_bar(aes(y = mentobaccoany), stat = "identity", fill = "red", alpha = 0.5) +
  labs(y = "Tobacco Usage (%)", title = "Tobacco Usage by Country and Gender") +
  theme_minimal()

# Bar plot for alcohol consumption
ggplot(data, aes(x = Country)) +
  geom_bar(aes(y = alco1drinkwomen), stat = "identity", fill = "blue", alpha = 0.5) +
  geom_bar(aes(y = alco1drinkmen), stat = "identity", fill = "red", alpha = 0.5) +
  labs(y = "Alcohol Consumption (1 drink)", title = "Alcohol Consumption by Country and Gender") +
  theme_minimal()


#Forest plot
# install.packages("meta")
# install.packages("metafor")

# Load necessary libraries
library(tidyverse)
library(meta)
library(metafor)


# Convert proportions to percentages
data <- data %>%
  mutate(
    womentobaccoany = womentobaccoany / 100,
    mentobaccoany = mentobaccoany / 100
  )

#Forest plot data
# Calculate standard errors for proportions
data <- data %>%
  mutate(
    se_womentobaccoany = sqrt((womentobaccoany * (1 - womentobaccoany)) / Women),
    se_mentobaccoany = sqrt((mentobaccoany * (1 - mentobaccoany)) / Men)
  )

# Prepare data for the forest plot
forest_data <- data %>%
  select(c(studyid,Country, Survey,Men,Women,womentobaccoany, se_womentobaccoany, mentobaccoany, se_mentobaccoany)) 


# Print the prepared data
print(forest_data)

#Forest plot
forest_meta_women <- forest_data %>% 
  filter(!is.na(Women)) %>% 
  filter(!is.na(womentobaccoany)) %>% 
  filter(Women >0) %>% 
  select(c(studyid,Country, Survey,Women,womentobaccoany, se_womentobaccoany)) %>% 
  rename(Studyid=studyid)

  

# Forest plot for women
# Calculate the logit-transformed proportions and their variances
forest_meta_women$yi <- escalc(measure="PLO", xi=forest_meta_women$womentobaccoany * forest_meta_women$Women, 
                               ni=forest_meta_women$Women, data=forest_meta_women)$yi
forest_meta_women$vi <- forest_meta_women$se_womentobaccoany^2

# Fit the random-effects model
res <- rma(forest_meta_women$yi, forest_meta_women$vi, data=forest_meta_women)

# Create the forest plot
forest(res, slab=forest_meta_women$Survey)


# Forest plot for men
forest_meta_men <- forest_data %>% 
  filter(!is.na(Men)) %>% 
  filter(!is.na(mentobaccoany)) %>% 
  filter(Men >0) %>% 
  select(c(studyid,Country, Survey,Men,mentobaccoany, se_mentobaccoany)) %>% 
  rename(Studyid=studyid)


# Calculate the logit-transformed proportions and their variances
forest_meta_men$yi <- escalc(measure="PLO", xi=forest_meta_men$mentobaccoany * forest_meta_men$Men, 
                               ni=forest_meta_men$Men, data=forest_meta_men)$yi
forest_meta_men$vi <- forest_meta_men$se_mentobaccoany^2

# Fit the random-effects model
res <- rma(forest_meta_men$yi, forest_meta_men$vi, data=forest_meta_men)

# Create the forest plot
forest(res, slab=forest_meta_men$Survey)

# Adjust plotting settings for large datasets
forest(res, slab=forest_meta_men$Studyid, cex=0.6, xlim=c(-5, 5), ylim=c(-1, nrow(forest_meta_men)+3))

# Add a title and axis labels
title(main="Forest Plot of Women Tobacco Use by Study", line=1)
mtext("Study ID", side=2, line=4)
mtext("Effect Size (logit transformed proportion)", side=1, line=3)
