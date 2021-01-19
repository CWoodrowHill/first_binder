library(tidyverse) # Load the tidyverse packages
library(afex) # ANOVA functions
library(emmeans) # Needed for pairwise comparisons
library(Hmisc) #needed for stat_summary code in graph

my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/12_glm_anova_pt2/master/data/ancova_data.csv")
head(my_data)

# Code Condition as a factor #
my_data <- my_data %>% 
  mutate(Condition = factor(Condition))
head(my_data)

# Summary stats #
my_data %>%
  group_by(Condition) %>%
  summarise(mean_ability = mean(Ability))

# Visualise #
set.seed(1234)
ggplot(my_data, aes(x = Gaming, y = Ability,  colour = Condition)) + 
  geom_point(size = 3, alpha = .9) +
  labs(x = "Gaming Frequency (hours per week)", 
       y = "Motor Ability") +
  theme_minimal() +
  theme(text = element_text(size = 11)) 

# ANOVA model #
anova_model <-aov_4(Ability ~ Condition + (1 | Participant), data = my_data)
anova(anova_model)

# Run pairwise comparisons #
emmeans(anova_model, pairwise ~ Condition)

# Build ANCOVA model #
model_ancova <- aov_4(Ability ~ Gaming + Condition + (1 | Participant), 
                      data = my_data, factorize = FALSE)
#The covariate goes before the experimental condition manipulation.
#factorize = FALSE means Gaming will be treated as a continuous predictor rather than an 
 #experimental factor.
anova(model_ancova)
#No longer have an effect of Condition but have one of covariate

# Produce adjusted means #
emmeans(model_ancova, pairwise ~ Condition)
#Means taking into account the covariate

## ANOVA & ANCOVA AS REGRESSION ##

# Visualise the data #
my_data %>%
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .05, alpha = .8) +
  labs(x = "Condition", 
       y = "Motor Ability") +
  stat_summary(fun.data = mean_cl_boot, colour = "black") +
  guides(colour = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 12)) 

# Set up contrasts #
contrasts(my_data$Condition)

# Code Water as intercept to act as reference #
my_data <- my_data %>%
  mutate(Condition = fct_relevel(Condition, 
                                 levels = c("Water", "Double Espresso", "Single Espresso")))
contrasts(my_data$Condition)

# Build a linear model #
model_lm <- lm(Ability ~ Condition, data = my_data)
model_lm
#Intercept = mean of Water condition. Can work out the mean of the other two conditions using 
 #the following:
#Ability = Intercept + β1(Double Espresso) + β2(Single Espresso)
#Ability = 4.817 + 4.199(1) + 1.871(0)
#Ability = 4.817 + 4.199
#Ability = 9.016
#To work out the mean Ability of our Double Espresso Group, we use the coding for the 
 #Double Espresso group (0, 1) with our equation:
#Ability = 4.817 + 4.199(0) + 1.871(1)
#Ability = 4.817 + 1.871
#Ability = 6.688

# Build ANCOVA linear model #
model_ancova <- lm(Ability ~ Gaming + Condition, data = my_data)
model_ancova
#Can work out the mean of Water (reference group) by plugging in values to the above equation.
 #As Gaming is not a factor we need to enter the mean of this variable.

# Find the mean of Gaming covariate #
mean(my_data$Gaming)

# Work out adjusted mean of Water #
#Ability = Intercept + β1(Gaming) + β2(Double Espresso) + β3(Single Espresso)
#Ability = -3.4498 + 0.8538(12.62296) + (- 1.0085)(0) + (-0.4563)(0)
#Ability = -3.4498 + 10.777
#Ability = 7.33
#The above matches the ANCOVA we did earlier.

# Center the covariate #
#This standardises the variable with the mean centered on 0
my_scaled_data <- my_data %>%
  mutate(centred_gaming = scale(Gaming))
plot(density(my_scaled_data$Gaming)) #Plot the uncentered data
plot(density(my_scaled_data$centred_gaming)) #Plot the centered data

# Build ANCOVA with the scaled covariate #
model_ancova_centred <- lm(Ability ~ centred_gaming + Condition, data = my_scaled_data)
model_ancova_centred
#Now the intercept matches the adjusted mean for Water. 