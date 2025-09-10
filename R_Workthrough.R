#########################
###  Census Analysis  ###
#########################

# This script will be used to clean and analyse the census data.
# It will help identify any problems in the data and try to solve them.
# New and cleaned census files will be saved for future use.

################################################################################

## Required Packages

library(readr)
library(knitr)
library(dplyr)
library(tidyverse)
library(MASS)
library(MuMIn)
library(car)
library(patchwork)
library(broom)
library(lmerTest)
library(performance)
library(boot)
library(broom.mixed)


## Load Data

# Raw (anonymised) data
CensusData <- read.csv("Data/CensusData.csv")



################################################################################
###############################  DATA CLEANING  ################################
################################################################################


################################
###  STEP 0: Tracking Setup  ###
################################

# Initialize tracking
tracking <- data.frame(
  Step = character(),
  Households = numeric(),
  Excluded = numeric(),
  stringsAsFactors = FALSE
)

update_tracking <- function(step_name, current_data, prev_data) {
  n_current <- nrow(current_data)
  n_prev <- if (is.null(prev_data)) n_current else nrow(prev_data)
  excluded <- n_prev - n_current
  tracking <<- rbind(tracking, data.frame(Step = step_name, Households = n_current, Excluded = excluded))
}


#######################################
###  STEP 1: Fix Enumerator Errors  ###
#######################################

# Identify enumerators who have misrecorded the HH_size (i.e., under_18 > HH_size)
CensusData_prev <- CensusData
CensusData$enumrtr <-str_to_title(CensusData$enumrtr)
misrecorded_enum <- unique(CensusData$enumrtr[CensusData$under_18 > CensusData$HH_size])

# Create new columns for "adults", "under_18_true", and "HH_size_true"
CensusData <- CensusData %>%
  mutate(
    adults = ifelse(enumrtr %in% misrecorded_enum, HH_size, HH_size - under_18),
    HH_size = adults + under_18,
    Urb_V_Rur = ifelse(lga == "Musoma MC", "Urban", "Rural") %>% as.factor(),
    TotalDogs = dogs + puppies  # Create TotalDogs upfront
  )

# Household Dog Ownership
CensusData$owns_dogs <- "Yes"
CensusData$owns_dogs[which(CensusData$TotalDogs==0)] <- "No"

update_tracking("1. Fixing Household Sizes", CensusData, CensusData_prev)

############################################
###  STEP 2: Enumerator Quality Control  ###
############################################

# Step 2a: Remove enumerators with <10 households
CensusData_prev <- CensusData
CensusData <- CensusData %>%
  group_by(enumrtr, village) %>%
  filter(n() >= 10) %>%
  ungroup()

update_tracking("2a. Remove Enumerators (<10 HHs)", CensusData, CensusData_prev)

# Step 2b: Remove enumerators who ONLY surveyed dog-owning HHs
CensusData_prev <- CensusData
CensusData <- CensusData %>%
  group_by(enumrtr) %>%
  filter(any(TotalDogs == 0)) %>%  # Ensure at least one HH without dogs
  ungroup()

update_tracking("2b. Remove Dog-Biased Enumerators", CensusData, CensusData_prev)

# Step 2c: Remove Subvillages with less than 5 Households
CensusData_prev <- CensusData
CensusData <- CensusData %>%
  group_by(lga, ward, village, subvillage, enumrtr) %>%
  filter(n() >= 5) %>%
  ungroup()

update_tracking("2c. Remove Subvillages with less than 5 Households", CensusData, CensusData_prev)


##################################
###  STEP 3: Spatial Cleaning  ###
##################################

# Step 3a: Remove non-unique coordinates
CensusData_prev <- CensusData
CensusData <- CensusData %>%
  group_by(latitude, longitude) %>%
  filter(n() == 1) %>%  # Keep only unique coordinates
  ungroup()

update_tracking("3a. Remove Duplicate Coordinates", CensusData, CensusData_prev)


########################################
###  Step 3b: Split spatial filters  ###
########################################

CensusData_prev <- CensusData
CensusData <- CensusData %>%
  filter(latitude > -2.2)  # 4b.1: Remove implausible coordinates
update_tracking("3b.1 Latitude Filter", CensusData, CensusData_prev)

CensusData_prev <- CensusData
CensusData <- CensusData %>%
  filter(precision < 25)  # 4b.2: Remove low-precision records
update_tracking("3b.2 Precision Filter", CensusData, CensusData_prev)

CensusData_prev <- CensusData
CensusData <- CensusData %>%
  filter(Land.Use != "Water")  # 4b.3: Remove water land use
update_tracking("3b.3 Land Use Filter", CensusData, CensusData_prev)


############################################
###  STEP 4: Enumerator Quality Control  ###
############################################

nsusData_prev <- CensusData
CensusData <- CensusData %>%
  filter(HH_size > 0)

update_tracking("4. Remove Empty Households", CensusData, CensusData_prev)


#################################
###  STEP 5: Outlier Removal  ###
#################################

remove_outliers <- function(data, var) {
  non_zero <- data[[var]] > 0
  if (sum(non_zero) == 0) return(data)

  z_scores <- scale(data[[var]][non_zero])
  outliers <- which(abs(z_scores) > 3)
  data[[var]][non_zero][outliers] <- NA
  data %>% filter(!is.na(.[[var]]))
}

CensusData_prev <- CensusData
CensusData <- CensusData %>%
  remove_outliers("adults") %>%
  remove_outliers("under_18") %>%
  remove_outliers("dogs") %>%
  remove_outliers("puppies")

update_tracking("5. Remove Outliers", CensusData, CensusData_prev)


## Data cleaning steps + removals
tracking  # These may not exactly resemble those seen in the paper due to the random jittering
sum(tracking$Excluded)


################################################################################
###############################  DATA ANALYSIS  ################################
################################################################################

## Basic Stats (These do not exactly resemble those seen in the paper due to the random jittering)

# Total Households
TotalHH <- nrow(CensusData)
TotalHH                   # 27264

# New Maximums
max(CensusData$adults)    # 14
max(CensusData$under_18)  #  9
max(CensusData$HH_size)   # 23

max(CensusData$dogs)      #  5
max(CensusData$puppies)   #  8
max(CensusData$TotalDogs) # 13

#Total Humans
sum(CensusData$adults)  # 123024  Adults
sum(CensusData$under_18)  # 84326  Under 18s
sum(CensusData$HH_size)  # 207350  Humans

# Total Dogs
sum(CensusData$dogs)  # 21912  Adult Dogs
sum(CensusData$puppies)  # 5831  Puppies
CensusData$TotalDogs <- CensusData$dogs + CensusData$puppies
sum(CensusData$TotalDogs)  # 27743  Dogs

#Land Cover Households
LU_HHs <-CensusData %>%
  group_by(Land.Use) %>%
  summarise(
    total_households = n(),
    percentage = n() / 27400 *100)
LU_HHs


##################
###  FIGURE 3  ###
##################

# Calculate maximum y-axis value
max_y <- max(
  max(hist(CensusData$HH_size, breaks = seq(-1, 24, by = 1), plot = FALSE)$counts),
  max(hist(CensusData$TotalDogs, breaks = seq(-1, 24, by = 1), plot = FALSE)$counts)
)

# Custom theme to match map figures
theme_hist <- theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, vjust = 0.5),
    axis.title = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(5, 10, 5, 10),
    plot.tag = element_text(face = "bold", size = 12)
  )

# Histogram creation function (ggplot version)
create_gg_hist <- function(data, breaks, xlab, threshold_q = 0.95) {
  # Calculate histogram data
  h <- hist(data, breaks = breaks, plot = FALSE)

  # Get upper bounds (for labels) and midpoints (for breaks)
  upper_bounds <- h$breaks[-1]
  midpoints <- h$mids

  # Create labels: show even numbers, hide odd ones
  x_labels <- ifelse(upper_bounds %% 2 == 0, as.character(upper_bounds), "")

  # Create plot data
  plot_data <- data.frame(
    x = midpoints,
    count = h$counts,
    upper = upper_bounds
  )

  # Calculate threshold
  threshold <- quantile(data, threshold_q, na.rm = TRUE)

  ggplot(plot_data, aes(x = x, y = count)) +
    geom_col(
      aes(fill = upper > threshold),
      width = 0.9,
      color = "black",
      linewidth = 0.2
    ) +
    scale_x_continuous(
      breaks = midpoints,       # Keep breaks at all midpoints (for grid lines)
      labels = x_labels,       # Show labels only for even numbers
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = c("gray70", "gray70"), guide = "none") +
    scale_y_continuous(limits = c(0, max_y)) +
    labs(x = xlab, y = "Number of Households") +
    theme_hist
}

# Create plots
plot_3a <- create_gg_hist(CensusData$HH_size,
                     seq(-1, 24, by = 1),
                     "Humans Per Household")

plot_3b <- create_gg_hist(CensusData$TotalDogs,
                     seq(-1, 24, by = 1),
                     "Dogs Per Household")

# Combine using patchwork
plot_3 <- plot_3a + plot_3b +
  plot_layout(nrow = 1) +
  plot_annotation(tag_levels = 'A')

plot_3


################################################################################

########################
###  Human Analysis  ###
########################


## Basic Statistics for Human Population
human_stats <- CensusData %>%
  group_by() %>%
  summarise(
    total_households = n(),
    total_adults = sum(adults, na.rm = TRUE),
    avg_adults = mean(adults, na.rm = TRUE),
    q1_adults = quantile(adults, probs = 0.25, na.rm = TRUE),
    median_adults = quantile(adults, probs = 0.50, na.rm = TRUE),
    q3_adults = quantile(adults, probs = 0.75, na.rm = TRUE),
    total_children = sum(under_18, na.rm = TRUE),
    avg_children = mean(under_18, na.rm = TRUE),
    q1_children = quantile(under_18, probs = 0.25, na.rm = TRUE),
    median_children = quantile(under_18, probs = 0.50, na.rm = TRUE),
    q3_children = quantile(under_18, probs = 0.75, na.rm = TRUE),
    avg_hh_size = mean(HH_size, na.rm = TRUE),
    q1_hh_size = quantile(HH_size, probs = 0.25, na.rm = TRUE),
    median_hh_size = quantile(HH_size, probs = 0.50, na.rm = TRUE),
    q3_hh_size = quantile(HH_size, probs = 0.75, na.rm = TRUE)
  )


## Urban/Rural Statistics for Human Population
UR_human_stats <- CensusData %>%
  group_by(Urb_V_Rur) %>%
  summarise(
    total_households = n(),
    total_perc = n() / TotalHH * 100,
    total_adults = sum(adults, na.rm = TRUE),
    avg_adults = mean(adults, na.rm = TRUE),
    q1_adults = quantile(adults, probs = 0.25, na.rm = TRUE),
    median_adults = quantile(adults, probs = 0.50, na.rm = TRUE),
    q3_adults = quantile(adults, probs = 0.75, na.rm = TRUE),
    total_children = sum(under_18, na.rm = TRUE),
    avg_children = mean(under_18, na.rm = TRUE),
    q1_children = quantile(under_18, probs = 0.25, na.rm = TRUE),
    median_children = quantile(under_18, probs = 0.50, na.rm = TRUE),
    q3_children = quantile(under_18, probs = 0.75, na.rm = TRUE),
    avg_hh_size = mean(HH_size, na.rm = TRUE),
    q1_hh_size = quantile(HH_size, probs = 0.25, na.rm = TRUE),
    median_hh_size = quantile(HH_size, probs = 0.50, na.rm = TRUE),
    q3_hh_size = quantile(HH_size, probs = 0.75, na.rm = TRUE)
  )

## Land Cover Statistics for Human Population
LU_human_stats <- CensusData %>%
  group_by(Land.Use) %>%
  summarise(
    total_households = n(),
    total_perc = n() / TotalHH * 100,
    total_adults = sum(adults, na.rm = TRUE),
    avg_adults = mean(adults, na.rm = TRUE),
    q1_adults = quantile(adults, probs = 0.25, na.rm = TRUE),
    median_adults = quantile(adults, probs = 0.50, na.rm = TRUE),
    q3_adults = quantile(adults, probs = 0.75, na.rm = TRUE),
    total_children = sum(under_18, na.rm = TRUE),
    avg_children = mean(under_18, na.rm = TRUE),
    q1_children = quantile(under_18, probs = 0.25, na.rm = TRUE),
    median_children = quantile(under_18, probs = 0.50, na.rm = TRUE),
    q3_children = quantile(under_18, probs = 0.75, na.rm = TRUE),
    avg_hh_size = mean(HH_size, na.rm = TRUE),
    q1_hh_size = quantile(HH_size, probs = 0.25, na.rm = TRUE),
    median_hh_size = quantile(HH_size, probs = 0.50, na.rm = TRUE),
    q3_hh_size = quantile(HH_size, probs = 0.75, na.rm = TRUE)
  )

## Model Analysis

# Fit a Negative Binomial Mixed Effect model to household size
HouseholdSizeModel <- glmer.nb(HH_size ~ Urb_V_Rur + Land.Use + (1 | village), data = CensusData)

# Summarize the model
summary(HouseholdSizeModel)

# Check for multicollinearity
vif(HouseholdSizeModel, type="predictor")

# Dredge the model to find the best combination of predictors
options(na.action = "na.fail")  # Required for dredging
logistic_dredge_HS <- dredge(HouseholdSizeModel)

# Get the best model
logistic_best_HS <- get.models(logistic_dredge_HS, subset = 1)

# Summarize the best model
summary(logistic_best_HS[[1]])

# Extract Odds Ratios (Exp of coefficients) from original model
odds_ratios_hs <- exp(fixef(HouseholdSizeModel))

# Confidence intervals for the Odds Ratios
ci_hs <- confint(HouseholdSizeModel,parm="beta_",method="Wald")
ci_odds_ratios_hs <- exp(ci_hs)

# Combine results into a table
results_hs <- data.frame(
  Estimate = odds_ratios_hs,
  CI_Lower = ci_odds_ratios_hs[, 1],
  CI_Upper = ci_odds_ratios_hs[, 2]
)

# Print the results
print(results_hs)


################################################################################

######################
###  Dog Analysis  ###
######################

## Basic Statistics for Dog Population
dog_stats <- CensusData %>%
  group_by() %>%
  summarise(
    households = n(),
    # TOTAL DOGS
    total_dog_owning_households = sum(TotalDogs > 0, na.rm = TRUE),
    total_dogs = sum(TotalDogs, na.rm = TRUE),
    avg_dogs = mean(TotalDogs[TotalDogs > 0], na.rm = TRUE),
    q1_dogs = quantile(TotalDogs[TotalDogs > 0], probs = 0.25, na.rm = TRUE),
    median_dogs = quantile(TotalDogs[TotalDogs > 0], probs = 0.50, na.rm = TRUE),
    q3_dogs = quantile(TotalDogs[TotalDogs > 0], probs = 0.75, na.rm = TRUE),
    dog_ownership_rate = total_dog_owning_households / households * 100,
    # ADULT DOGS
    total_Adog_owning_households = sum(dogs > 0, na.rm = TRUE),
    total_Adogs = sum(dogs, na.rm = TRUE),
    avg_Adogs = mean(dogs[dogs > 0], na.rm = TRUE),
    q1_Adogs = quantile(dogs[dogs > 0], probs = 0.25, na.rm = TRUE),
    median_Adogs = quantile(dogs[dogs > 0], probs = 0.50, na.rm = TRUE),
    q3_Adogs = quantile(dogs[dogs > 0], probs = 0.75, na.rm = TRUE),
    # PUPPIES
    total_puppy_owning_households = sum(puppies > 0, na.rm = TRUE),
    total_puppies = sum(puppies, na.rm = TRUE),
    avg_puppies = mean(puppies[puppies > 0], na.rm = TRUE),
    q1_puppies = quantile(puppies[puppies > 0], probs = 0.25, na.rm = TRUE),
    median_puppies = quantile(puppies[puppies > 0], probs = 0.50, na.rm = TRUE),
    q3_puppies = quantile(puppies[puppies > 0], probs = 0.75, na.rm = TRUE),
    puppy_ownership_rate = total_puppy_owning_households / households * 100
  )

## Urban/Rural Statistics for Dog Population
UR_dog_stats <- CensusData %>%
  group_by() %>%
  summarise(
    households = n(),
    # TOTAL DOGS
    total_dog_owning_households = sum(TotalDogs > 0, na.rm = TRUE),
    total_dogs = sum(TotalDogs, na.rm = TRUE),
    avg_dogs = mean(TotalDogs[TotalDogs > 0], na.rm = TRUE),
    q1_dogs = quantile(TotalDogs[TotalDogs > 0], probs = 0.25, na.rm = TRUE),
    median_dogs = quantile(TotalDogs[TotalDogs > 0], probs = 0.50, na.rm = TRUE),
    q3_dogs = quantile(TotalDogs[TotalDogs > 0], probs = 0.75, na.rm = TRUE),
    dog_ownership_rate = total_dog_owning_households / households * 100,
    # ADULT DOGS
    total_Adog_owning_households = sum(dogs > 0, na.rm = TRUE),
    total_Adogs = sum(dogs, na.rm = TRUE),
    avg_Adogs = mean(dogs[dogs > 0], na.rm = TRUE),
    q1_Adogs = quantile(dogs[dogs > 0], probs = 0.25, na.rm = TRUE),
    median_Adogs = quantile(dogs[dogs > 0], probs = 0.50, na.rm = TRUE),
    q3_Adogs = quantile(dogs[dogs > 0], probs = 0.75, na.rm = TRUE),
    # PUPPIES
    total_puppy_owning_households = sum(puppies > 0, na.rm = TRUE),
    total_puppies = sum(puppies, na.rm = TRUE),
    avg_puppies = mean(puppies[puppies > 0], na.rm = TRUE),
    q1_puppies = quantile(puppies[puppies > 0], probs = 0.25, na.rm = TRUE),
    median_puppies = quantile(puppies[puppies > 0], probs = 0.50, na.rm = TRUE),
    q3_puppies = quantile(puppies[puppies > 0], probs = 0.75, na.rm = TRUE),
    puppy_ownership_rate = total_puppy_owning_households / households * 100
  )

## Land Cover Statistics for Dog Population
LU_dog_stats <- CensusData %>%
  group_by() %>%
  summarise(
    households = n(),
    # TOTAL DOGS
    total_dog_owning_households = sum(TotalDogs > 0, na.rm = TRUE),
    total_dogs = sum(TotalDogs, na.rm = TRUE),
    avg_dogs = mean(TotalDogs[TotalDogs > 0], na.rm = TRUE),
    q1_dogs = quantile(TotalDogs[TotalDogs > 0], probs = 0.25, na.rm = TRUE),
    median_dogs = quantile(TotalDogs[TotalDogs > 0], probs = 0.50, na.rm = TRUE),
    q3_dogs = quantile(TotalDogs[TotalDogs > 0], probs = 0.75, na.rm = TRUE),
    dog_ownership_rate = total_dog_owning_households / households * 100,
    # ADULT DOGS
    total_Adog_owning_households = sum(dogs > 0, na.rm = TRUE),
    total_Adogs = sum(dogs, na.rm = TRUE),
    avg_Adogs = mean(dogs[dogs > 0], na.rm = TRUE),
    q1_Adogs = quantile(dogs[dogs > 0], probs = 0.25, na.rm = TRUE),
    median_Adogs = quantile(dogs[dogs > 0], probs = 0.50, na.rm = TRUE),
    q3_Adogs = quantile(dogs[dogs > 0], probs = 0.75, na.rm = TRUE),
    # PUPPIES
    total_puppy_owning_households = sum(puppies > 0, na.rm = TRUE),
    total_puppies = sum(puppies, na.rm = TRUE),
    avg_puppies = mean(puppies[puppies > 0], na.rm = TRUE),
    q1_puppies = quantile(puppies[puppies > 0], probs = 0.25, na.rm = TRUE),
    median_puppies = quantile(puppies[puppies > 0], probs = 0.50, na.rm = TRUE),
    q3_puppies = quantile(puppies[puppies > 0], probs = 0.75, na.rm = TRUE),
    puppy_ownership_rate = total_puppy_owning_households / households * 100
  )


##  Dog Ownership Analysis

# Univariable logistic regression for dog ownership
DogOwnershipModel <- glmer(as.factor(owns_dogs) ~ adults + under_18 + Land.Use + Urb_V_Rur + (1 | village),
                           family = binomial, data = CensusData)
summary(DogOwnershipModel)

# Check for multicolinearity
vif(DogOwnershipModel)  # all <5 suggesting no multicolinearity

# Logistic regression
options(na.action = "na.fail")  # Required for dredging
logistic_dredge_DO <- dredge(DogOwnershipModel)
logistic_best_DO <- get.models(logistic_dredge_DO, subset = 1)  # Best model
summary(logistic_best_DO[[1]])

# Odds Ratios
exp(fixef(logistic_best_DO[[1]]))

## Confidence Intervals
ci <- confint(logistic_best_DO[[1]],parm="beta_",method="Wald")

# Exponentiate coefficients and CIs to get odds ratios
odds_ratios <- exp(fixef(logistic_best_DO[[1]]))
ci_odds_ratios <- exp(ci)

# Combine results into a table
results <- data.frame(
  Estimate = odds_ratios,
  CI_Lower = ci_odds_ratios[, 1],
  CI_Upper = ci_odds_ratios[, 2]
)

# Print results
print(results)

# R2
r2(logistic_best_DO[[1]])


################################################################################

#############################
###  Dog Number Analysis  ###
#############################

# Select dog-owing households
CensusDataOwnedN <- CensusData[which(CensusData$owns_dogs=="Yes"),]

# Univariable logistic regression for dog ownership
DogOwnershipNModel <- glmer.nb(TotalDogs ~ adults + under_18 + Land.Use + Urb_V_Rur + (1 | village),
                               data = CensusDataOwnedN)
summary(DogOwnershipNModel)

# Odds Ratios
exp(fixef(DogOwnershipNModel))

## Confidence Intervals
ci <- confint(DogOwnershipNModel,parm="beta_",method="Wald")

# Exponentiate coefficients and CIs to get odds ratios
odds_ratios <- exp(fixef(DogOwnershipNModel))
ci_odds_ratios <- exp(ci)

# Combine results into a table
results <- data.frame(
  Estimate = odds_ratios,
  CI_Lower = ci_odds_ratios[, 1],
  CI_Upper = ci_odds_ratios[, 2]
)

# Print results
print(results)

# Mean household dogs per urban/rural
mean(CensusDataOwnedN$TotalDogs[which(CensusDataOwnedN$Urb_V_Rur=="Urban")])  # 1.93
mean(CensusDataOwnedN$TotalDogs[which(CensusDataOwnedN$Urb_V_Rur=="Rural")])  # 2.24

# Check for multicolinearity
vif(DogOwnershipNModel)  # all <5 suggesting no multicolinearity

# Logistic regression
options(na.action = "na.fail")  # Required for dredging
logistic_dredge_DN <- dredge(DogOwnershipNModel)
logistic_best_DN <- get.models(logistic_dredge_DN, subset = 1)  # Best model
summary(logistic_best_DN[[1]])

# Odds Ratios
exp(fixef(logistic_best_DN[[1]]))

## Confidence Intervals
ci <- confint(logistic_best_DN[[1]],parm="beta_",method="Wald")

# Exponentiate coefficients and CIs to get odds ratios
odds_ratios <- exp(fixef(logistic_best_DN[[1]]))
ci_odds_ratios <- exp(ci)


# Combine results into a table
results <- data.frame(
  Estimate = odds_ratios,
  CI_Lower = ci_odds_ratios[, 1],
  CI_Upper = ci_odds_ratios[, 2]
)

# Print results
print(results)

# R2
r2(logistic_best_DN[[1]])


# Mean household dogs per urban/rural
mean(CensusDataOwnedN$TotalDogs[which(CensusDataOwnedN$Urb_V_Rur=="Urban")])
mean(CensusDataOwnedN$TotalDogs[which(CensusDataOwnedN$Urb_V_Rur=="Rural")])


## Number of households with 1-3 dogs
nrow(CensusDataOwnedN[which(CensusDataOwnedN$TotalDogs<4),]) / nrow(CensusDataOwnedN) * 100


##################
###  FIGURE 4  ###
##################

# Extract model estimates
dog_ownership_est <- tidy(DogOwnershipModel, conf.int = TRUE, conf.method = "Wald", effects = "fixed") %>%
  mutate(Model = "Dog Ownership")

dog_ownership_n_est <- tidy(DogOwnershipNModel, conf.int = TRUE, conf.method = "Wald", effects = "fixed") %>%
  mutate(Model = "Dog Count")

# Combine results, remove intercept
combined_est <- bind_rows(dog_ownership_est, dog_ownership_n_est) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = factor(term, levels = rev(unique(term))))  # Reverse order for better Y-axis display


# Create a nicer label for predictors, handling multi-level factors.
# For example, change "Land.UseCrops" to "Land Cover: Crops".
combined_est <- combined_est %>%
  mutate(Predictor = case_when(
    grepl("^Land\\.Use", term) ~ paste("Land Cover:", gsub("Land\\.Use", "", term)),
    grepl("^Urb_V_Rur", term) ~ paste("District:", gsub("Urb_V_Rur", "", term)),
    grepl("^under_18", term) ~ paste("No. of Children"),
    grepl("^adults", term) ~ paste("No. of Adults"),
    TRUE ~ term
  ))


# Recalculate overlap0 based on exponentiated CIs (whether they include 1)
combined_est <- combined_est %>%
  mutate(
    # Exponentiate estimates and CIs
    estimate_exp = exp(estimate),
    conf.low_exp = exp(conf.low),
    conf.high_exp = exp(conf.high),
    # Check if exponentiated CI includes 1
    overlap1 = (conf.low_exp < 1 & conf.high_exp > 1)
  )

# Swap facet order by redefining Model as a factor
combined_est$Model <- factor(combined_est$Model,levels = c("Dog Ownership", "Dog Count"))  # Ownership first

# Create the plot
plot_4 <- ggplot(combined_est, aes(x = estimate_exp, y = Predictor)) +
  geom_errorbarh(
    aes(xmin = conf.low_exp, xmax = conf.high_exp, color = !overlap1),  # Use overlap1
    height = 0.2
  ) +
  geom_point(aes(color = !overlap1), size = 3) +  # Match errorbar colors
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  facet_grid(
    . ~ Model,
    scales = "free_x",
    labeller = labeller(Model = c(
      "Dog Ownership" = "Dog Ownership",
      "Dog Count" = "Dog Count"
    ))
  ) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
  scale_x_log10(
    breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 8),
    labels = c("0.1", "0.25", "0.5", "1", "2", "4", "8")
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.spacing = unit(1, "lines"),
    axis.text = element_text(size = 10)
  ) +
  labs(x = "Effect Size (log scale)", y = "Predictor")

plot_4


################################################################################

##################################
###  Urban Rural HDR Analysis  ###
##################################

# Function to bootstrap HDR with village-level resampling
bootstrap_hdr <- function(data, group_var, R = 5000) {
  group_var <- enquo(group_var)
  results <- data %>%
    group_by(!!group_var) %>%
    summarise(
      total_humans = sum(HH_size, na.rm = TRUE),
      total_dogs = sum(TotalDogs, na.rm = TRUE),
      point_est = total_humans / total_dogs
    ) %>%
    mutate(
      ci_lower = NA_real_,
      ci_upper = NA_real_
    )

  for(i in 1:nrow(results)) {
    group_val <- results[[rlang::as_name(group_var)]][i]
    group_data <- filter(data, !!group_var == group_val)

    # Get unique villages
    villages <- unique(group_data$village)

    # Bootstrap function
    boot_func <- function(d, indices) {
      sampled_villages <- villages[indices]
      sampled_data <- d %>% filter(village %in% sampled_villages)
      sum(sampled_data$HH_size) / sum(sampled_data$TotalDogs)
    }

    # Run bootstrap
    set.seed(123)
    boot_res <- boot(group_data, statistic = boot_func, R = R)
    ci <- boot.ci(boot_res, type = "perc")

    # Store results
    results$ci_lower[i] <- ci$percent[4]
    results$ci_upper[i] <- ci$percent[5]
  }
  return(results)
}

# Calculate overall HDRs with CIs
urban_rural_hdr <- bootstrap_hdr(CensusData, Urb_V_Rur)
print(urban_rural_hdr)

# Village-level stats (medians/IQRs)
w_human_dog_ratio <- CensusData %>%
  group_by(lga, ward, Urb_V_Rur) %>%
  summarise(
    total_humans = sum(HH_size, na.rm = TRUE),
    total_dogs = sum(TotalDogs, na.rm = TRUE),
    human_dog_ratio = total_humans / total_dogs
  ) %>%
  filter(human_dog_ratio < 100)

village_stats_ur <- w_human_dog_ratio %>%
  group_by(Urb_V_Rur) %>%
  summarise(
    village_median = median(human_dog_ratio),
    village_q1 = quantile(human_dog_ratio, 0.25),
    village_q3 = quantile(human_dog_ratio, 0.75)
  )

# Mixed-effects model
WardHDRModel <- glmer.nb(human_dog_ratio ~ Urb_V_Rur + (1 | ward),
                         data = w_human_dog_ratio)

# Extract IRR and CI
model_summary <- summary(WardHDRModel)
irr <- exp(fixef(WardHDRModel))
ci <- exp(confint(WardHDRModel, parm = "beta_", method = "Wald"))
model_results <- data.frame(
  IRR = irr,
  CI_Lower = ci[,1],
  CI_Upper = ci[,2],
  p_value = coef(model_summary)[,4]
)

# Combine all results
final_ur_results <- urban_rural_hdr %>%
  left_join(village_stats_ur, by = "Urb_V_Rur") %>%
  mutate(model_irr = c(NA, model_results$IRR[-1]),
         model_ci_lower = c(NA, model_results$CI_Lower[-1]),
         model_ci_upper = c(NA, model_results$CI_Upper[-1]),
         p_value = c(NA, model_results$p_value[-1]))

print(final_ur_results)

# R2
r.squaredGLMM(WardHDRModel)[1, "R2m"]
r.squaredGLMM(WardHDRModel)[1, "R2c"]


###############################
###  Land Use HDR Analysis  ###
###############################

# Overall HDRs with bootstrap CIs
land_use_hdr <- bootstrap_hdr(CensusData, Land.Use)
print(land_use_hdr)

# Village-level stats (medians/IQRs)
lu_human_dog_ratio <- CensusData %>%
  group_by(lga, ward, Land.Use) %>%
  summarise(
    total_humans = sum(HH_size, na.rm = TRUE),
    total_dogs = sum(TotalDogs, na.rm = TRUE),
    human_dog_ratio = total_humans / total_dogs
  ) %>%
  filter(human_dog_ratio < 100)

village_stats_lu <- lu_human_dog_ratio %>%
  group_by(Land.Use) %>%
  summarise(
    village_median = median(human_dog_ratio),
    village_q1 = quantile(human_dog_ratio, 0.25),
    village_q3 = quantile(human_dog_ratio, 0.75)
  )

# Mixed-effects model
LandHDRModel <- glmer.nb(human_dog_ratio ~ Land.Use + (1 | ward),
                         data = lu_human_dog_ratio)

# Extract IRR and CI (using built as reference)
model_summary_lu <- summary(LandHDRModel)
irr_lu <- exp(fixef(LandHDRModel))
ci_lu <- exp(confint(LandHDRModel, parm = "beta_", method = "Wald"))
model_results_lu <- data.frame(
  IRR = irr_lu,
  CI_Lower = ci_lu[,1],
  CI_Upper = ci_lu[,2],
  p_value = coef(model_summary_lu)[,4]
)

# Combine all results
final_lu_results <- land_use_hdr %>%
  left_join(village_stats_lu, by = "Land.Use") %>%
  mutate(model_irr = c(irr_lu[1], model_results_lu$IRR[-1]),
         model_ci_lower = c(ci_lu[1,1], model_results_lu$CI_Lower[-1]),
         model_ci_upper = c(ci_lu[1,2], model_results_lu$CI_Upper[-1]),
         p_value = c(coef(model_summary_lu)[1,4], model_results_lu$p_value[-1]))

print(final_lu_results)

# R2
r.squaredGLMM(LandHDRModel)[1, "R2m"]
r.squaredGLMM(LandHDRModel)[1, "R2c"]


##################
###  FIGURE 5  ###
##################

# Get HDR range (ignore extreme outliers above 100)
combined_hdr <- c(
  w_human_dog_ratio$human_dog_ratio,
  lu_human_dog_ratio$human_dog_ratio
)
y_max <- min(150, max(combined_hdr, na.rm = TRUE))  # Cap at HDR=100
y_limits <- c(1, y_max)

# Create consistent theme
theme_boxplot <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Custom breaks for better log scaling
log_breaks <- c(1, 3, 10, 30, 100)
log_labels <- c("1", "3", "10", "30", "100")

# Plot A: Urban/Rural
plot_5a <- ggplot(w_human_dog_ratio, aes(x = Urb_V_Rur, y = human_dog_ratio)) +
  #  geom_violin(fill="orangered1") +
  geom_boxplot(width=0.8, fill="white") +
  scale_y_log10(
    limits = y_limits,
    breaks = log_breaks,
    labels = log_labels,
    oob = scales::squish  # Show >100 values at top
  ) +
  coord_cartesian(ylim = y_limits) +  # Focus on 1-100 range
  labs(y = "Human-to-Dog Ratio (log scale)", x = "District Type") +
  theme_boxplot

# Plot B: Land Use
plot_5b <- ggplot(lu_human_dog_ratio, aes(x = Land.Use, y = human_dog_ratio)) +
  #  geom_violin(fill="dodgerblue") +
  geom_boxplot(width=0.8, fill="white") +
  scale_y_log10(
    limits = y_limits,
    breaks = log_breaks,
    labels = log_labels,
    oob = scales::squish
  ) +
  coord_cartesian(ylim = y_limits) +
  labs(y = NULL, x = "Land Cover") +
  theme_boxplot

# Combine plots
plot_5 <- plot_5a + plot_5b +
  plot_layout(nrow = 1) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = "bold"))

plot_5



################################################################################



# Load human population data
district_pops <- data.frame(
  lga = c("Bunda DC", "Bunda TC", "Butiama DC", "Musoma DC", "Musoma MC",
          "Rorya DC", "Serengeti DC", "Tarime DC", "Tarime TC"),
  human_pop = c(243822, 182970, 281656, 266665, 164172,
                354490, 340349, 404848, 133043)
)

# Total regional population
regional_pop <- sum(district_pops$human_pop)

# Initialise results data frame
results <- data.frame(
  lga = character(),
  hdr_point = numeric(),
  hdr_ci_lower = numeric(),
  hdr_ci_upper = numeric(),
  dog_est = numeric(),
  dog_lower = numeric(),
  dog_upper = numeric(),
  stringsAsFactors = FALSE
)

# Bootstrap function for HDR
calculate_hdr <- function(data, indices) {
  resampled_data <- data[indices, ]
  total_humans <- sum(resampled_data$HH_size, na.rm = TRUE)
  total_dogs <- sum(resampled_data$TotalDogs, na.rm = TRUE)
  if(total_dogs == 0) return(NA)
  total_humans / total_dogs
}

# Process each district individually
for(lga_name in district_pops$lga) {
  # Subset data for current LGA
  lga_data <- CensusData %>% filter(lga == lga_name)

  # Skip if no villages or dogs
  villages <- unique(lga_data$village)
  if(length(villages) == 0) next

  # Calculate point estimate
  total_humans <- sum(lga_data$HH_size, na.rm = TRUE)
  total_dogs <- sum(lga_data$TotalDogs, na.rm = TRUE)
  hdr_point <- ifelse(total_dogs > 0, total_humans / total_dogs, NA)

  # Run bootstrap only if sufficient data
  if(length(villages) >= 3 && total_dogs > 0) {
    boot_res <- boot(lga_data, statistic = calculate_hdr, R = 10000)
    ci <- boot.ci(boot_res, type = "perc")
    hdr_ci_lower <- ci$percent[4]
    hdr_ci_upper <- ci$percent[5]
  } else {
    hdr_ci_lower <- NA
    hdr_ci_upper <- NA
  }

  # Get human population
  human_pop <- district_pops$human_pop[district_pops$lga == lga_name]

  # Calculate dog populations
  dog_est <- ifelse(is.na(hdr_point), NA, human_pop / hdr_point)
  dog_lower <- ifelse(is.na(hdr_ci_upper), NA, human_pop / hdr_ci_upper)
  dog_upper <- ifelse(is.na(hdr_ci_lower), NA, human_pop / hdr_ci_lower)

  # Add to results
  results <- rbind(results, data.frame(
    lga = lga_name,
    hdr_point,
    hdr_ci_lower,
    hdr_ci_upper,
    dog_est,
    dog_lower,
    dog_upper
  ))
}

# Process regional estimate
# Calculate point estimate
total_regional_humans <- sum(CensusData$HH_size, na.rm = TRUE)
total_regional_dogs <- sum(CensusData$TotalDogs, na.rm = TRUE)
regional_hdr_point <- total_regional_humans / total_regional_dogs

# Bootstrap regional estimate (x 10,000)
boot_regional <- boot(CensusData, statistic = calculate_hdr, R = 10000)
ci_regional <- boot.ci(boot_regional, type = "perc")
regional_ci_lower <- ci_regional$percent[4]
regional_ci_upper <- ci_regional$percent[5]

# Regional dog population
regional_dog_est <- regional_pop / regional_hdr_point
regional_dog_lower <- regional_pop / regional_ci_upper
regional_dog_upper <- regional_pop / regional_ci_lower

# Add regional to results
results <- rbind(results, data.frame(
  lga = "MARA TOTAL",
  hdr_point = regional_hdr_point,
  hdr_ci_lower = regional_ci_lower,
  hdr_ci_upper = regional_ci_upper,
  dog_est = regional_dog_est,
  dog_lower = regional_dog_lower,
  dog_upper = regional_dog_upper
))

# Format results
results_formatted <- results %>%
  mutate(
    hdr_ci = ifelse(is.na(hdr_ci_lower) | is.na(hdr_ci_upper),
                    sprintf("%.1f", hdr_point),
                    sprintf("%.1f (%.1f-%.1f)", hdr_point, hdr_ci_lower, hdr_ci_upper)),
    dog_pop = sprintf("%.0f", dog_est),
    dog_pop_ci = ifelse(is.na(dog_lower) | is.na(dog_upper),
                        sprintf("%.0f", dog_est),
                        sprintf("%.0f (%.0f-%.0f)", dog_est, dog_lower, dog_upper)),
    human_pop = case_when(
      lga == "REGIONAL TOTAL" ~ regional_pop,
      TRUE ~ district_pops$human_pop[match(lga, district_pops$lga)]
    )
  ) %>%
  dplyr::select(lga, human_pop, hdr_ci, dog_pop_ci)

# Print results
print(results_formatted)

sum(results$dog_est[which(results$lga!="MARA TOTAL")])




