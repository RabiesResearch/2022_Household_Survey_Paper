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

# Fit a Negative Binomial model to household size
HouseholdSizeModel <- glm.nb(HH_size ~ Urb_V_Rur + Land.Use, data = CensusData)

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

# Extract Odds Ratios (Exp of coefficients)
odds_ratios_hs <- exp(coef(logistic_best_HS[[1]]))

# Confidence intervals for the Odds Ratios
ci_hs <- confint(logistic_best_HS[[1]])
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
DogOwnershipModel <- glm(as.factor(owns_dogs) ~ adults + under_18 + Land.Use + Urb_V_Rur,
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
exp(coef(logistic_best_DO[[1]]))

## Confidence Intervals
ci <- confint(logistic_best_DO[[1]])

# Exponentiate coefficients and CIs to get odds ratios
odds_ratios <- exp(coef(logistic_best_DO[[1]]))
ci_odds_ratios <- exp(ci)

# Combine results into a table
results <- data.frame(
  Estimate = odds_ratios,
  CI_Lower = ci_odds_ratios[, 1],
  CI_Upper = ci_odds_ratios[, 2]
)

# Print results
print(results)


###  Dog Number Analysis

# Select dog-owing households
CensusDataOwnedN <- CensusData[which(CensusData$owns_dogs=="Yes"),]

# Univariable logistic regression for dog ownership
DogOwnershipNModel <- glm.nb(TotalDogs ~ adults + under_18 + Land.Use + Urb_V_Rur,
                             data = CensusDataOwnedN)
summary(DogOwnershipNModel)
# Odds Ratios
exp(coef(DogOwnershipNModel))
## Confidence Intervals
ci <- confint(DogOwnershipNModel)
# Exponentiate coefficients and CIs to get odds ratios
odds_ratios <- exp(coef(DogOwnershipNModel))
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
exp(coef(logistic_best_DN[[1]]))

## Confidence Intervals
ci <- confint(logistic_best_DN[[1]])

# Exponentiate coefficients and CIs to get odds ratios
odds_ratios <- exp(coef(logistic_best_DN[[1]]))
ci_odds_ratios <- exp(ci)

# Combine results into a table
results <- data.frame(
  Estimate = odds_ratios,
  CI_Lower = ci_odds_ratios[, 1],
  CI_Upper = ci_odds_ratios[, 2]
)

# Print results
print(results)


##################
###  FIGURE 4  ###
##################

# Extract model estimates
dog_ownership_est <- tidy(DogOwnershipModel, conf.int = TRUE) %>%
  mutate(Model = "Dog Ownership")

dog_ownership_n_est <- tidy(DogOwnershipNModel, conf.int = TRUE) %>%
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

# Step 1: Recalculate overlap0 based on exponentiated CIs (whether they include 1)
combined_est <- combined_est %>%
  mutate(
    # Exponentiate estimates and CIs
    estimate_exp = exp(estimate),
    conf.low_exp = exp(conf.low),
    conf.high_exp = exp(conf.high),
    # Check if exponentiated CI includes 1
    overlap1 = (conf.low_exp < 1 & conf.high_exp > 1)
  )

# Step 2: Create the plot with corrected aesthetics
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


##################################
###  Urban Rural HDR Analysis  ###
##################################

# Calculate the human-to-dog ratio (HDR)
human_dog_ratio_UR <- CensusData %>%
  group_by(Urb_V_Rur) %>%
  summarise(
    total_humans = sum(HH_size, na.rm = TRUE),
    total_dogs = sum(TotalDogs, na.rm = TRUE),
    human_dog_ratio = total_humans / total_dogs,
    mean_humans = mean(HH_size, na.rm = TRUE),
    mean_dogs = mean(TotalDogs, na.rm = TRUE)
  )

# Overall HDR
print(human_dog_ratio_UR)

# Calculate the human-to-dog ratio (HDR) at the ward level
w_human_dog_ratio <- CensusData %>%
  group_by(lga, ward, Urb_V_Rur) %>%
  summarise(
    total_humans = sum(HH_size, na.rm = TRUE),
    total_dogs = sum(TotalDogs, na.rm = TRUE),
    human_dog_ratio = total_humans / total_dogs
  )

# Filter out extreme HDR values
w_human_dog_ratio1 <- w_human_dog_ratio[which(w_human_dog_ratio$human_dog_ratio < 100),]

# Perform logistic regression for HDR and urban/rural classification (at the ward level)
WardHDRModel <- glm.nb(human_dog_ratio ~ Urb_V_Rur, data = w_human_dog_ratio1)

# Summary of model
summary(WardHDRModel)

# Extract odds ratios for HDR model
odds_ratios <- exp(coef(WardHDRModel))

# Confidence intervals for odds ratios
ci <- confint(WardHDRModel)
odds_ratios_ci <- exp(ci)

# Combine odds ratios and confidence intervals for HDR
odds_results <- data.frame(
  Estimate = odds_ratios,
  CI_Lower = odds_ratios_ci[, 1],
  CI_Upper = odds_ratios_ci[, 2]
)

# Print odds ratios and confidence intervals for HDR at the ward level
print(odds_results)


###############################
###  Land Use HDR Analysis  ###
###############################

# Calculate the human-to-dog ratio (HDR)
human_dog_ratio_LU <- CensusData %>%
  group_by(Land.Use) %>%
  summarise(
    total_humans = sum(HH_size, na.rm = TRUE),
    total_dogs = sum(TotalDogs, na.rm = TRUE),
    human_dog_ratio = total_humans / total_dogs
  )

# Overall HDR
print(human_dog_ratio_LU)

# Calculate the human-to-dog ratio (HDR) at the ward level
lu_human_dog_ratio <- CensusData %>%
  group_by(lga, ward, Land.Use) %>%
  summarise(
    total_humans = sum(HH_size, na.rm = TRUE),
    total_dogs = sum(TotalDogs, na.rm = TRUE),
    human_dog_ratio = total_humans / total_dogs
  )

# Filter out extreme HDR values
lu_human_dog_ratio <- lu_human_dog_ratio[which(lu_human_dog_ratio$human_dog_ratio < 100),]

# Perform logistic regression for HDR and land use classification (at the ward level)
LandHDRModel <- glm.nb(human_dog_ratio ~ Land.Use, data = lu_human_dog_ratio)

# Summary of model
summary(LandHDRModel)

# Extract odds ratios for HDR model
odds_ratios <- exp(coef(LandHDRModel))

# Confidence intervals for odds ratios
ci <- confint(LandHDRModel)
odds_ratios_ci <- exp(ci)

# Combine odds ratios and confidence intervals for HDR
odds_results <- data.frame(
  Estimate = odds_ratios,
  CI_Lower = odds_ratios_ci[, 1],
  CI_Upper = odds_ratios_ci[, 2]
)

# Print odds ratios and confidence intervals for HDR at the ward level
print(odds_results)


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


