
#---LME Example code---#

#---------------------#
#### Load packages ####
#---------------------#

library(tidyverse); library(lme4); library(lspline); library(splines); library(broom.mixed); library(ggeffects)

#-------------------------#
#### LOAD BMI DATASET ####
#-------------------------#

dat <- read_csv(
  ("https://raw.githubusercontent.com/aelhak/ARMLT_24/main/bmi_long.csv")) %>% 
  mutate(sex = as.factor(sex)
         )

#### INSPECT DATA AND SUMMARY ####

with(dat, plot(age, bmi))

dat %>% group_by(sex) %>% summarise(N = n_distinct(id))

dat %>% summarise_if(is.numeric, list(min = min, max = max))

dat %>% group_by(id) %>% count() %>% ungroup() %>% summarise(median(n), min(n), max(n))

#------------------------------#
#### RANDOM INTERCEPT MODEL ####
#------------------------------#

# Fit model

(RIM <- lmer(bmi ~ age + (1 | id ), data = dat))

# Plot predicted mean trajectory

ggemmeans(RIM, terms = c("age [all]")) %>% plot()

#--------------------------#
#### RANDOM SLOPE MODEL ####
#--------------------------#

# Fit model

(RSM <- lmer(bmi ~ age + (age | id ), data = dat))

# Plot predicted mean trajectory

ggemmeans(RSM, terms = c("age [all]")) %>% plot()

#--------------------------------------------#
#### RANDOM SLOPE MODEL + SEX INTERACTION ####
#--------------------------------------------#

# Fit model

(RSM2 <- lmer(bmi ~ age * sex + (age | id ), data = dat))

# Plot predicted mean trajectory by sex

ggemmeans(RSM2, terms = c("age [all]", "sex [all]")) %>% plot()

#---------------------------------------------#
#### LINEAR SPLINE MODEL + SEX INTERACTION ####
#  (2 to 5 knots at equal quantiles of age)   #
#---------------------------------------------#

# Fit models and select one with lowest BIC

ls_mf <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "lmer(bmi ~ qlspline(age, ", .x, ") * sex + (age | id ), 
  REML = F, data = dat)")))}, otherwise = NA_real_))

(ls_mf_best <- ls_mf[[which.min(unlist(map(ls_mf, BIC)))]])

# Print knots positions

attr(terms(ls_mf_best), "predvars")

# Plot mean BMI trajectory by sex

ggemmeans(ls_mf_best, terms = c("age [all]", "sex [all]")) %>% plot()

# Summarise linear slope for each spline segment 

as.data.frame(tidy(ls_mf_best, conf.int = T)) %>%
  filter(effect == "fixed") %>% select(-group)

#----------------------------------------------#
#### NATURAL SPLINE MODEL + SEX INTERACTION ####
#   (2 to 5 knots at equal quantiles of age)   #
#----------------------------------------------#

# Fit models and select one with lowest BIC

ns_mf <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "lmer(bmi ~ ns(age, ", .x, ")* sex + (age | id ), 
  REML = F, data = dat)")))}, otherwise = NA_real_))

(ns_mf_best <- ns_mf[[which.min(unlist(map(ns_mf, BIC)))]])

# Print knots positions (not that important to know)

attr(terms(ns_mf_best), "predvars")

# Plot mean BMI trajectory by sex

ggemmeans(ns_mf_best, terms = c("age [all]", "sex [all]")) %>% plot()

# P-value for sex by age interaction

car::linearHypothesis(ns_mf_best, c(
  "ns(age, 5)1:sexM", "ns(age, 5)2:sexM", "ns(age, 5)3:sexM", 
  "ns(age, 5)4:sexM", "ns(age, 5)5:sexM"))

# Calculate and plot sex differences in predicted BMI at specified ages

(pred_diff <- ggemmeans(ns_mf_best, terms = c(
  "age [0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7]", 
  "sex [all]")) %>% as_tibble() %>% group_by(x) %>% mutate(
    y = predicted - lag(predicted), se = sqrt(std.error^2 + lag(std.error^2))) %>% 
    select(x, y, se) %>% drop_na() %>% mutate(
      lci = y - 1.96*se, uci = y + 1.96*se))

(pred_diff_plot <- ggplot(
  data = pred_diff, aes(x = x, y = y, ymin = lci, ymax = uci)) + theme_classic() +
    geom_pointrange() + geom_hline(yintercept = 0, lty = 1, col = "red", linewidth = 0.3) + 
    scale_x_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7)) +
    xlab("age (years)") + ylab("predicted mean difference in BMI (males minus females)"))
