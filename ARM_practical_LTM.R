
#---LTM Example code---#

#---------------------#
#### Load packages ####
#---------------------#

library(tidyverse); library(lcmm); library(splines); library(lattice)

#---------------------------#
#### LOAD BMI DATASETS ####
#---------------------------#

dat <- read_csv((
  "https://raw.githubusercontent.com/aelhak/ARMLT_24/main/bmi_long.csv")) %>% 
  group_by(id) %>% mutate(id = cur_group_id(), sex = as.factor(sex)) %>% 
  as.data.frame()

#--------------------------------#
#### INSPECT DATA AND SUMMARY ####
#--------------------------------#

with(dat, plot(age, bmi))

dat %>% summarise(N = n_distinct(id))

dat %>% select(age, bmi) %>% summarise_all(list(min = min, max = max))

dat %>% group_by(id) %>% count() %>% ungroup() %>% summarise(median(n), min(n), max(n))

#------------------------------------#
#### GROUP-BASED TRAJECTORY MODEL ####
#------------------------------------#

## Set seed
set.seed(2024)

## Fit 1-4 class models

(gbtm_1c <- hlme(
  fixed = bmi ~ 1 + ns(age, 5), 
  subject = 'id', ng = 1, data = dat))

gbtm_24c <- map(2:4, ~ { eval(parse(text = paste0(
  "gridsearch(hlme(fixed = bmi ~ 1 + ns(age, 5), subject = 'id', 
  mixture = ~ ns(age, 5), ng = ", .x, ", nwg = F, data = dat), 
    minit = gbtm_1c, maxiter = 2, rep = 10)")))}
)

## Plot mean trajectories for 1-4 class models

gbtm_res <- c(list(gbtm_1c), gbtm_24c)

iwalk(
  map(gbtm_res, ~ predictY(
    ., data.frame(age = seq(min(dat$age), max(dat$age), length = 100)), var.time ="age")
    ), ~{plot(.x, main = "", legend.loc = "topleft", lty = 1)}
  )

## Get summary statistics for 1-4 class models

map_df(gbtm_res, ~ {return(data.frame(summarytable(.x, which = c(
  "G", "npm", "BIC", "SABIC", "entropy", "%class"), display = F)))}) 

## Inspect postprob and plot the selected model trajectory sub-groups with CIs

(gbtm_best <- gbtm_res[[2]])

postprob(gbtm_best)
head(predictClass(gbtm_best, dat))

gbtm_best_pred <- data.frame(age = seq(min(dat$age), max(dat$age), length = 100))

gbtm_best_pred <- predictY(gbtm_best, gbtm_best_pred, var.time ="age", draws = T)

gbtm_best_pred <- cbind(gbtm_best_pred$times, gbtm_best_pred$pred)

ggplot(
  data = gbtm_best_pred, aes(x = age)) +
  geom_line(aes(y = Ypred_class1, col = "Class 1 (36.5%)")) +
  geom_line(aes(y = Ypred_class2, col = "Class 2 (63.5)")) + theme_classic() +
  geom_ribbon(aes(ymin = lower.Ypred_class1, ymax = upper.Ypred_class1), alpha = 0.05) +
  geom_ribbon(aes(ymin = lower.Ypred_class2, ymax = upper.Ypred_class2), alpha = 0.05) +
  labs(x = 'Age', y = 'BMI') + theme(legend.title = element_blank())

#-----------------------------------------------#
#### RELATE TRAJECTORY SUB-GROUPS TO OUTCOME ####
#-----------------------------------------------#

dat2 <- predictClass(gbtm_best, dat) %>% full_join(dat) %>% 
  select(id, class, prob1, prob2, sbp_10) %>% 
  distinct(id, .keep_all = TRUE)

dat2$class <- dplyr::recode(dat2$class, '1' = 'Higher BMI trajectory', '2' = 'Lower BMI trajectory')
dat2$class = factor(dat2$class, levels=c("Lower BMI trajectory", "Higher BMI trajectory"))

# Association b/w BMI trajectory sub-groups and SBP at 10y

broom::tidy(lm(sbp_10 ~ class, data = dat2), conf.int = T)

# Association analysis Weighted by class probabilities

dat2 <- dat2 %>% mutate(probs = prob1 + prob2)

broom::tidy(lm(sbp_10 ~ class, weights = probs, data = dat2), conf.int = T)

#----------------------------#
#### GROWTH MIXTURE MODEL ####
#----------------------------#

 # The only difference from GBTM is we add a 'random' argument, post modelling stuff is all the same:

## Fit 1-4 class growth mixture models

gmm_1c <- hlme(
  fixed = bmi ~ 1 + ns(age, 5), random = ~ 1 + age,
  subject = 'id', ng = 1, data = dat)

gmm_24c <- map(2:4, ~ { eval(parse(text = paste0(
  "gridsearch(hlme(fixed = bmi ~ 1 + ns(age, 5), random = ~ 1 + age, 
  subject = 'id', mixture = ~ ns(age, 5), ng = ", .x, ", nwg = F, 
  data = dat), minit = gmm_1c, maxiter = 2, rep = 10)")))}
  )

## Plot mean trajectories for 1-4 class models

gmm_res <- c(list(gmm_1c), gmm_24c)

iwalk(map(gmm_res, ~ predictY(
  ., data.frame(age = seq(min(dat$age), max(dat$age), length = 100)), 
  var.time ="age")), ~{plot(.x, main = "", legend.loc = "topleft", lty = 1)}
  )

## Get summary statistics for 1-4 class models

map_df(gmm_res, ~ {return(data.frame(summarytable(.x, which = c(
  "G", "npm", "BIC", "SABIC", "entropy", "%class"), display = F)))}) 
