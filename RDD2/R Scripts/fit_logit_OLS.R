# Question 1A
# estimating model using logit
logit_nsw <- glm(treat ~ age + agesq + agecube + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                   u75 + interaction1, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)
logit_nsw
# estimating model using linear probability model
OLS_nsw <- lm(treat ~ age + agesq + agecube + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                   u75 + interaction1,data = nsw_dw_cpscontrol)
###########################################
# Question 1B
# Fit a propensity score using a quadratic for every variable and another with a cubic for every variable
###################
##### Logit 
### quadratic max
# take out age cubed
logit_nsw_quad <- glm(treat ~ age + agesq  + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                   u75 + interaction1, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)
# Fit propoensity score to quad model
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(pscore = logit_nsw_quad$fitted.values)

# mean pscore control
pscore_control <- nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()
# 0.00885

# mean pscore treated
pscore_treated <- nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.2159
####################################
### cube max
logit_nsw_cube <- glm(treat ~ age + agesq + agecube + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                   u75 + interaction1, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)

# Fit propoensity score to cubic model
nsw_dw_cpscontrol_cube <- nsw_dw_cpscontrol %>% 
  mutate(pscore = logit_nsw$fitted.values)

# mean pscore control
pscore_control_cube <- nsw_dw_cpscontrol_cube %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()
# 0.00892

# mean pscore treated
pscore_treated_cube <- nsw_dw_cpscontrol_cube %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.2218


########################################################
#Linear probability model
# Quad max
OLS_nsw_quad <- lm(treat ~ age + agesq + educ + educsq + 
                marr + nodegree + black + hisp + re74 + re75 + u74 +
                u75 + interaction1,data = nsw_dw_cpscontrol)

# Fit propoensity score to quad model
OLS_cpscontrol_quad <- nsw_dw_cpscontrol %>% 
  mutate(pscore = OLS_nsw_quad$fitted.values)

# mean pscore control
pscore_control_quad <- OLS_cpscontrol_quad %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

# 0.01005653

# mean pscore treated
pscore_treated_quad <- OLS_cpscontrol_quad %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.1165476

############
# Cube max
OLS_nsw_cube <- lm(treat ~ age + agesq + agecube + educ + educsq + 
                marr + nodegree + black + hisp + re74 + re75 + u74 +
                u75 + interaction1,data = nsw_dw_cpscontrol)

# Fit propoensity score to quad model
OLS_cpscontrol_cube <- nsw_dw_cpscontrol %>% 
  mutate(pscore = OLS_nsw_cube$fitted.values)

# mean pscore control
pscore_control_cube <- OLS_cpscontrol_cube %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

# 0.010024

# mean pscore treated
pscore_treated_cube <- OLS_cpscontrol_cube %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.1204243
