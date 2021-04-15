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
logit_model_quad <- glm(treat ~ age + agesq  + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + re74sq + re75 + re75sq +
                     u74 + u75, family = binomial(link = "logit"), 
                 data = nsw_logit)

# Fit propoensity score to quad model
nsw_logit_quad <- nsw_logit %>% 
  mutate(pscore = logit_model_quad$fitted.values)

# mean pscore control
pscore_control_logit_quad <- nsw_logit_quad %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()
# 0.00893

# mean pscore treated
pscore_treated_logit_quad <- nsw_logit_quad %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.2156
####################################
### cube max
logit_model_cube <- glm(treat ~ age + agesq + agecube + educ + educsq + educcube +
                   marr + nodegree + black + hisp + re74 + re74sq + re74cube + re75 + re75sq + re75cube + u74 +
                   u75, family = binomial(link = "logit"), 
                 data = nsw_logit)

# Fit propoensity score to cubic model
nsw_logit_cube <- nsw_logit %>% 
  mutate(pscore = logit_model_cube$fitted.values)

# mean pscore control
pscore_control_cube <- nsw_logit_cube %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()
# 0.008816

# mean pscore treated
pscore_treated_cube <- nsw_logit_cube %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.2255


########################################################
#Linear probability model
# Quad max
OLS_model_quad <- lm(treat ~ age + agesq + educ + educsq + 
                marr + nodegree + black + hisp + re74 + re74sq + re75 + re75sq + u74 +
                u75,data = nsw_lm)

# Fit propoensity score to quad model
nsw_OLS_quad <- nsw_lm %>% 
  mutate(pscore = OLS_model_quad$fitted.values)

# mean pscore control
pscore_control_quad <- nsw_OLS_quad %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

# 0.01005653

# mean pscore treated
pscore_treated_quad <- nsw_OLS_quad %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.1165476

############
# Cube max
OLS_model_cube <- lm(treat ~ age + agesq + agecube + educ + educsq + educcube +
                marr + nodegree + black + hisp + re74 + re74sq + re74cube +
                  re75 + re75sq + re75cube + u74 +
                u75,data = nsw_lm)

# Fit propoensity score to quad model
nsw_OLS_cube <- nsw_lm %>% 
  mutate(pscore = OLS_model_cube$fitted.values)

# mean pscore control
pscore_control_OLScube <- nsw_OLS_cube %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

# 0.010024

# mean pscore treated
pscore_treated_OLScube <- nsw_OLS_cube %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.1194




