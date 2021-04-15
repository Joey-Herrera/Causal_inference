# Question 1D. trim the propsenity scores and recreate the histograms from 1C

# trimming propensity score for logit and OLS data
# logit model (quadratic)
nsw_logit_quad_trim <- nsw_logit_quad %>% 
  filter(!(pscore >= 0.9)) %>% 
  filter(!(pscore <= 0.1))

# logit model (cubic)
nsw_logit_cube_trim <- nsw_logit_cube %>% 
  filter(!(pscore >= 0.9)) %>% 
  filter(!(pscore <= 0.1))

# linear probability model (quadratic)
nsw_OLS_quad_trim <- nsw_OLS_quad %>% 
  filter(!(pscore >= 0.9)) %>% 
  filter(!(pscore <= 0.1))

# linear probability model (cubic)
nsw_OLS_cube_trim <- nsw_OLS_cube %>% 
  filter(!(pscore >= 0.9)) %>% 
  filter(!(pscore <= 0.1))

###################################################################
# Rerun Question 1B
##### Logit 
### quadratic max

# mean pscore control
pscore_control_logit_quad_trim <- nsw_logit_quad_trim %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()
# 0.2506

# mean pscore treated
pscore_treated_logit_quad_trim <- nsw_logit_quad_trim %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.2768
####################################
### cube max

# mean pscore control
pscore_control_cube_trim <- nsw_logit_cube_trim %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()
# 0.2516

# mean pscore treated
pscore_treated_cube_trim <- nsw_logit_cube_trim %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.2902


########################################################
#Linear probability model
# Quad max
# mean pscore control
pscore_control_quad_trim <- nsw_OLS_quad_trim %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

# 0.1286

# mean pscore treated
pscore_treated_quad_trim <- nsw_OLS_quad_trim %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.1374

############
#linear proability model (cubic)
# mean pscore control
pscore_control_OLS_cube_trim <- nsw_OLS_cube_trim %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

# 0.1294

# mean pscore treated
pscore_treated_OLS_cube_trim <- nsw_OLS_cube_trim %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()
# 0.1404

###############################
# rerun the histograms from 1C

# logit quad control
nsw_logit_quad_trim %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# logit quad treatment
nsw_logit_quad_trim %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# logit cube control
nsw_logit_cube_trim %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# logit cube treatment
nsw_logit_cube_trim %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# linear probability model (quadratic) control
nsw_OLS_quad_trim %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# linear probability model (quadratic) treatment
nsw_OLS_quad_trim %>%
  filter(treat == 1) %>%
  ggplot() +
  geom_histogram(aes(x = pscore))

# linear probability model (cubic) control
nsw_OLS_cube_trim %>%
  filter(treat == 0) %>%
  ggplot() +
  geom_histogram(aes(x = pscore))

# linear probability model (cubic) treatment
nsw_OLS_cube_trim %>%
  filter(treat == 1) %>%
  ggplot() +
  geom_histogram(aes(x = pscore))