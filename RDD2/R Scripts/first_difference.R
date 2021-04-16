##### Question 2: First difference for the logit cubic model and quadratic linear probability model
### First difference for the logit cubic model
# Filter re78 to only include people in the treatment group
nsw_logit_cube_trim %>% 
  filter(treat == 1) %>% 
  summary(re78)

# Filter and average on the treatment effect
mean1 <- nsw_logit_cube_trim %>% 
  filter(treat == 1) %>% 
  pull(re78) %>% 
  mean()

# save the 
nsw_logit_cube_trim$y1 <- mean1

# Average treatment effect on the untreated
nsw_logit_cube_trim %>% 
  filter(treat == 0) %>% 
  summary(re78)

mean0 <- nsw_logit_cube_trim %>% 
  filter(treat == 0) %>% 
  pull(re78) %>% 
  mean()

nsw_logit_cube_trim$y0 <- mean0

ate_logit <- unique(nsw_logit_cube_trim$y1 - nsw_logit_cube_trim$y0)
# 1461.0585
nsw_logit_cube_trim <- nsw_logit_cube_trim %>% 
  filter(treat == 1) %>% 
  select(-y1, -y0)

### First difference for quadratic linear probability model
# Filter re78 to only include people in the treatment group
nsw_OLS_quad_trim %>% 
  filter(treat == 1) %>% 
  summary(re78)

# Filter and average on the treatment effect
mean1_OLS <- nsw_OLS_quad_trim %>% 
  filter(treat == 1) %>% 
  pull(re78) %>% 
  mean()

# save the 
nsw_OLS_quad_trim$y1_OLS <- mean1_OLS

# Average treatment effect on the untreated
nsw_OLS_quad_trim %>% 
  filter(treat == 0) %>% 
  summary(re78)

mean0_OLS <- nsw_OLS_quad_trim %>% 
  filter(treat == 0) %>% 
  pull(re78) %>% 
  mean()

nsw_OLS_quad_trim$y0_OLS <- mean0_OLS

ate_OLS <- unique(nsw_OLS_quad_trim$y1_OLS - nsw_OLS_quad_trim$y0_OLS)
# -5014.1128
nsw_OLS_quad_trim <- nsw_OLS_quad_trim %>% 
  filter(treat == 1) %>% 
  select(-y1_OLS, -y0_OLS)