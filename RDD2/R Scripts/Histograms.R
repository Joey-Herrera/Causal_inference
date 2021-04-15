##### Question 1C Histograms

# logit quad control
nsw_logit_quad %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# logit quad treatment
nsw_logit_quad %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# logit cube control
nsw_logit_cube %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# logit cube treatment
nsw_logit_cube %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# linear probability model (quadratic) control
nsw_OLS_quad %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# linear probability model (quadratic) treatment
nsw_OLS_quad %>%
  filter(treat == 1) %>%
  ggplot() +
  geom_histogram(aes(x = pscore))

# linear probability model (cubic) control
nsw_OLS_cube %>%
  filter(treat == 0) %>%
  ggplot() +
  geom_histogram(aes(x = pscore))

# linear probability model (cubic) treatment
nsw_OLS_cube %>%
  filter(treat == 1) %>%
  ggplot() +
  geom_histogram(aes(x = pscore))