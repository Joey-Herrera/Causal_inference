##### Question 3 weighted difference in difference regression
### Cubic logit model
# Manual with non-normalized weights using trimmed data for the cubic logit model
nsw_logit_cube_trim <- nsw_logit_cube_trim %>% 
  mutate(d1 = treat/pscore,
         d0 = (1-treat)/(1-pscore))

s1 <- sum(nsw_logit_cube_trim$d1)
s0 <- sum(nsw_logit_cube_trim$d0)

nsw_logit_cube_trim <- nsw_logit_cube_trim %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ht = y1 - y0)

# Manual with normalized weights with trimmed data the quadratic linear probability model
nsw_logit_cube_trim <- nsw_logit_cube_trim %>% 
  mutate(y1 = (treat*re78/pscore)/(s1/N),
         y0 = ((1-treat)*re78/(1-pscore))/(s0/N),
         norm = y1 - y0)

nsw_logit_cube_trim %>% 
  pull(ht) %>% 
  mean()

nsw_logit_cube_trim %>% 
  pull(norm) %>% 
  mean()

### Quadratic linear proability model
# Manual with non-normalized weights using trimmed data for the cubic logit model
nsw_logit_cube_trim <- nsw_logit_cube_trim %>% 
  mutate(d1 = treat/pscore,
         d0 = (1-treat)/(1-pscore))

s1 <- sum(nsw_logit_cube_trim$d1)
s0 <- sum(nsw_logit_cube_trim$d0)

nsw_logit_cube_trim <- nsw_logit_cube_trim %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ht = y1 - y0)


# Manual with normalized weights with trimmed data the quadratic linear probability model
nsw_logit_cube_trim <- nsw_logit_cube_trim %>% 
  mutate(y1 = (treat*re78/pscore)/(s1/N),
         y0 = ((1-treat)*re78/(1-pscore))/(s0/N),
         norm = y1 - y0)

nsw_logit_cube_trim %>% 
  pull(ht) %>% 
  mean()

nsw_logit_cube_trim %>% 
  pull(norm) %>% 
  mean()
