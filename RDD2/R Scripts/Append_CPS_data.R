# Load in required packages
library(haven)
library(tidyverse)

# Create a path to read data from the source
read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/Joey-Herrera/Causal_inference/main/RDD2/Data/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

nsw_dw <- read_data("nsw_mixtape.dta")

# Append variables as necessary
nsw_lm <- read_data("cps_mixtape.dta") %>% 
  bind_rows(nsw_dw) %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         educcube = educ^3,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         re74sq = re74^2, 
         re74cube = re74^3,
         re75sq = re75^2,
         re75cube = re75^3,
         re78sq = re78^2,
         re75cube = re78^3,
         )

nsw_logit <- read_data("cps_mixtape.dta") %>% 
  bind_rows(nsw_dw) %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         educcube = educ^3,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         re74sq = re74^2, 
         re74cube = re74^3,
         re75sq = re75^2,
         re75cube = re75^3,
         re78sq = re78^2,
         re75cube = re78^3)
