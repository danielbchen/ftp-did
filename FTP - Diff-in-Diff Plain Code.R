library(tidyverse)
library(foreign)
library(broom)
library(plm)
library(lmtest)
library(kableExtra)

# Load in data
admin <- read.dta(paste("/Users/danielchen/Desktop/UChicago/Year Two/Autumn 2020/",
                        "Program Evaluation/Problem Sets/Problem Set 4/ftp_ar.dta",
                        sep = ""))
survey <- read.dta(paste("/Users/danielchen/Desktop/UChicago/Year Two/Autumn 2020/",
                         "Program Evaluation/Problem Sets/Problem Set 4/ftp_srv.dta",
                         sep = ""))

# View snippet of data
admin %>%
  select(1:11) %>%
  head(10) %>%
  kable("latex")

survey %>%
  select(1:10) %>%
  head(10) %>%
  kable("latex")

# Create function that uses a loop to create a vector containing the names of 
# the employment variables we're interested in
make_variable <- function(prefix, start, end) {
  
  vars <- c()
  for (number in start:end) {
    variable <- str_c(prefix, as.character(number))
    vars <- c(vars, variable)
  }
  
  return(vars) 
}

# Create variable names and store to object
pre_vars <- make_variable(prefix = "emppq", start = 1, end = 10)
post_vars <- make_variable(prefix = "empq", start = 1, end = 20)

# View our variables
pre_vars
post_vars

# Merge admin data, only keep valid responses for variable fmi2, derive a 
# new variable named TLyes denoting whether an individual believed in the time 
# limit or not, keep only sampleid, TLyes, and all the employment variables,
# reshape data from wide to long, and add dummy indicating whether employment
# var falls into pre-random assignment or post-random assignment
ftp <- merge(admin, survey, by = "sampleid") %>%
  rename_at(vars(ends_with(".x")), ~str_replace(., "\\..$", "")) %>%
  select(-ends_with(".y")) %>%
  as_tibble() %>%
  filter(fmi2 == 1 | fmi2 == 2) %>%
  mutate(TLyes = case_when(fmi2 == 1 ~ 1,
                           fmi2 == 2 ~ 0)) %>%
  select(sampleid, TLyes, all_of(c(pre_vars, post_vars))) %>%
  pivot_longer(starts_with("e"), names_to = "quarter", values_to = "employed") %>%
  mutate(post_treat = case_when(str_sub(quarter, 1, 5) == "emppq" ~ 0,
                                TRUE ~ 1))

# Preview new dataframe
ftp %>%
  head(5) %>%
  kable("latex")

# Calculate number of NA values
admin %>%
  select(starts_with(c("emppq", "empq"))) %>%
  map(~sum(is.na(.))) %>%
  bind_cols() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(`NA count` = V1) %>% # Using spaces in variable names for presentation
  kable("pipe")

# Calculate employment rates by group
ftp %>%
  group_by(TLyes, post_treat) %>%
  summarise(`Employment Rate` = mean(employed, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  kable("pipe")

# Regress employment explained by belief in the time limit
ftp %>%
  filter(post_treat == 0) %>%
  filter(is.na(employed) == FALSE) %>%
  lm(employed ~ TLyes, data = .) %>%
  tidy() %>%
  kable("pipe")

# Store vector containing coefficients of interest
pre_treat_estimates <- c("quarteremppq2:TLyes",
                         "quarteremppq3:TLyes",
                         "quarteremppq4:TLyes",
                         "quarteremppq5:TLyes",
                         "quarteremppq6:TLyes",
                         "quarteremppq7:TLyes",
                         "quarteremppq8:TLyes",
                         "quarteremppq9:TLyes")

# Run DiD using plm() function and store output to object
period_effects <- plm(
  employed ~ sampleid + quarter + TLyes*quarter, 
  data = ftp %>% filter(!quarter %in% c("empq18", "empq19", "empq20", "emppq10")), 
  index = c("sampleid", "quarter"), 
  model = "within"
)

# Cluster standard errors by sampleid and return a dataframe containing the pre-
# treatment estimates
pre_treat_results <- coeftest(
  period_effects, 
  vcovHC(period_effects, type = "HC1", cluster = "group")
) %>%
  tidy(conf.int = TRUE) %>%
  filter(term %in% pre_treat_estimates)

# View results
pre_treat_results %>%
  kable("pipe")

pre_treat_results %>%
  mutate(
    period = case_when(
      term == "quarteremppq2:TLyes" ~ 8,
      term == "quarteremppq3:TLyes" ~ 7,
      term == "quarteremppq4:TLyes" ~ 6,
      term == "quarteremppq5:TLyes" ~ 5,
      term == "quarteremppq6:TLyes" ~ 4,
      term == "quarteremppq7:TLyes" ~ 3,
      term == "quarteremppq8:TLyes" ~ 2,
      term == "quarteremppq9:TLyes" ~ 1
    )
  ) %>%
  mutate(period = as.numeric(period)) %>%
  ggplot(aes(x = period, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  theme_bw() +
  scale_x_discrete(breaks = c(1:9), labels = c(1:9), limits = c(1:9)) +
  labs(
    x     = "Pre-Treatment Quarter",
    y     = "Estimated Coefficients",
    title = "Estimated Heterogenous Effects in Periods Prior to Random Assignment"
  ) 

# Diff-in-Diff regression
ftp %>%
  filter(!quarter %in% c("empq18", "empq19", "empq20", "emppq10")) %>%
  lm(employed ~ sampleid + quarter + TLyes*post_treat, data = .) %>%
  tidy() %>%
  filter(str_sub(term, 1, 6) == "TLyes:") %>%
  kable("pipe")

# Run DiD using plm() function and store output to object
clustered_plm <- plm(
  employed ~ sampleid + quarter + TLyes*post_treat, 
  data = ftp %>% filter(!quarter %in% c("empq18", "empq19", "empq20", "emppq10")), 
  index = c("sampleid", "quarter"), 
  model = "within"
)

# Cluster standard errors at the individual level 
coeftest(
  clustered_plm, 
  vcovHC(clustered_plm, type = "HC1", cluster = "group")
) %>%
  tidy() %>%
  filter(term == "TLyes:post_treat") %>%
  kable("pipe")

# Run for loop to store a vector containing the names of our coefficients of interest
post_treat_estimates <- c()
for (number in 1:17) {
  var_name <- str_c("quarterempq", number , ":TLyes")
  post_treat_estimates <- c(post_treat_estimates, var_name)
}

# Cluster standard errors by sampleid and return a dataframe containing the
# post-treatment estimates
post_treat_results <- coeftest(
  period_effects, 
  vcovHC(period_effects, type = "HC1", cluster = "group")
) %>%
  tidy(conf.int = TRUE) %>%
  filter(term %in% post_treat_estimates)

# Plot estimates
post_treat_results %>%
  mutate(
    period = case_when(
      term == "quarterempq1:TLyes"  ~ 10,
      term == "quarterempq2:TLyes"  ~ 11,
      term == "quarterempq3:TLyes"  ~ 12,
      term == "quarterempq4:TLyes"  ~ 13,
      term == "quarterempq5:TLyes"  ~ 14,
      term == "quarterempq6:TLyes"  ~ 15,
      term == "quarterempq7:TLyes"  ~ 16,
      term == "quarterempq8:TLyes"  ~ 17,
      term == "quarterempq9:TLyes"  ~ 18,
      term == "quarterempq10:TLyes" ~ 19,
      term == "quarterempq11:TLyes" ~ 20,
      term == "quarterempq12:TLyes" ~ 21,
      term == "quarterempq13:TLyes" ~ 22,
      term == "quarterempq14:TLyes" ~ 23,
      term == "quarterempq15:TLyes" ~ 24,
      term == "quarterempq16:TLyes" ~ 25,
      term == "quarterempq17:TLyes" ~ 26,
    )
  ) %>%
  mutate(period = as.numeric(period)) %>%
  ggplot(aes(x = period, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  theme_bw() +
  scale_x_discrete(breaks = c(10:26), labels = c(10:26), limits = c(10:26)) +
  labs(
    x     = "Post-Treatment Quarter",
    y     = "Estimated Coefficients",
    title = "Estimated Heterogenous Effects in Periods After Random Assignment"
  ) 