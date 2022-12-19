#BBB Question: why are incomes so high? Answer: mainly droping old households
#BBB Question: should I winsorize income?
#BBB Question: should I filter managers?
#BBB Check: Industries - why are affected industries higher paying?
#-------------------------------------------------------------------------------
# Import libraries

library(tidyverse)
library(broom)
library(stargazer)
library(haven)
library(Hmisc)
library(tidysynth)

# Set parameters

min_age = 25
max_age = 65
n_years = 8

# Read in data

df_full <- read_dta("~/Downloads/cps_00005.dta") 

#-------------------------------------------------------------------------------

# Make list of industries covered under legislation

ind90_list <- 
  df_full%>%
  select(ind90ly) %>%
  unique %>% 
  filter((ind90ly >= 580 & ind90ly < 700) | ind90ly %in% c(762, 770)) %>%
  pull()

# Create CPI deflator data frame

cpi <-
  tibble(year = seq(2012,2020),
         inflator = c(0.741, 0.726, 0.715, 0.704,
                      0.703, 0.694, 0.679, 0.663, 0.652) * 	1.507)

inc_max <-
  df_full %>%
  filter(year != 2012,
         asecflag == 1)  %>%
  summarise(wtd.quantile(hhincome, asecwth, 0.975)) %>%
  pull()

ind_desc <-
  read_csv("~/repo/fair_work/analysis/input/industry.csv")

ind90_desc <-
  read_csv("~/repo/fair_work/analysis/input/industry90.csv")

occ_desc <-
  read_csv("~/repo/fair_work/analysis/input/occupation.csv")

#-------------------------------------------------------------------------------

# Create necessary variables

df_clean <-
  df_full %>%
  group_by(year, serial, pernum) %>%
  mutate(numjob = max(numjob)) %>%
  filter(year != 2012,
         between(age, min_age, max_age),
         asecflag == 1) %>% 
  inner_join(cpi, by = "year") %>% 
  mutate(hhincome = hhincome*inflator,
         hhincome = ifelse(hhincome > inc_max, inc_max, hhincome),
         unemp = case_when(empstat %in% c(10, 12) ~ 0,
                         empstat %in% c(20, 21, 22) ~ 1,
                         T ~ NA_real_),
         in_affected_ind = case_when(unemp == 0 & ind90ly %in% ind90_list ~ 1,
                                     unemp == 0 ~ 0,
                                     T ~ NA_real_),
         black = race == 200,
         white = race == 100,
         asian = race %in% c(650, 651, 652),
         hispan = case_when(hispan == 000 ~ 0,
                            hispan < 700 ~ 1, 
                            T ~ NA_real_), 
         female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0,
                            T ~ NA_real_),
         hourly_worker = case_when(paidhour == 2 ~ 1,
                                   paidhour == 1 ~ 0,
                                   T ~ NA_real_),
         metarea = statefip*10000 + metarea,
         effective_year = year - 1,
         ln_income = log(hhincome + 0.001),
         owns_home = ownershp == 10) 

#-------------------------------------------------------------------------------

# Separate dataframe of just affected industry workers

df_industry_workers <-
  df_clean %>%
  filter(hhincome < 9999999 * inflator,
         hhincome >= 0,
         ind90ly %in% ind90_list) %>%
  left_join(occ_desc) %>%
  filter(!str_detect(occ_desc, "manager")) %>%
  group_by(serial) %>%
  filter(pernum == min(pernum)) %>%
  ungroup() 

#-------------------------------------------------------------------------------

# Aggregate individual-level predictors (race, age, sex, 
# unemployment, employment in affected industries)

df_agg_ind <-
  df_clean %>%
  group_by(statefip, effective_year) %>%
  summarise(across(c("unemp", "in_affected_ind", #"single", 
                     "black", "white", "hispan", "asian", "age", "female"), 
                   ~ weighted.mean(.x, asecwt, na.rm = T)))

# Aggregate household-level predictors (race, age, sex, 
# unemployment, employment in affected industries)

df_agg_hh <-
  df_clean %>%
  filter(pernum == 1,
         hhincome < 9999999 * inflator, #should I winsorize income?
         hhincome >= 0) %>%
  group_by(statefip, effective_year) %>%
  summarise(across(c("ln_income", "owns_home", "famsize"), 
                   ~ weighted.mean(.x, asecwth, na.rm = T)),
            n_hh = n())

# Aggregate household-level predictors for workers in affected industries
# (log income)

df_agg_hh_affected <-
  df_industry_workers %>%
  group_by(statefip, effective_year) %>%
  summarise(weight = sum(asecwth),
            ln_income_industry = weighted.mean(ln_income, asecwth))



df_agg <-
  df_agg_hh %>%
  inner_join(df_agg_ind, by = c("statefip", "effective_year"))%>%
  inner_join(df_agg_hh_affected, by = c("statefip", "effective_year"))%>%
  filter(!is.na(unemp),
         !is.na(ln_income_industry),
         n_hh >= 20) %>%
  mutate(statefip = as.character(statefip)) %>%
  group_by(statefip) %>%
  filter(n() == n_years) #make panel


#-------------------------------------------------------------------------------

industries <- 
  df_industry_workers %>% 
  group_by(indly, ind90ly) %>%
  summarise(n = n(),
            share_hourly = weighted.mean(hourly_worker, asecwt, na.rm = T),
            num_w_hour = sum(!is.na(hourly_worker))) %>%
  mutate(share_hourly = ifelse(num_w_hour <= 20, NA_real_, share_hourly)) %>%
  select(indly, ind90ly, share_hourly) %>%
  left_join(ind_desc) %>%
  left_join(ind90_desc)

occupations <- 
  df_industry_workers %>% 
  group_by(occly) %>%
  summarise(n = n()) %>%
  left_join(occ_desc)



#-------------------------------------------------------------------------------

# Create list of predictor variables
predictor_list <- c("ln_income", "unemp", "in_affected_ind", "black", 
                    "white", "hispan", "asian", "age", "female") #add education


# Create list of metro areas (if doing metro analysis)
# metro_list <- c("41900", "412400", "414890", "416442", "417080", "419998", "419999")

# Function that creates synthetic control
synth_func <- function(state, predictors){
  
  output <-
    df_agg %>%
    synthetic_control(outcome = ln_income_industry, # outcome
                      unit = statefip, # unit index in the panel data
                      time = effective_year, # time index in the panel data
                      i_unit = state, # unit where the intervention occurred
                      i_time = 2017, # time period when the intervention occurred
                      generate_placebos=T) %>%
    generate_predictor(time_window = 2012:2016,
                       across(predictors, ~ mean(.x, na.rm = T))) %>%
    generate_predictor(time_window = 2012,
                       ln_income_industry_2012 = ln_income_industry) %>%
    generate_predictor(time_window = 2013,
                       ln_income_industry_2013 = ln_income_industry) %>%
    generate_predictor(time_window = 2014,
                       ln_income_industry_2014 = ln_income_industry) %>%
    generate_predictor(time_window = 2015,
                       ln_income_industry_2015 = ln_income_industry) %>%
    generate_predictor(time_window = 2016,
                       ln_income_industry_2016 = ln_income_industry) %>%
    generate_weights(optimization_window = 2012:2016, # time to use in the optimization task
                     margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
    ) %>%
    generate_control()
  
  output
}

# Create synthetic Oregon
synthetic_oregon <- 
  synth_func("41", predictor_list)

# Create balance table
balance_table <-
  synthetic_oregon %>%
  grab_balance_table()

# Plot trends
synthetic_oregon %>% 
  plot_trends() +
  ylab("Log income in affected industries") +
  xlab("Year") +
  ggtitle("Log income in affected industries (2018 dollars)") +
  scale_color_manual(values = c("#b41e7c", "grey60"),  labels=c('Real Oregon', 'Synthetic Oregon'))+
  scale_linetype_manual(values = c(1, 4),  labels=c('Real Oregon', 'Synthetic Oregon'))

# Plot differences
synthetic_oregon %>%
  plot_differences() +
  ylab("Change in log income in affected industries") +
  xlab("Year") +
  ggtitle("Change in log income in affected industries") 

# Plot placebos
synthetic_oregon %>%
  plot_placebos() +
  ylab("Change in log income in affected industries") +
  xlab("Year") +
  ggtitle("Change in log income in affected industries") +
  scale_color_manual(values = c("#b41e7c", "grey60"),  labels=c('Oregon', 'Placebos'))+
  scale_alpha_manual(values = c(1, 0.4),  labels=c('Oregon', 'Placebos')) +
  scale_size_manual(values = c(1, 0.5),  labels=c('Oregon', 'Placebos'))
  
# Create weight table
weights_table <- 
  synthetic_oregon %>%
  grab_unit_weights() %>%
  filter(weight > 0.01)

synthetic_oregon %>%
  grab_predictor_weights()
