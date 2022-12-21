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
library(tidycensus)

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

# Define max income for winsorization

inc_max <-
  df_full %>%
  filter(year != 2012,
         asecflag == 1)  %>%
  summarise(wtd.quantile(hhincome, asecwth, 0.975)) %>%
  pull()

# Read in industry and occupation descriptions

ind_desc <- read_csv("~/repo/fair_work/analysis/input/industry.csv")

ind90_desc <- read_csv("~/repo/fair_work/analysis/input/industry90.csv")

occ_desc <- read_csv("~/repo/fair_work/analysis/input/occupation.csv")

state_fips <- read_csv("~/repo/fair_work/analysis/input/statefips.csv")
  
# Create list of predictor variables
predictor_list <- c("ln_income", "unemp", "in_affected_ind") 
# predictor_list <- c("ln_income", "unemp", "in_affected_ind", "age", "female") #add education



plot_placebos_5 <- function (data, time_window = NULL, prune = TRUE) 
{
  if (!(".meta" %in% colnames(data))) {
    stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")
  }
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  treatment_unit <- data$.meta[[1]]$treatment_unit[1]
  unit_index <- data$.meta[[1]]$unit_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]
  if (is.null(time_window)) {
    time_window <- unique(data$.original_data[[1]][[time_index]])
  }
  plot_data <- data %>% grab_synthetic_control(placebo = TRUE) %>% 
    dplyr::mutate(diff = real_y - synth_y) %>% dplyr::filter(time_unit %in% 
                                                               time_window) %>% dplyr::mutate(type_text = ifelse(.placebo == 
                                                                                                                   0, treatment_unit, "control units"), type_text = factor(type_text, 
                                                                                                                                                                           levels = c(treatment_unit, "control units")))
  caption <- ""
  if (prune) {
    sig_data = data %>% grab_signficance(time_window = time_window)
    thres <- sig_data %>% dplyr::filter(type == "Treated") %>% 
      dplyr::pull(pre_mspe) %>% sqrt(.)
    retain_ <- sig_data %>% dplyr::select(unit_name, pre_mspe) %>% 
      dplyr::filter(sqrt(pre_mspe) <= thres * 5) %>% dplyr::pull(unit_name)
    plot_data <- plot_data %>% dplyr::filter(.id %in% retain_)
    caption <- "Pruned all placebo cases with a pre-period RMSPE exceeding two times the treated unit's pre-period RMSPE."
  }
  plot_data %>% ggplot2::ggplot(ggplot2::aes(time_unit, diff, 
                                             group = .id, color = type_text, alpha = type_text, size = type_text)) + 
    ggplot2::geom_hline(yintercept = 0, color = "black", 
                        linetype = 2) + ggplot2::geom_vline(xintercept = trt_time, 
                                                            color = "black", linetype = 3) + ggplot2::geom_line() + 
    ggplot2::scale_color_manual(values = c("#b41e7c", "grey60")) + 
    ggplot2::scale_alpha_manual(values = c(1, 0.4)) + ggplot2::scale_size_manual(values = c(1, 
                                                                                            0.5)) + ggplot2::labs(color = "", alpha = "", size = "", 
                                                                                                                  y = outcome_name, x = time_index, title = paste0("Difference of each '", 
                                                                                                                                                                   unit_index, "' in the donor pool"), caption = caption) + 
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
}

#-------------------------------------------------------------------------------
### ASEC analysis (age, race, sex, income, industry of employment)

# Create necessary variables from ASEC
df_clean <-
  df_full %>%
  filter(year != 2012,
         between(age, min_age, max_age),
         asecflag == 1) %>% 
  inner_join(cpi, by = "year") %>% 
  mutate(hhincome = ifelse(hhincome > 9999000 | hhincome < 0, NA_real_, hhincome),
         hhincome = hhincome*inflator,
         hhincome = ifelse(hhincome > inc_max, inc_max, hhincome),
         unemp_asec = case_when(workly == 2 ~ 0,
                                wantjob == 0 ~ 1,
                                T ~ NA_real_),
         in_affected_ind = case_when(unemp_asec == 0 & ind90ly %in% ind90_list ~ 1,
                                     unemp_asec == 0 ~ 0,
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
         owns_home = ownershp == 10,
         cpsidp = ifelse(cpsidp == 0, NA_real_, cpsidp)) 

# Separate dataframe of just affected industry workers (ASEC)

df_industry_workers_asec <-
  df_clean %>%
  filter(!is.na(hhincome),
         ind90ly %in% ind90_list) %>%
  left_join(occ_desc) %>%
  filter(!str_detect(occ_desc, "manager")) %>%
  group_by(serial) %>%
  filter(pernum == min(pernum)) %>%
  ungroup() 

# Aggregate individual-level predictors from ASEC (race, age, sex, 
# unemployment, employment in affected industries)

df_agg_ind_asec <-
  df_clean %>%
  group_by(statefip, effective_year) %>%
  summarise(across(c("in_affected_ind","black", "white", "hispan",
                     "asian", "age", "female"), 
                   ~ weighted.mean(.x, asecwt, na.rm = T)))

# Aggregate household-level predictors (race, age, sex, 
# unemployment, employment in affected industries)

df_agg_hh_asec <-
  df_clean %>%
  filter(pernum == 1,
         !is.na(hhincome)) %>%
  group_by(statefip, effective_year) %>%
  summarise(across(c("ln_income"), 
                   ~ weighted.mean(.x, asecwth)))

# Aggregate household-level predictors for workers in affected industries
# (log income)

df_agg_hh_affected_asec <-
  df_industry_workers_asec %>%
  group_by(statefip, effective_year) %>%
  summarise(ln_income_industry = weighted.mean(ln_income, asecwth))

#-------------------------------------------------------------------------------
### March Basic aggregation (num jobs, unemployment rates)

# Create necessary variables from March Basic

df_march_basic <-
  df_full %>%
  filter(year != 2020,
         between(age, min_age, max_age),
         asecflag == 2) %>% 
  transmute(unemp = case_when(empstat %in% c(10, 12) ~ 0,
                              empstat %in% c(20, 21, 22) ~ 1,
                              T ~ NA_real_),
            numjob = case_when(multjob == 1 ~ 1,
                               multjob == 2 & numjob == 2 ~ 2,
                               multjob == 2 & numjob == 3 ~ 3,
                               multjob == 2 & numjob == 4 ~ 4,
                               T ~ NA_real_),
            metarea = statefip*10000 + metarea,
            effective_year = year,
            statefip,
            wtfinl,
            cpsidp,
            ind1990,
            occ,
            serial,
            pernum) 


df_industry_workers_mb <-
  df_march_basic %>%
  filter(ind1990 %in% ind90_list) %>%
  left_join(occ_desc %>% rename(occ = "occly")) %>%
  filter(!str_detect(occ_desc, "manager")) %>%
  group_by(serial) %>%
  filter(pernum == min(pernum)) %>%
  ungroup() 

# Aggregate individual-level predictors from March Basic (unemployment)

df_agg_ind_mb <-
  df_march_basic %>%
  group_by(statefip, effective_year) %>%
  summarise(across(c("unemp"), 
                   ~ weighted.mean(.x, wtfinl, na.rm = T)))


df_agg_ind_affected_mb <-
  df_industry_workers_mb %>%
  filter(unemp == 0) %>%
  group_by(statefip, effective_year) %>%
  summarise(numjobs = weighted.mean(numjob, wtfinl),
            more_than_one_job = weighted.mean(numjob > 1, wtfinl))

#-------------------------------------------------------------------------------

df_agg <-
  df_agg_ind_asec %>%
  inner_join(df_agg_hh_asec, by = c("statefip", "effective_year"))%>%
  inner_join(df_agg_ind_mb, by = c("statefip", "effective_year"))%>%
  inner_join(df_agg_hh_affected_asec, by = c("statefip", "effective_year"))%>%
  inner_join(df_agg_ind_affected_mb, by = c("statefip", "effective_year"))%>%
  filter(!is.na(unemp),
         !is.na(ln_income_industry)) %>%
  mutate(statefip = as.character(statefip)) %>%
  group_by(statefip) %>%
  filter(n() == n_years) %>%
  filter(!(statefip %in% c(36, 6, 53, 42, 17)))


#-------------------------------------------------------------------------------

industries <- 
  df_industry_workers_asec %>% 
  group_by(indly, ind90ly) %>%
  summarise(n = n(),
            share_hourly = weighted.mean(hourly_worker, asecwt, na.rm = T),
            num_w_hour = sum(!is.na(hourly_worker))) %>%
  mutate(share_hourly = ifelse(num_w_hour <= 20, NA_real_, share_hourly)) %>%
  select(indly, ind90ly, share_hourly) %>%
  left_join(ind_desc) %>%
  left_join(ind90_desc)

occupations <- 
  df_industry_workers_asec %>% 
  group_by(occly) %>%
  summarise(n = n()) %>%
  left_join(occ_desc)



#-------------------------------------------------------------------------------

### INCOME RESULTS

# Function that creates synthetic control for income
synth_func_income<- function(state, predictors){
  
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
                     #margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
    ) %>%
    generate_control()
  
  output
}

# Create synthetic Oregon for income analysis
synthetic_oregon_income <- 
  synth_func_income("41", predictor_list)

# Create balance table
balance_table_income <-
  synthetic_oregon_income %>%
  grab_balance_table()

# Plot trends
synthetic_oregon_income %>% 
  plot_trends() +
  ylab("Log household income") +
  xlab("Year") +
  ggtitle("") +
  scale_color_manual(values = c("#b41e7c", "grey60"),  labels=c('Real Oregon', 'Synthetic Oregon'))+
  scale_linetype_manual(values = c(1, 4),  labels=c('Real Oregon', 'Synthetic Oregon')) +
  ylim(c(10.7, 11.2)) +
  geom_vline(xintercept = 2018, color = "black", linetype = 2) +
  labs(caption = "")

ggsave("~/repo/fair_work/analysis/output/income_trends.png", w = 8, h = 5)

# Plot placebos
synthetic_oregon_income %>%
  plot_placebos_5() +
  ylab("Change in log household income") +
  xlab("Year") +
  ggtitle("") +
  scale_color_manual(values = c("#b41e7c", "grey60"),  labels=c('Oregon', 'Placebos'))+
  scale_alpha_manual(values = c(1, 0.4),  labels=c('Oregon', 'Placebos')) +
  scale_size_manual(values = c(1, 0.5),  labels=c('Oregon', 'Placebos')) +
  geom_vline(xintercept = 2018, color = "black", linetype = 3) +
  labs(caption = "")

ggsave("~/repo/fair_work/analysis/output/income_placebos.png", w = 8, h = 5)

# Create weight table
weights_table_inc <- 
  synthetic_oregon_income %>%
  grab_unit_weights() %>%
  filter(weight > 0.01) %>%
  mutate(statefip = as.numeric(unit),
         weight = round(weight, digits = 3)) %>%
  left_join(state_fips) %>%
  transmute(`State` = stname,
            Weight = weight) %>%
  arrange(desc(Weight))


stargazer(weights_table_inc, 
          type = "latex",
          summary = F, 
          rownames = FALSE,
          out = "~/repo/fair_work/analysis/output/inc_weight_table.tex")


synthetic_oregon_income %>%
  grab_predictor_weights()

#Inference
t <-
  synthetic_oregon_income %>% grab_signficance() 

ggplot(t, aes(x = mspe_ratio))+
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 28.5862559, color = "black", linetype = 2) +
  theme_minimal() +
  ylab("Frequency") +
  xlab("RMSPE")

treat_inc <-
  synthetic_oregon_income %>% 
  grab_synthetic_control() %>%
  mutate(post = time_unit >= 2017) %>%
  group_by(post) %>%
  summarise(real = mean(real_y),
            synth = mean(synth_y)) %>%
  mutate(first_diff = real - synth) %>%
  summarise(att_income = last(first_diff) - first(first_diff)) %>%
  pull

pval_inc <-
  synthetic_oregon_income %>%
  grab_signficance() %>%
  filter(unit_name == "41") %>%
  select(fishers_exact_pvalue) %>%
  pull

# Create synthetic Oregon Num jobs 
synth_func_numjobs <- function(state, predictors){
  
  output <-
    df_agg %>%
    synthetic_control(outcome = more_than_one_job, # outcome
                      unit = statefip, # unit index in the panel data
                      time = effective_year, # time index in the panel data
                      i_unit = state, # unit where the intervention occurred
                      i_time = 2017, # time period when the intervention occurred
                      generate_placebos=T) %>%
    generate_predictor(time_window = 2012:2016,
                       across(predictors, ~ mean(.x, na.rm = T))) %>%
    generate_predictor(time_window = 2012,
                       numjobs_2012 = more_than_one_job) %>%
    generate_predictor(time_window = 2013,
                       numjobs_2013 = more_than_one_job) %>%
    generate_predictor(time_window = 2014,
                       numjobs_2014 = more_than_one_job) %>%
    generate_predictor(time_window = 2015,
                       numjobs_2015 = more_than_one_job) %>%
    generate_predictor(time_window = 2016,
                       numjobs_2016 = more_than_one_job) %>%
    generate_weights(optimization_window = 2012:2016, # time to use in the optimization task
                     # margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
    ) %>%
    generate_control()
  
  output
}

synthetic_oregon_numjobs <- 
  synth_func_numjobs("41", predictor_list)

# Create balance table
balance_table_numjobs <-
  synthetic_oregon_numjobs %>%
  grab_balance_table()

# Plot trends
synthetic_oregon_numjobs %>% 
  plot_trends() +
  ylab("Share holding more than one job") +
  xlab("Year") +
  ggtitle("") +
  scale_color_manual(values = c("#b41e7c", "grey60"),  labels=c('Real Oregon', 'Synthetic Oregon'))+
  scale_linetype_manual(values = c(1, 4),  labels=c('Real Oregon', 'Synthetic Oregon')) +
  ylim(c(0, 0.1)) +
  geom_vline(xintercept = 2018, color = "black", linetype = 2) +
  labs(caption = "")

ggsave("~/repo/fair_work/analysis/output/job_trends.png", w = 8, h = 5)

# Plot placebos
synthetic_oregon_numjobs %>%
  plot_placebos_5(prune = T) +
  ylab("Change in share holding more than one job") +
  xlab("Year") +
  ggtitle("") +
  scale_color_manual(values = c("#b41e7c", "grey60"),  labels=c('Oregon', 'Placebos'))+
  scale_alpha_manual(values = c(1, 0.4),  labels=c('Oregon', 'Placebos')) +
  scale_size_manual(values = c(1, 0.5),  labels=c('Oregon', 'Placebos')) +
  geom_vline(xintercept = 2018, color = "black", linetype = 3) +
  labs(caption = "")

ggsave("~/repo/fair_work/analysis/output/job_placebos.png", w = 8, h = 5)

# Create weight table
weights_table_nj <- 
  synthetic_oregon_numjobs %>%
  grab_unit_weights() %>%
  filter(weight > 0.01) %>%
  mutate(statefip = as.numeric(unit),
         weight = round(weight, digits = 3)) %>%
  left_join(state_fips) %>%
  transmute(`State` = stname,
            Weight = weight) %>%
  arrange(desc(Weight))

stargazer(weights_table_nj, 
          type = "latex",
          summary = F, 
          rownames = FALSE,
          out = "~/repo/fair_work/analysis/output/jobs_weight_table.tex")


synthetic_oregon_numjobs %>%
  grab_predictor_weights()

synthetic_oregon_numjobs %>% plot_mspe_ratio()

#Inference
t <-
  synthetic_oregon_numjobs %>% grab_signficance() 

treat_nj <-
  synthetic_oregon_numjobs %>% 
  grab_synthetic_control() %>%
  mutate(post = time_unit >= 2017) %>%
  group_by(post) %>%
  summarise(real = mean(real_y),
            synth = mean(synth_y)) %>%
  mutate(first_diff = real - synth) %>%
  summarise(att_income = last(first_diff) - first(first_diff)) %>%
  pull

pval_nj <-
  synthetic_oregon_numjobs %>%
  grab_signficance() %>%
  filter(unit_name == "41") %>%
  select(fishers_exact_pvalue) %>%
  pull

ggplot(t, aes(x = mspe_ratio))+
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 12.32106869, color = "black", linetype = 2) +
  theme_minimal() +
  ylab("Frequency") +
  xlab("RMSPE")


#Ballance output

balance_table <-
  balance_table_income %>%
  bind_rows(balance_table_numjobs %>% filter(str_detect(variable, "^num"))) %>%
  mutate(across(c("41", "synthetic_41", "donor_sample"), ~round(.x, digits = 3))) %>%
  rename(Variable = "variable") %>%
  mutate(Variable = c("Share workers in affected industry",
                      "Log household income (all workers)",
                      "Unemployment rate",
                      "Log household income (affected workers) - 2012",
                      "Log household income (affected workers) - 2013",
                      "Log household income (affected workers) - 2014",
                      "Log household income (affected workers) - 2015",
                      "Log household income (affected workers) - 2016",
                      "Share affected workers with multipe jobs - 2012",
                      "Share affected workers with multipe jobs - 2013",
                      "Share affected workers with multipe jobs - 2014",
                      "Share affected workers with multipe jobs - 2015",
                      "Share affected workers with multipe jobs - 2016")) %>%
  rename(`Real Oregon` = "41",
         `Synthetic Oregon` = "synthetic_41",
         `Unweighted Donor Sample` = "donor_sample")

stargazer(balance_table, 
          type = "latex",
          summary = F, 
          rownames = FALSE,
          
          out = "~/repo/fair_work/analysis/output/balance_table.tex")

reg_table <-
  tibble(` ` = c("Average treatment effect", "RMSPE p-value"),
         `Log household income` = c(treat_inc, pval_inc),
         `Share working multiple jobs` = c(treat_nj, pval_nj)) %>%
  mutate(across(c(`Log household income`, `Share working multiple jobs`), ~round(.x, digits = 4)))

stargazer(reg_table, 
          type = "latex",
          summary = F, 
          rownames = FALSE,
          out = "~/repo/fair_work/analysis/output/ate_table.tex")

