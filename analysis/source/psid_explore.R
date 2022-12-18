library(tidyverse)

df <- get_pnadc(year = 2013, quarter = 1)

var_list <- c("Ano", "Trimestre", "UF", "V2007", "V4012")

df_try <- 
  df$variables %>%
  select(var_list)

df_clean <- 
  df_try %>%
  slice(1:20000) %>%
  transmute(year = Ano,
            quarter = Trimestre,
            state = UF,
            sex = case_when(V2007 == "Mulher" ~ "female",
                            V2007 == "Homem" ~ "male"),
            type_work = case_when(V4012 == "Trabalhador doméstico" ~ "domestic worker",
                                  V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" ~ "military",
                                  V4012 == "Empregado do setor privado" ~ "private sector",
                                  V4012 == "Empregado do setor público (inclusive empresas de economia mista)" ~ "public sector",
                                  V4012 == "Empregador" ~ "employer",
                                  V4012 == "Trabalhador familiar não remunerado" ~ "unpaid family worker",
                                  V4012 == "Conta própria" ~ "self-employed")) %>%
  group_by(type_work, sex) %>%
  summarise(count = n())
