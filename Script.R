library(tidyverse)
library(glue)
library(DescTools)
options(scipen = 100)

Parameters <- tibble(
  n_memory = 5,
  n_strategies = 3,
  threshold = 60)

Agents <- tibble(Name = rep(glue("Agent {1:100}"),
                            (Parameters$n_memory))) %>%
  arrange(Name) %>%
  add_column(Lag = rep(c(0:(Parameters$n_memory-1)),100))

for (i in (1:(Parameters$n_strategies)) ) {
  Agents %>% add_column(Strategy = rchisq(100*(Parameters$n_memory),
                                          1)) -> Agents
  names(Agents)[i+2] <- glue("Strategy_{i}")
}

H_attendences = sample(0:100,Parameters$n_memory)

#

Agents %>% filter(Lag != 0) %>%
  mutate (t = length(H_attendences) - Lag) %>%
  mutate(across(starts_with('Strategy'),
                ~((((H_attendences[t]+1)^(1/.x))*100)/(101^(1/.x))
                ),
                .names = "Coeff_{str_extract(col, '[0-9]+$')}")) %>%
  mutate(across(starts_with("Coeff_"),
                ~(replace_na(.x,
                             0)))) %>%
  group_by(Name) %>%
  summarise(across(starts_with('Coeff'),
                   ~ abs(mean(.x, na.rm = TRUE) - 
                     tail(H_attendences,1)))) -> Errors

Errors %>% rowwise() %>%
  mutate(choice = which.min(across(starts_with('Coeff')))) -> Errors

Agents %>% filter(Lag == 0) %>%
  add_column(choice = Errors$choice) %>%
  rowwise %>%
  mutate(X = cur_data()[[choice+2]]) %>% ungroup %>%
  mutate(E_X = ((((tail(H_attendences,1)+1)^(1/X))*100)/(101^(1/X)))) %>%
  mutate(E_X = replace_na(E_X,0) -
           Parameters$threshold) %>%
  summarise(sum(E_X < 0)) %>% as.integer() -> H_attendences[length(H_attendences) + 1]

H_attendences
