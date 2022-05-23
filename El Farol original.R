library(tidyverse)
library(purrr)
library(data.table)
library(glue)
options(scipen = 100)

Parameters <- tibble(
  n_agents = 100,
  n_memory = 10,
  n_strategies = 8,
  threshold = 60)

H_A = sample(0:100,(Parameters$n_memory*2))
T_H = H_A

Agents <- tibble(Name = rep(glue("Agent {1:Parameters$n_agents}"),
                            (Parameters$n_memory))) %>%
  arrange(Name)

for (i in (1:(Parameters$n_strategies)) ) {
  Agents %>%
    add_column(Strategy = runif(Parameters$n_agents*(Parameters$n_memory+1),
                                -100,100)) -> Agents
  names(Agents)[i+1] <- glue("Strategy_{i}")
}

Agents %>% group_by(Name) %>% mutate(Coeff = rowid(Name)) -> Agents

#

for (i in 1:39) {
  Agents %>% ungroup() %>%
    group_split(Name) %>% 
    map_dfr(~ map_dfr(shift(rev(H_A)[1:Parameters$n_memory*2],
                            n = 1:nrow(.x), type = 'lead'), 
                      function(y) .x %>% 
                        summarise(Name = first(Name), across(starts_with('Strategy'),  
                                                             ~ sum(. * y[seq_along(.)], na.rm = TRUE)))) ) %>%
    mutate(Lag = rowid(Name)) %>%
    mutate(A = rev(H_A)[Lag]) %>% mutate(across(starts_with('Strategy'),
                                                ~ abs(.x - A),
                                                .names = "Error_{str_extract(col, '[0-9]+$')}")) %>%
    group_by(Name) %>%
    summarise(across(starts_with('Error'),
                     ~ sum(.x))) %>% rowwise() %>%
    mutate(choice = which.min(across(starts_with('Error')))) -> Errors
  
  Agents %>%
    mutate(Lag = rowid(Name)) %>%
    mutate(A = rev(H_A)[Lag]) %>% inner_join(Errors, Agents, by = "Name") %>%
    group_by(Name) %>%
    rowwise %>%
    mutate(EX = cur_data()[[choice]] * A) %>%
    ungroup() %>% group_by(Name) %>%
    summarise(EX = sum(EX)) %>%
    summarise(sum(EX < Parameters$threshold)) %>%
    as.integer() -> T_H[length(T_H) + 1]
  
  as.integer(rev(T_H)[1] / 1.2) -> H_A[length(H_A) + 1]
}

rev(H_A)[1:100]
rev(T_H)[1:100]

mean(H_A)
mean(T_H)

median(H_A)
