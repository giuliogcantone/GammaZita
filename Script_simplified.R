library(tidyverse)
library(glue)
options(scipen = 100)

Parameters <- tibble(
  n_agents = 100,
  n_memory = 5,
  n_strategies = 3,
  threshold = 60,
  N_coeff = n_agents*n_memory)

H_A = sample(0:100,(Parameters$n_memory*2))

Agents <- tibble(Name = rep(glue("Agent {1:Parameters$n_agents}"),
                            (Parameters$n_memory))) %>%
  arrange(Name) %>%
  add_column(Lag = rep(c(1:(Parameters$n_memory)),Parameters$n_agents))

for (i in (1:(Parameters$n_strategies)) ) {
  Agents %>%
    add_column(Strategy = runif(
    Parameters$n_agents*(Parameters$n_memory),
    0,1)) -> Agents
  names(Agents)[i+2] <- glue("Strategy_{i}")
}

Agents %>% group_by(Name) %>% mutate(
  across(starts_with('Strategy'),
  ~(.x/sum(.x))
)) %>% mutate (t = length(H_A) - Lag) -> Agents

A = tibble(.rows = 5)
for (i in (1:Parameters$n_memory)) {
  A %>% add_column(!! str_c("Lag_",i) := rev(H_A)[(i+1):(i+Parameters$n_memory)]) -> A}

Agents %>% summarise(across(
  starts_with("Strategy"),
  ~(.x*A))) -> Coeff

do.call(data.frame, Coeff) %>% as_tibble() %>% group_by(Name) %>%
  summarise(across(
    starts_with("Strategy"),
    ~(sum(.x))
  )) -> Coeff

Coeff
H_A

  group_by(Name) %>%
  summarise(across(starts_with('Coeff'),
                   ~ abs(mean(.x, na.rm = TRUE) - 
                           tail(H_A,1)))) -> Errors