rgamma(1,1,1)
###

library(tidyverse)
library(glue)
library(DescTools)

Parameters <- tibble(
  n_memory = 5,
  n_strategies = 3,
  threshold = 60)

Agents <- tibble(Name = rep(glue("Agent {1:100}"),
                            (Parameters$n_memory))) %>%
  arrange(Name) %>%
  add_column(Lag = rep(c(0:(Parameters$n_memory)),100))

for (i in (1:(Parameters$n_strategies)) ) {
  Agents %>% add_column(Strategy = rchisq(100*(Parameters$n_memory),
                                          1)) -> Agents
  names(Agents)[i+2] <- glue("Strategy_{i}")
}

H_attendences = sample(0:100,Parameters$n_memory)

H_attendences = c(0,0,0,0,0)
H_attendences = c(25,25,25,25,25)
H_attendences = c(50,50,50,50,50)
H_attendences = c(75,75,75,75,75)
H_attendences = c(95,95,95,95,95)
H_attendences = c(100,100,100,100,100)

Agents %>% filter(Lag != 0) %>%
  mutate (t = length(H_attendences) - Lag) %>%
  mutate(across(starts_with('Strategy'),
                ~((((H_attendences[t]+1)^(1/.x))*100)/(101^(1/.x))
                ),
                .names = "Error_{str_extract(col, '[0-9]+$')}")) %>%
  mutate(across(starts_with("Error_"),
                ~(replace_na(.x,
                             tail(H_attendences,1))))) %>%
  mutate(across(starts_with('Error'),
                ~abs(.x - tail(H_attendences,1)))) %>%
  group_by(Name) %>%
  summarise(across(starts_with('Error'),
                   ~ sum(.x, na.rm = TRUE))) -> Errors

Errors %>% rowwise() %>%
  mutate(choice = which.min(across(starts_with('Error')))) -> Errors

mean(Errors$Error_1)
mean(Errors$Error_2)
mean(Errors$Error_3)


###

((1/.2) + (1/1.2) + (1/1.3) + (1/4.3) + (1/.1)) -> x

((1/.2) * (1/1.2) * (1/1.3) * (1/4.3) * (1/.1)) -> x

(.2+1.2+1.3+4.3+.1) / 5 -> y


(20 + 30 + 40 + 50 + 60)^(1/x) * 100 /
(500)^(1/x)

40^(1/1.42) * 100 /
100^(1/1.42)

lm(y ~ x)

y = 55
x = c(20, 30, 40, 50, 60)


### metodo equalizzatore


runif(5,0,1) -> a
a / sum(a) -> a

runif(5,0,100) -> b

a*b

sum(a*b)

a


qplot(sum(a*b))

rep(100,5) -> b

