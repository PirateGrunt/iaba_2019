library(tidyverse)

set.seed(1234)

num_pols <- 1e3
mean <- 10e3
cv <- 0.3
b_0 <- 2
b_1 <- 1 / 10
b_2 <- 1 / 1e3
b_region <- c(-1, 0, 2, 3)

regions <- c('north', 'south', 'east', 'west')
base_date <- as.Date('2001-01-01')
freq_trend <- .0

tbl_policy <- tibble(
  id = seq_len(num_pols)
  , effective_date = base_date
  , premium = runif(num_pols, 1e3, 5e3)
  , years_in_operation = runif(num_pols, 0, 30)
  , number_of_employees = runif(num_pols, 1e3, 3e3)
  , region = sample(regions, num_pols, replace = TRUE)
) %>% 
  mutate(
    effective_date = effective_date + runif(num_pols, 0, 3650 + 2)
    , expiration_date = effective_date + lubridate::years(1) - lubridate::days(1)
    , t = ((effective_date - base_date) / 365.25) %>% as.double()
    , freq_trend = (1 + freq_trend) ^ t
    , region_factor = case_when(
        region == 'north' ~ b_region[1]
        , region == 'south' ~ b_region[2]
        , region == 'east' ~ b_region[3]
        , region == 'west' ~ b_region[4]
    )
    , mu = b_0 * freq_trend + region_factor + b_1 * years_in_operation + b_2 * number_of_employees
    , num_claims = rpois(num_pols, mu)
    
  ) %>% 
  select(
    -mu
    , -region_factor
  ) %>% 
  select(id, region, effective_date, expiration_date, everything())

# tbl_policy <- tbl_policy %>%
#   bind_rows(
#     tribble(~region, ~num_claims, 'north', 500)
#   )

tbl_claim <- tbl_policy %>% 
  filter(
    num_claims > 0
  ) %>% 
  select(policy_id = id, num_claims, effective_date) %>% 
  mutate(
    claim_amount = map(num_claims, rgamma, 1 / cv^2, scale = mean * cv ^ 2)
  ) %>% 
  unnest(claim_amount) %>% 
  mutate(
    id = seq_len(nrow(.))
    , occurrence_date = effective_date + runif(nrow(.), 0, 364)
  ) %>% 
  select(-num_claims, -effective_date) %>% 
  select(policy_id, id, occurrence_date, claim_amount)

save(
  file = file.path('data', 'data.rda')
  , tbl_policy
  , tbl_claim
)