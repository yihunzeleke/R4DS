#devtools::install_github("epinotes/injurymatrix")
library(tidyverse)
library(injurymatrix)

set.seed(11)

# Assault
Assault <- icd10cm_data150 %>% 
  matrix_intent(inj_col = c(2:6))

Assault %>% 
  count(Assault, Unintentional)

Assault %>%
  select(-diagnosis_1:-ecode2) %>%
  pivot_longer(cols = -uid,
               names_to = "intent",
               values_to = "count") %>%
  group_by(intent) %>%
  summarise_at(vars(count), sum)

# Keywords used  

Assault_2 <- icd10cm_data150 %>% 
  matrix_intent(inj_col = c(2:6), "unintent", "undeterm")

Assault_2 %>% view()

# Motor Vehcile - Unintensional
# matrix_mechanism
mvt <- icd10cm_data150 %>% 
  matrix_mechanism(inj_col = c(2:6), "Motor", "MVT", "unintent")
mvt

# Fall-Unintensinal

fall <- icd10cm_data150 %>% 
  matrix_mechanism(inj_col = c(2:6), "drug", "fall", "pierce")


# Suicidal Attempt

intent_mechanism <- icd10cm_data150 %>% 
  matrix_intent_mechanism(inj_col = c(2:6))

intent_mechanism %>% 
  count(`Intentional Self-harm_Fall`)

intent_mechanism %>% 
  count(Unintentional_Fall)








