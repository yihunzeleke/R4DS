library(janitor)
library(tidyverse)

df <- mtcars %>% 
  mutate(cyl = factor(cyl), 
         gear = factor(gear),
         transmision = ifelse(am == 0, "automatic", "manual"))

df %>% 
  tabyl(cyl, transmision) %>% 
  adorn_totals('row') %>% 
  adorn_percentages('row') %>% 
  adorn_pct_formatting() %>%
  adorn_ns() %>% 
  adorn_title('combined')

test <- df %>% 
  tabyl(cyl, transmision,gear) %>% 
  adorn_totals('row')
 
test_df <- as.data.frame(test)



