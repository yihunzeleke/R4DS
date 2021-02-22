
library(tidyverse)
set.seed(19900626)

white <- sample(x = c("Y","N"), 50, prob = c(0.45,0.55), replace = T)
black <- sample(x = c("Y","N"), 50, prob = c(0.25,0.75), replace = T)

mex <- sample(x = c("H","N"), 50, prob = c(0.20,0.8), replace = T)
cub <- sample(x = c("H","N"), 50, prob = c(0.15,0.85), replace = T)

ch <- sample(x = c("Y","N"), 50, prob = c(0.17,0.93), replace = T)
jap <- sample(x = c("Y","N"), 50, prob = c(0.05,0.95), replace = T)

other <- sample(x = c("Y","N"), 50, prob = c(0.05,0.95), replace = T)

race <- data.frame(white, black, mex, cub, ch, jap, other)


race <- race %>% 
  mutate(race_ethnicity = case_when(
    white == "Y" &!(black == "Y"| other == "Y"|mex == "H"| cub == "H"|ch == "Y"|jap== "Y") ~ "white",
    black == "Y" &!( white == "Y"|mex == "H"| cub == "H"|ch == "Y"|jap== "Y"| other == "Y") ~ "black",
    mex == "H"| cub == "H" & !(white == "Y"| ch == "Y"|jap== "Y"| other == "Y") ~ "hispanic",
    ch == "Y"|jap== "Y" & !(mex == "H"| cub == "H"|other == "Y"|white == "Y") ~ "asian",
    T ~ "others"))











