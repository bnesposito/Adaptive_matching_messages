library(dplyr)
library(readxl)

group2_scores = read_xlsx("Group2Pilot_numerical.xlsx") %>% 
    slice(1:6)

data=sapply(group2_scores[,6:18], as.numeric) %>% 
    as_tibble()

PC = prcomp(data, scale = T)

rot = PC$rotation
