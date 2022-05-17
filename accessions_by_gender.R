library(tidyverse)
library(ggtext)
library(extrafont)


## Load Data
accessions <- read.csv(file = "~/Desktop/work/data/r/accessions_by_gender/accessions_by_gender.csv")


## Data Wrangling
access <- accessions %>%
            mutate(diff = Male - Female) %>%
            pivot_longer(cols = c(Male, Female)) %>%
            rename(Gender = name,
                   Accessions = value)

male_access <- access %>%
                 filter(Gender == "Male")
female_access <- access %>%
                   filter(Gender == "Female")