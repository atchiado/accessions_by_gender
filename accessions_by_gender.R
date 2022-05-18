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

stats <- access %>%
           group_by(Gender) %>%
           summarise(mean = mean(Accessions),
                     SE = sd(Accessions)) %>%
           mutate(meanpos = mean + 1*SE,
                  meanneg = mean - 1*SE)

stats_male <- stats %>%
                filter(Gender == "Male")
stats_female <- stats %>%
                  filter(Gender == "Female")

diff <- access %>%
          filter(Gender == "Male") %>%
          mutate(x_pos = Accessions + (diff/2))


# Data Viz
ggplot(access) +
  geom_segment(data = male_access,
               aes(x = Accessions, y = Year,
                   yend = female_access$Year, xend = female_access$Accessions),
               color = "#aeb6bf",
               size = 4.5,
               alpha = 0.5) +
  geom_point(aes(x = Accessions, y = Year, color = Gender), size = 4, show.legend = TRUE) +
  ggtitle("Total Army 56A Accession Trends")
