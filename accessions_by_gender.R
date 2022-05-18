library(tidyverse)
library(ggtext)
library(extrafont)
loadfonts(device = "win", quiet = TRUE)


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
          filter(Gender == "Female") %>%
          mutate(x_pos = Accessions + (diff/2))


# Data Viz
ggplot(access) +
  geom_rect(xmin = stats_male$meanneg, xmax = stats_male$meanpos,
            ymin = 2022, ymax = 2005, fill = "#00bfc4", alpha = 0.1) +
  geom_vline(xintercept = stats_male$mean, linetype = "solid",
             size = 0.5, alpha = 0.7, color = "#00bfc4") +
  geom_rect(xmin = stats_female$meanneg, xmax = stats_female$meanpos,
            ymin = 2022, ymax = 2005, fill = "#f8766d", alpha = 0.1) +
  geom_vline(xintercept = stats_female$mean, color = "#f8766d", linetype = "solid",
             size = 0.5, alpha = 0.7) +
  geom_segment(data = male_access,
               aes(x = Accessions, y = Year,
                   yend = female_access$Year, xend = female_access$Accessions),
               color = "#aeb6bf",
               size = 4.5,
               alpha = 0.5) +
  geom_point(aes(x = Accessions, y = Year, color = Gender), size = 4, show.legend = FALSE) +
    scale_color_manual(values = c("#f8766d", "#00bfc4")) +
  geom_text(data = diff,
            aes(label = paste("Δ: ", diff), x = x_pos, y = Year),
            fill = "white",
            color = "#4a4e4d",
            size = 2.5) +
  scale_x_continuous(limits = c(50, 150)) +
  geom_text(x = stats_male$mean - 1.5, y = 2006, label = "MEAN",
            angle = 90, size = 2, color = "#00bfc4") +
  geom_text(x = stats_male$meanpos - 1.5, y = 2006, label = "STDEV",
            angle = 90, size = 2, color = "#00bfc4") +
  facet_grid(Year ~ ., scales = "free", switch = "y") +
  ggtitle("56A Accessions Trends") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(color = "#4a4e4d"),
        strip.text.y.left = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,0.5,1, "cm")) +
  labs(subtitle = "<span style = 'color: #00bfc4;'>**Male**</span> and <span style = 'color: #f8766d;'>**Female**</span> Total Army Accessions from 2006 to 2021<br>",
       caption = "Plot by Anthony Chiado") +
  theme(plot.caption = element_markdown(hjust = 0, lineheight = 1.5),
        plot.subtitle = element_markdown(size = 14, hjust = -0.6),
        plot.title = element_text(size = 16, hjust = -.1))

