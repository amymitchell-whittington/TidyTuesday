## Tidy Tuesday - Hot Ones

# set up ---------------------------

library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggtext)

#load data set ---------------------------

episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/episodes.csv')
sauces <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/seasons.csv')

#explore ---------------------------

#the average scoville rating by season and the percentage change between each season - did they get overall hotter as the seasons continued?

###note: If you want to sum every n consecutive numbers use colSums
#If you want to sum every nth number use rowSums
n <- 10
v <- sauces$scoville

#total scoville per season function:
colSums(matrix(v, nrow=n))

#average scoville rating 

av_scoville <- colSums(matrix(v, nrow=n))/10

#to make a data.frame
season <- c(1:21)
av_scoville <- colSums(matrix(v, nrow=n))/10

df <- data.frame(season, av_scoville)

#to round percentage up to nearest integer
df$av_scoville <- ceiling(df$av_scoville)

##the percentage change between each average scoville ranking per season

perc_change <- df %>%
  mutate(lag = lag(av_scoville),
         perc_change = (av_scoville - lag) / lag * 100) %>%
  mutate(perc_change = ifelse(is.na(perc_change), 0, perc_change))

#plot ---------------------------

perc_change %>%
  ggplot(aes(x = season, y = perc_change, label = perc_change)) +
  geom_line(colour = "red") +
  geom_point(colour = "red") +
  geom_label(
    label="134%", 
    x=4,
    y=134.3,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.10,
    color = "yellow",
    fill="red4",
    hjust = -0.1) +
  geom_label(
    label="The average hot sauce scoville score per season fluctuated\ndramatically in the early seasons of the show,\nand evened out after season 7.", 
    x=8,
    y=20,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "yellow",
    fill="red4",
    hjust = 0) +
  scale_y_continuous(limits = c(-20, 140), breaks = c(-20, 0, 20, 40, 60, 80, 100, 120, 140), labels = scales::percent_format(scale = 1)) +
  #geom_richtext(aes(x = 8.75, y = 340, label = ""), size = 3, hjust = 0, vjust = "top", fill = NA, label.color = NA, lineheight = 1.3) +
  scale_x_continuous(breaks = c(1:21)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, colour = "red2", face = "bold"),
    plot.subtitle = element_text(size = 12, colour = "red2"),
    plot.caption = element_text(size = 6, colour = "red2", face = "italic"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.title.x = element_text(colour = "red2", face = "bold", size = 11),
    axis.text.x = element_text(colour = "yellow"),
    axis.title.y = element_text(colour = "red2", face = "bold", size = 11),
    axis.text.y = element_text(colour = "yellow"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "red4", linetype = 2),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()) +
          labs(title = "HOT ONES \U1F525",
               subtitle = "Hot Ones is an American YouTube talk show, where celebrities are interviewed while eating\nchicken wings coated in increasingly hotter sauces. There are 10 sauces per episode, each with\nit's own scoville unit score - starting from as mild as 747 to as hot as 2,000,000.",
            caption = "Data source: Wikipedia.",
               x = "Season",
               y = "% change in average scoville units (season over season)")
        