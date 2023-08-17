## Tidy Tuesday - Hot Ones

# set up ---------------------------

library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggtext)
library(scales) # added this package to add commas to my y-axis for clarity.

#load data set ---------------------------

episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/episodes.csv')
sauces <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/seasons.csv')

#explore ---------------------------

#what was the average scoville rating by season and then percentage change between each season?

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

a <- ifelse(df$av_scoville == 332770, "yellow", "red4") #to highlight season 5 on the graph - used in theme() below:

##average scoville score per season mapped out across all 21 seasons:
df %>%
  ggplot(aes(x = season, y = av_scoville)) + 
  geom_line(colour = "red") +
  geom_point(colour = "red") +
  geom_label(
    label="The average heat level for season 5 was 332k,\njust shy of the Habenero chili, which ranks 350k on the Scoville scale.",
    x=6,
    y=250000,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "yellow",
    fill="red4",
    hjust = 0) +
  scale_y_continuous(labels = comma) +
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
    panel.grid.major.x = element_line(linetype = 2, colour = a),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()) +
  labs(title = "HOT ONES \U1F525 How Hot is Each Season?",
       subtitle = "Hot Ones is an American YouTube talk show, where celebrities are interviewed while eating\nchicken wings coated in increasingly hotter sauces. Hot sauce heat levels are measured in\nscoville units: the higher the scoville number, the hotter the sauce.",
       caption = "Data source: Wikipedia.",
       x = "Season",
       y = "Average Scoville units")

#save ---------------------------

ggsave(paste0("hot_ones_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 9, height = 6)
        