ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv") %>%
  

library(ggplot2)
library(tidyverse)
install.packages("wesanderson")
# Load
library(wesanderson)

# plot 1

ramen_ratings1 <- ramen_ratings %>%
  filter(style != "Bar") %>%
  filter(style != "Can") %>%
  filter(style != "NA")

ggplot(ramen_ratings1, aes(style, stars, fill = style)) + 
  geom_violin() + 
  labs(title="Ramen Ratings by Packet Style", 
       subtitle="Rating out of 5 stars",
       caption="Source: #tidytuesdays",
       x="Packet Style",
       y="Stars") +
  theme(panel.background = element_blank()) +
  scale_fill_brewer(palette="Spectral")

# plot 2
library(ggalt)
theme_set(theme_classic())

health <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/health.csv")
health$Area <- factor(health$Area, levels=as.character(health$Area))  # for right ordering of the dumbells

# health$Area <- factor(health$Area)
gg <- ggplot(health, aes(x=pct_2013, xend=pct_2014, y=Area, group=Area)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                point.colour.l="#0e668b") + 
  scale_x_continuous(label=percent) + 
  labs(x=NULL, 
       y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Pct Change: 2013 vs 2014", 
       caption="Source: https://github.com/hrbrmstr/ggalt") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

