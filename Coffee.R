library(ggalt)
library(tidyverse)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

coffee_total <- coffee_ratings %>%
  group_by(country_of_origin) %>%
  summarise(mean = round(mean(total_cup_points), 1)) %>%
  top_n(10)

coffee_aroma <- coffee_ratings %>%
  group_by(country_of_origin) %>%
  summarise(meanaroma = round(mean(aroma), 1)) %>%
  right_join(coffee_total)

coffee_flavor <- coffee_ratings %>%
  group_by(country_of_origin) %>%
  summarise(meanflavour = round(mean(flavor), 1)) %>%
  right_join(coffee_total)

coffee_sweet <- coffee_ratings %>%
  group_by(country_of_origin) %>%
  summarise(meansweet = round(mean(sweetness), 1)) %>%
  right_join(coffee_total)

coffee_body <- coffee_ratings %>%
  group_by(country_of_origin) %>%
  summarise(meanbody = round(mean(body), 1)) %>%
  right_join(coffee_total)

coffee_acidity <- coffee_ratings %>%
  group_by(country_of_origin) %>%
  summarise(meanacid = round(mean(acidity), 1)) %>%
  right_join(coffee_total)

coffee_combined <- coffee_flavor %>%
  left_join(coffee_aroma, by = "country_of_origin") %>%
  left_join(coffee_sweet, by = "country_of_origin") %>%
  left_join(coffee_body, by = "country_of_origin") %>%
  left_join(coffee_acidity, by = "country_of_origin")

coffee_combined <- arrange(coffee_combined, desc(meansweet))
coffee_combined$country_of_origin <- factor(coffee_combined$country_of_origin, levels=rev(coffee_combined$country_of_origin))

ggplot(coffee_combined, aes(x=meanacid, xend=meansweet, y=country_of_origin)) + 
  #create a thick line between x and xend instead of using defaut 
  #provided by geom_dubbell
  geom_segment(aes(x=meanacid, 
                   xend=meansweet, 
                   y=country_of_origin, 
                   yend=country_of_origin), 
               color="goldenrod3", size=1.5)+
  geom_dumbbell(color="goldenrod3", 
                size_x=5, 
                size_xend = 5,
                #Note: there is no US:'color' for UK:'colour' 
                # in geom_dumbbel unlike standard geoms in ggplot()
                colour_x="tan4", 
                colour_xend = "brown4")+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "beige"), panel.background = element_rect(fill = "beige"),
        axis.text.y = element_text(size = 12, face = "bold"), plot.title = element_text(size=22, colour = "brown4"), plot.subtitle = element_text(size =16, colour = "chocolate3")) +
  labs(x=NULL, y=NULL, 
       title="Sweet or Strong?", 
       subtitle="Coffee rated by country")+
  geom_text(color="black", size=4, nudge_x = -0.25,
            aes(x=meanacid, label=meanacid))+
  geom_text(color="black", size=4, nudge_x = 0.25,
            aes(x=meansweet, label=meansweet))

coffee_count <- count(coffee_ratings, country_of_origin) %>%
  arrange(desc(n))
