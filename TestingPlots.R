library(tidyverse)
library(plotly)
library(ggthemes)

GOTKiller = GOTKills %>%
  group_by(Killer) %>%
  summarize( kills = n() )
  
GoodKills = left_join(Goodness,GOTKiller ,by = c( "character"= "Killer"))
GoodKills = inner_join(GoodKills, ScreenTimeGender)
GoodKills[is.na(GoodKills)] <- 0

GoodKills = GoodKills %>%
  mutate( EpTime = Total / Episodes)


houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Greyjoy" = "grey31", "Baratheon" = "chocolate4", "Tyrell" = "darkolivegreen3", "Clegane" = "steelblue", "Free Folk" = "coral2", "Bolton"= "darkred", "Mormont" = "darkslategrey", "Seaworth" = "darkseagreen2", "Tarly"= "yellow", "Unknown" = "pink", "Baelish" = "Blue")
ggplotly(ggplot(GoodKills, aes( Goodness, EpTime,color = House, text = character))+geom_point(size = 4)+theme_economist()+scale_color_manual(values = houses.colors))

ggplotly(ggplot(GoodKills, aes( Goodness, EpTime,color = Gender, text = character))+
           geom_point(size = 4)+
           theme_economist())
