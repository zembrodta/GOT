library(tidyverse)
library(readxl)
library(reshape2)

ScreenTime <- read_excel("~/Documents/GameOfThrones/ScreenTime.xlsx")
houses <- read_csv("houses.csv")

ScreenTime = left_join(ScreenTime, houses)
#Rise and fall of Characters in Screen Time 

top10 = head(ScreenTime, 10)
top10melted = melt(top10, id.var = c("character", "House", "Color", "Episodes"))

test = filter(top10melted, top10melted$character == "Daenerys Targaryen" | top10melted$character == "None")

 #Character Growth 

ggplot(top10melted, aes(variable, value ))+geom_line(aes(group = character, color = character), lwd = 2)

ggplot(top10melted, aes(variable, value))+geom_line(aes(group = character, color = character), lwd = 2)
 
#Jon 
Jon = subset(top10melted, top10melted$character == "Jon Snow")
ggplot(Jon, aes(variable, value ))+geom_line(aes(group = character), color =Jon$Color ,lwd = 2)


#Which house has the most screen time through each season 
#there are some issues with this analysis. IF the starks are in a room together, there screen time will be counted twice. 
#This weights towards the starks who are often together in the first few seasons 


HouseScreenTime = ScreenTime %>%
  group_by(House) %>%
  filter(House != "Unknown") %>% 
  summarize( season1 = sum(season1), season2 = sum(season2), season3 = sum(season3), season4 = sum(season4), season5 = sum(season5), season6 = sum(season6), season7 = sum(season7)) %>%
  mutate ( totalTime = season1 +season2 +season3+season4 +season5+season6+season7)


Season1Top = top_n(HouseScreenTime, 4, season1)
Season2Top = top_n(HouseScreenTime, 4, season2)
Season3Top = top_n(HouseScreenTime, 4, season3)
Season4Top = top_n(HouseScreenTime, 4, season4)
Season5Top = top_n(HouseScreenTime, 4, season5)
Season6Top = top_n(HouseScreenTime, 4, season6)
Season7Top = top_n(HouseScreenTime, 4, season7)

houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")

ggplot( Season1Top, aes(x= reorder(House,season1), season1, fill = House))+
  geom_bar(stat = 'identity')+coord_flip()+ 
  labs( x = "Season 1", y = "House", title = "Season 1 Screen Time")+
  scale_fill_manual(values = houses.colors)

ggplot( Season2Top, aes(x= reorder(House,season2), season2))+
  geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
  labs( x = "", y = "Screen Time (Minutes)", title = "Season 2 Screen Time")+
  scale_fill_manual(values = houses.colors)+theme_economist()+ theme(legend.position = "none")

ggplot( Season3Top, aes(x= reorder(House,season3), season3))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 3", y = "House", title = "Season 3 Screen Time")
ggplot( Season4Top, aes(x= reorder(House,season4), season4))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 4", y = "House", title = "Season 4 Screen Time")
ggplot( Season5Top, aes(x= reorder(House,season5), season5))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 5", y = "House", title = "Season 5 Screen Time")
ggplot( Season6Top, aes(x= reorder(House,season6), season6))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 6", y = "House", title = "Season 6 Screen Time")
ggplot( Season7Top, aes(x= reorder(House,season7), season7))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 7", y = "House", title = "Season 7 Screen Time")

#Character Screen Time - Make House Colors Better 


Season1TopChar = top_n(ScreenTime, 5, season1)
Season2TopChar = top_n(ScreenTime, 5, season2)
Season3TopChar = top_n(ScreenTime, 5, season3)
Season4TopChar = top_n(ScreenTime, 5, season4)
Season5TopChar = top_n(ScreenTime, 5, season5)
Season6TopChar = top_n(ScreenTime, 5, season6)
Season7TopChar = top_n(ScreenTime, 5, season7)


ggplot( Season1TopChar, aes(x= reorder(character,season1), season1))+geom_bar(stat = 'identity',aes( fill = Color))+coord_flip()+ labs( x = "Season 1", y = "character", title = "Season 1 Screen Time")
ggplot( Season2TopChar, aes(x= reorder(character,season2), season2))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 2", y = "character", title = "Season 2 Screen Time")
ggplot( Season3TopChar, aes(x= reorder(character,season3), season3))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 3", y = "character", title = "Season 3 Screen Time")
ggplot( Season4TopChar, aes(x= reorder(character,season4), season4))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 4", y = "character", title = "Season 4 Screen Time")
ggplot( Season5TopChar, aes(x= reorder(character,season5), season5))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 5", y = "character", title = "Season 5 Screen Time")
ggplot( Season6TopChar, aes(x= reorder(character,season6), season6))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 6", y = "character", title = "Season 6 Screen Time")
ggplot( Season7TopChar, aes(x= reorder(character,season7), season7))+geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+ labs( x = "Season 7", y = "character", title = "Season 7 Screen Time")





