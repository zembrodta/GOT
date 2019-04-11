library(tidyverse)
library(readxl)
library(reshape2)
library(plotly)

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


library(igraph)

NamedDeath <- read_excel("~/Documents/GameOfThrones/NamedDeath.xlsx")
SigDeath <- read_excel("~/Documents/GameOfThrones/SigDeath.xlsx")
SigDeath1 = SigDeath[c(5,1)]

a = data.frame(unique(SigDeath$name))
b = data.frame(unique(SigDeath$killer))
names(a)[names(a) == "unique.SigDeath.name."] = "person"
names(b)[names(b) == "unique.SigDeath.killer."] = "person"
v = rbind(a,b)
v = unique(v)
# write.csv(v,"V.csv")
#v <- unique(c(SigDeath[,1], SigDeath[,5]))


deathnet = graph_from_data_frame(d = SigDeath1, vertices = V, directed = T)
plot(deathnet, vertex.color = V$color, vertex.label.cex = 1, vertex.size=5, vertex.label.dist =1.25,edge.curve = .05, edge.arrow.size=.05, vertex.label.color="black", layout=layout_with_fr)

#Cause of Death Waffel Plot 



COD = ScreenTimeGender %>%
  group_by(CauseOfDeath)%>%
  summarize ( count = n()) %>%
  filter(count >1) %>%
  filter(!is.na(CauseOfDeath))
sum(COD$count)
c = COD$count
names(c) = COD$CauseOfDeath

waffle::waffle(c,  rows=5, colors = c("plum4", "chocolate1", "lightsteelblue3", "darkslateblue","forestgreen", "goldenrod3", "lightsalmon", "orange2", "palegreen4", "peru", "seashell4", "thistle3", "wheat4", "steelblue4", "tan2", "lavenderblush4"))+ theme_economist()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        legend.position="bottom", 
        legend.text = element_text(size=9))


COD.colors = c("plum4", "chocolate1", "lightsteelblue3", "darkslateblue","forestgreen", "goldenrod3", "lightsalmon", "orange2", "palegreen4", "peru", "seashell4", "thistle3", "wheat4", "steelblue4", "tan2", "lavenderblush4")

ggplotly(ggplot(COD, aes(reorder(CauseOfDeath, -count),count, fill = CauseOfDeath))+geom_bar(stat= "identity")+
  scale_fill_manual(values = COD.colors)+theme_economist()+ theme(legend.position="none")+ labs(x = ""))


SeasonEp = GOTKills %>%
group_by(SeasonNum, EpNum) %>%
  summarize (kills = n() )


ggplot(SeasonEp, aes(EpNum, kills))+geom_line(aes(group = SeasonNum, color =SeasonNum))

r = SeasonEp %>%
  filter( SeasonNum %in% c( 1,2,3))
library(lubridate)

GOTKillstest = GOTKills %>%
  mutate(minutes = minute(Time)) %>%
  filter(!is.na(minutes)) %>% 
  mutate( bin = case_when(minutes <10 ~ "0-10", 
                          minutes >=10 & minutes <20 ~ "10-20" , 
                          minutes>=20 & minutes <30 ~ "20-30" ,
                          minutes>=30 & minutes <40 ~ "30-40" ,
                          minutes>=40 & minutes <50 ~ "40-50" ,
                          minutes>=50 ~ "50+" 
                          )) %>%
  group_by (SeasonNum, bin)%>%
  summarize ( count = n())

ggplot(GOTKillstest, aes(bin,count))+geom_bar(stat = "identity")
# + scale_fill_manual(values = Season.colors,name = "Seasons" )


