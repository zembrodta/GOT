characters.colors <- c("Jon Snow" = "#333BFF", "Daenerys Targaryen" = "#CC6600")
df = filter(top10melted, top10melted$character == input$Character | top10melted$character == input$Character2)
ggplot(df, aes(variable, value, group = character, color = character))+
  geom_line(lwd = 2) + scale_color_manual(values = characters.colors)



top10TimePerEpisode = top10melted %>%group_by(character) %>% summarise(timePerEp = sum(value)/ mean(Episodes))

#Back burner Time per Episode vs. Audience Goodness Score)

test = left_join(top10TimePerEpisode, Goodness)

ggplot(test, aes(timePerEp,Goodness ))+geom_point()+geom_smooth(method = lm)

linTimeGood = lm(test$Goodness~ timePerEp, data = test)
summary(linTimeGood)

library(igraph)
StarkEdges = read_excel("~/Documents/GameOfThrones/StarkEdges.xlsx")
StarkNodes = read_excel("~/Documents/GameOfThrones/StarkNodes.xlsx")

net = graph_from_data_frame(d = StarkEdges, vertices = StarkNodes, directed= F)

plot(net, edge.color = StarkEdges$RelationshipColor,
     vertex.label.color = "black", vertex.color = StarkNodes$famColor,
     vertex.label = StarkNodes$id ,
     vertex.label.dist = 1, vertex.size = StarkNodes$Size *.8, 
     edge.curved = .05, edge.width = 2)
     # ,layout=layout_as_tree(net, root = c("Catelyn Stark","Eddard 'Ned' Stark", "Benjen Stark", "Brandon Stark", "Lyanna Stark")))

layout=layout_as_tree(p,root = c("Catelyn Stark"))


ggplot() + annotate("text", x = 1, y = 1.1, size=15, label = "8") + ylim( .9, 1.2)+
  annotate("text", x = 1, y = 1, size = 12, fontface = "italic", label = "Number of Seasons")+  theme_economist()+ 
  theme(axis.title.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.title.y = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.line.x.bottom = element_blank())


