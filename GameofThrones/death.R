ScreenTimeGender <- read_excel("~/Documents/GameOfThrones/ScreenTimeGender.xlsx")

SigDeath <- read_excel("~/Documents/GameOfThrones/SigDeath.xlsx")
SigDeath1 = SigDeath[c(5,1)] 

V <- read_csv("~/Documents/GitHub/GOT/V.csv")

COD = ScreenTimeGender %>%
  group_by(CauseOfDeath)%>%
  summarize ( count = n()) %>%
  filter(count >1) %>%
  filter(!is.na(CauseOfDeath))


output$pageStub <- renderUI( 
  fluidPage(theme = shinytheme('superhero'),
    fluidRow(column(5, offset = 1,
                    h2("Directed Death Network Graph")
                    )
    ), 
    fluidRow(
      column(10,offset =1,
             plotOutput("directedDeathPlot")
      ), 
    fluidRow( column(5, offset = 1,style = 'margin-top:10%;', 
                       h2 ("Cause of Death")), 
    fluidRow( 
      column(10, offset =1, 
              plotlyOutput("causePlot"))
    ) 
      )
    )
    
  )
  
)

output$directedDeathPlot <- renderPlot({
  # make a ggplot of the screen time of character
  deathnet = graph_from_data_frame(d = SigDeath1, vertices = V, directed = T)
  plot(deathnet, vertex.label.degree = -pi/2, vertex.color = V$color, vertex.label.cex = 1.18, vertex.size=7, vertex.label.dist =1.1,edge.curve = .1, edge.arrow.size=.5, vertex.label.color="white",vertex.label.font =2, layout=layout_with_fr)
  
}, bg="transparent",height = 1000, width = 1250)

output$causePlot = renderPlotly({
  
  COD.colors = c("plum4", "chocolate1", "lightsteelblue3", "darkslateblue","forestgreen", "goldenrod3", "lightsalmon", "orange2", "palegreen4", "peru", "seashell4", "thistle3", "wheat4", "steelblue4", "tan2", "lavenderblush4")
  ggplotly(ggplot(COD, aes(reorder(CauseOfDeath, -count),count, fill = CauseOfDeath))+geom_bar(stat= "identity")+
             scale_fill_manual(values = COD.colors)+theme_economist()+ theme(legend.position="none")+ labs(x = ""))
})

# height = 700, width = 1500