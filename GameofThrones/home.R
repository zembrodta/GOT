library(shinydashboard)

output$pageStub <- renderUI( fluidPage(
  
  theme = shinytheme('superhero'),
  fluidRow(
    h2("By the Numbers", align = "center")
  ),
  hr(),
  fluidRow(
    column( 4, 
            plotOutput("Seasons")), 
    column( 4, 
            plotOutput("Screen")), 
    column( 4,
            plotOutput("Death"))
  ), 
  fluidRow(style = 'margin-top:5%;',
           column( 4, 
                   plotOutput("Viewers")), 
           column( 4, 
                   plotOutput("Ep")), 
           column( 4,
                   plotOutput("Days"))
  ),
  # fluidRow( column (4, div(style = "background-color: azure3;" ,offset =3, h2( "Seasons:"))), column (4,offset =1, h2( "8")) ),
  # fluidRow( column (4,div(style = "background-color: azure3;",offset =3,  h2( "Run Time:"))), column (4,offset =1, h2( "3,810 minutes") )),
  # fluidRow( column( 8, offset = 2, h1( "Named On Screen Deaths: 206")) ),
  # fluidRow( column( 8, offset = 2, h1( "Average Number of Viewers Per Episode: 23.3 million")) ),
  # fluidRow( column( 8, offset = 2, h1( "Run Time: 3,810 minutes")))
  tags$audio(src = "MainTheme.mp3", type = "audio/mp3", autoplay = TRUE, controls =T)
  )
)

output$GOT = renderImage({ 
  img = "logo.png" 
  list(src = img, width = 700,
                       height = 300)},
  deleteFile = FALSE)
#creating the valueBoxOutput content

output$Seasons = renderPlot ({
  ggplot() + annotate("text", x = 1, y = 1.1, size=15, label = "8") + ylim( .9, 1.2)+
    annotate("text", x = 1, y = 1, size = 12, fontface = "italic", label = "Number of Seasons")+  theme_economist()+ 
    theme(axis.title.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.y = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.line.x.bottom = element_blank(), 
          panel.border = element_rect(colour = 'darkgrey', size = 4))
  
}
)

output$Screen = renderPlot ({
  ggplot() + annotate("text", x = 1, y = 1.1, size=15, label = "3,810") + ylim( .9, 1.2)+
    annotate("text", x = 1, y = 1, size = 12, fontface = "italic", label = "Run Time (minutes)")+  theme_economist()+ 
    theme(axis.title.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = 'darkgrey', size = 4),
          axis.title.y = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.line.x.bottom = element_blank())
  
}
)

output$Death = renderPlot ({
  ggplot() + annotate("text", x = 1, y = 1.1, size=15, label = "206") + ylim( .9, 1.2)+
    annotate("text", x = 1, y = 1, size = 12, fontface = "italic", label = "On Screen Named Death")+  theme_economist()+ 
    theme(axis.title.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = 'darkgrey', size = 4),
          axis.title.y = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.line.x.bottom = element_blank())
  
}
)

output$Ep = renderPlot ({
  ggplot() + annotate("text", x = 1, y = 1.1, size=15, label = "73") + ylim( .9, 1.2)+
    annotate("text", x = 1, y = 1, size = 12, fontface = "italic", label = "Episodes")+  theme_economist()+ 
    theme(axis.title.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = 'darkgrey', size = 4),
          axis.title.y = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.line.x.bottom = element_blank())
  
}
)

output$Viewers = renderPlot ({
  ggplot() + annotate("text", x = 1, y = 1.1, size=15, label = "23,300,000") + ylim( .9, 1.2)+
    annotate("text", x = 1, y = 1, size = 12, fontface = "italic", label = "Average Viewers")+  theme_economist()+ 
    theme(axis.title.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = 'darkgrey', size = 4),
          axis.title.y = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.line.x.bottom = element_blank())
  
}
)
output$Days = renderPlot ({
  ggplot() + annotate("text", x = 1, y = 1.1, size=15, label = "3") + ylim( .9, 1.2)+
    annotate("text", x = 1, y = 1, size = 12, fontface = "italic", label = "Days Til Season 8")+  theme_economist()+ 
    theme(axis.title.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = 'darkgrey', size = 4),
          axis.title.y = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.line.x.bottom = element_blank())
  
}
)


