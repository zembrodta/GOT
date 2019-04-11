ScreenTimeGender <- read_excel("~/Documents/GameOfThrones/ScreenTimeGender.xlsx")

STG = ScreenTimeGender %>%
  group_by(Gender)%>%
  summarize(count = n())

STAm = ScreenTimeGender %>%
  filter(Gender == "M"| Gender == "F")%>%
  group_by(Gender)%>%
  summarize(Season1 = mean(Season1), Season2 = mean(Season2), Season3 = mean(Season3), Season4 = mean(Season4), Season5 = mean(Season5), Season6 = mean(Season6), Season7 = mean(Season7), Total = mean(Total))


fSTG = ScreenTimeGender %>%
  filter(Gender == "F") %>%
  filter(dead == 1) %>%
  group_by(CauseOfDeath) %>%
  summarize( count = n())

mSTG = ScreenTimeGender %>%
  filter(Gender == "M") %>%
  filter(dead == 1) %>%
  group_by(CauseOfDeath) %>%
  summarize( count = n())

aSTG = ScreenTimeGender %>%
  filter(dead == 1) %>%
  group_by(CauseOfDeath) %>%
  summarize( count = n())


output$pageStub <- renderUI( 
  fluidPage(
    theme = shinytheme('superhero'),
    fluidRow( column ( 7, offset = 1, h2("Top 100 Characters"))
             
      ),
    fluidRow(
      column (2, offset = 1, style = 'margin-top: 3%;',
              h3("Males:    69"), 
              h3("Females:  29"), 
              h3("Other:     2")),
      
      column(8,plotOutput("Top100GenderPlot")
             ) 
      ),
    # div(style = 'margin-top: -5%;',
    # fluidRow(column ( 7, offset = 1 ,h2("Percent of Each Gender that is Deceased"))
    # )),
    # fluidRow(
    #   column(3, offset = 3,
    #          h4( "Female: "),
    #          textOutput("Fper"), 
    #          tags$style("#Fper {font-size:50px;color:white;}")
    #          
    #   ),
    #   column(3,
    #          h4( "Male:"),
    #          textOutput("Mper"), 
    #          tags$style("#Mper {font-size:50px;color:white;}")
    #   )
    # ),
    div(style = 'margin-top: -5%;',
    fluidRow(column(7, offset =1, h2("Screen Time by Gender"))
      
    )),
    fluidRow( style = 'margin-top: 5%;',
      column (2,offset = 1,
              selectInput("Season",
                          label = "Choose a Season to display the Average and Total Screentime per Gender",
                          choices = c("Total", 
                                      "Season1", 
                                      "Season2", 
                                      "Season3", 
                                      "Season4", 
                                      "Season5", 
                                      "Season6", 
                                      "Season7"), 
                          selected = "Season1")), 
      column( 4, plotOutput( "AvgScreenTime")), 
      column( 4, plotOutput( "TotalScreenTime"))
      
      ) 
    )
   
)

output$Top100GenderPlot <- renderPlot({
  v = STG$count
  names(v) = STG$Gender
  waffle::waffle(v,  rows=5, colors = c("plum4", "darkseagreen", "lightsteelblue3"))+ theme_economist()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), 
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          legend.position="bottom", 
          legend.text = element_text(size=9))
  
} , bg="transparent")


output$AvgScreenTime <- renderPlot({
  gender.colors <- c("F" = "plum4", "M" = "darkseagreen")
  STA = STA %>%
    select(Gender, var = input$Season)
  
  ggplot(STA, aes(Gender, var, fill = Gender)) +geom_bar(stat = "identity")+scale_fill_manual(values = gender.colors)+theme_economist()+
    theme(axis.title.x=element_blank(),
          axis.title.y = element_blank())
})

output$AvgScreenTime <- renderPlot({
  gender.colors <- c("F" = "plum4", "M" = "darkseagreen")
  STAm = STAm %>%
    select(Gender, var = input$Season)
  
  ggplot(STAm, aes(Gender, var, fill = Gender)) +geom_bar(stat = "identity")+scale_fill_manual(values = gender.colors)+theme_economist()+
    theme(axis.title.x=element_blank(),
          axis.title.y = element_blank(), legend.position = "none")+ labs(title = paste("Average Screen Time by Gender in ", input$Season))
  
})

output$TotalScreenTime <- renderPlot({
  gender.colors <- c("F" = "plum4", "M" = "darkseagreen")
  
  STA = ScreenTimeGender %>%
    filter(Gender == "M"| Gender == "F")%>%
    select(Gender, var = input$Season)
  
  ggplot(STA, aes(Gender, var, fill = Gender)) +geom_bar(stat = "identity")+scale_fill_manual(values = gender.colors)+theme_economist()+
    theme(axis.title.x=element_blank(),
          axis.title.y = element_blank(), legend.position = "none") + labs(title = paste("Total Screen Time by Gender in ", input$Season))
  
})

# g= aSTG$count
# names(g)= aSTG$CauseOfDeath
# waffle (g, rows = 3, colors = c("antiquewhite3", 'aquamarine4', "cadetblue4", "darkgoldenrod", "darkslategrey", "deepskyblue3", "darkseagreen", "lightpink3", "tomato", "paleturquoise1", "salmon4", "steelblue4"))

output$Fper = renderText ({
  paste(round(sum(fSTG$count)/29* 100,2), "%")
})

output$Mper = renderText ({
  paste(round(sum(mSTG$count)/69*100,2), "%")
}) 