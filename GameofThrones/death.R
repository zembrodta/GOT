#ALLI- ADD THE EPISODE LINE GRAPH AND MAKE SO YOU CAN TOGGLE WHICH SEASONS YOU WANT TO SEE 
# ALSO ADD WHAT PARTS OF THE EPISODE SHOW THE MOST DEATH 
#STRETCH - MALE AND FEMALE 

ScreenTime <- read_excel("data/ScreenTime.xlsx")
ScreenTimeGender <- read_excel("data/ScreenTimeGender.xlsx")

SigDeath <- read_excel("data/SigDeath.xlsx")
SigDeath1 = SigDeath[c(5,1)] 

V <- read_csv("data/V.csv")

GOTKills <- read_excel("data/GOTKills.xlsx", 
                       col_types = c("text", "text", "numeric", 
                                     "numeric", "date", "skip"))

SeasonEp = GOTKills %>%
  group_by(SeasonNum, EpNum) %>%
  summarize (kills = n() )

Season.colors = c("mistyrose4", "lightsteelblue3", "navajowhite3", "paleturquoise4", "salmon3", "tan2", "darkseagreen")
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
      column(10,offset =1,style = 'margin-top:-3%;',
             plotOutput("directedDeathPlot")
      )), 
    fluidRow( column(5, offset = 1,style = 'margin-top:35%;', 
                       h2 ("Cause of Death"))), 
    fluidRow( 
      column (2,offset = 1,style = 'margin-top:3%;',
              #h4( "Select Characters"),
              selectInput("Cause",
                          label = "Choose Cause of Death",
                          choices = c("All", 
                                      "Stabbed", 
                                      "Battle", 
                                      "Explosion", 
                                      "Beheaded", 
                                      "Poisoned", 
                                      "ThroatSlit", 
                                      "Burned", 
                                      "Fell", 
                                      "Mauled", 
                                      "Hanged",
                                      "Locked Inside Vault", 
                                      "Strangled", 
                                      "Shot",
                                      "Suicide"), 
                          selected = "All")),
      column(8,
              plotlyOutput("causePlot"))
    ), 
    conditionalPanel(
      condition = "input.Cause != 'All' ",
      fluidRow(
        column(7, offset = 1,
               h2("Season Death Type Breakdown")
        )
      ),
      fluidRow(
        column(8, offset =3, 
               plotlyOutput("causeSeason"))
      )
), 
    fluidRow(column(5, offset = 1,
                    h2("Bloodiest Season (Named Deaths)")
    ), 
  fluidRow(
      column(8,offset =3,
         plotlyOutput("bloody")
      )), 
  fluidRow(
    column ( 7, offset = 1, 
             h2 ( "Bloodiest Episodes"))
  ),
  fluidRow( 
    column( 2, offset = 1, 
            checkboxGroupInput("seasons","Choose Seasons", choices=c("1","2","3","4","5","6","7"), selected = c("1","2","3","4","5","6","7"))),
            column( 8, plotlyOutput("Season"))
    
    ), 
  fluidRow(
    column ( 7, offset = 1, 
             h2 ( "Kill Time"))
  ),
  fluidRow( 
    column( 2, offset = 1, 
            selectInput("Season2",
                        label = "Choose a season to see what part of the episodes had the most deaths",
                        choices = c("All",
                                    "1", 
                                    "2", 
                                    "3", 
                                    "4", 
                                    "5", 
                                    "6", 
                                    "7"), 
                        selected = "All")
            )
    ,column( 8, plotOutput("Part"))
    
  )
      
    )
    )
    
  )
  
output$directedDeathPlot <- renderPlot({
  # make a ggplot of the screen time of character
  deathnet = graph_from_data_frame(d = SigDeath1, vertices = V, directed = T)
  plot(deathnet, vertex.label.degree = -pi/2, vertex.color = V$color, vertex.label.cex = 1, vertex.size=7, vertex.label.dist =1.1,edge.curve = .1, edge.arrow.size=.5, vertex.label.color="white",vertex.label.font =2, layout=layout_with_fr)
  
}, bg="transparent",height =975, width = 1250)

output$causePlot = renderPlotly({
  
  COD.colors = c("plum4", "chocolate1", "lightsteelblue3", "darkslateblue","forestgreen", "goldenrod3", "lightsalmon", "orange2", "palegreen4", "peru", "seashell4", "thistle3", "wheat4", "steelblue4", "tan2", "lavenderblush4")
  ggplotly(ggplot(COD, aes(reorder(CauseOfDeath, -count),count, fill = CauseOfDeath))+geom_bar(stat= "identity")+
             scale_fill_manual(values = COD.colors)+theme_economist()+ theme(legend.position="none")+ labs(x = ""))
})

output$causeSeason = renderPlotly({
  df = filter(ScreenTimeGender, CauseOfDeath == input$Cause)
  df= df%>%
    group_by(seasonDeath = as.character(seasonDeath)) %>%
    summarize(count = n())
  #use the scale_color_manual to get each character to have their own color 
  print(ggplotly(ggplot(df, aes(seasonDeath, count, fill = seasonDeath))+
                   geom_bar(stat = "identity") + theme_economist()+ scale_fill_manual(values = Season.colors)+ theme(legend.position="none")))
})

output$bloody = renderPlotly({
  df1 = GOTKills %>%
    group_by(SeasonNum =as.character(SeasonNum)) %>%
    summarize(count = n())
  print(ggplotly(ggplot(df1, aes(SeasonNum, count, fill = SeasonNum))+
                   geom_bar(stat = "identity") + theme_economist()+ scale_fill_manual(values = Season.colors)+ theme(legend.position="none")))
})
# height = 700, width = 1500

output$Season = renderPlotly({ 
  df2 = SeasonEp %>%
    filter(SeasonNum %in% input$seasons) 
  #df2$EpNum <- factor(df2$EpNum, levels = df2$EpNum[order(df2$EpNum)])
  print(ggplotly(ggplot(df2, aes(as.factor(EpNum), kills))+geom_line(aes(group = as.character(SeasonNum), color = as.character(SeasonNum)),size=2)+ theme_economist()+ scale_color_manual(values = Season.colors, name = "Seasons" )+ labs (x = "Episode Number", y = "Kills")))
  #print(ggplotly(ggplot(df2, aes(EpNum, kills))+geom_line(aes(group = SeasonNum))+ scale_color_manual(values = Season.colors)))
})        

output$Part = renderPlot({
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
  
  if (is.null(input$Season2) || is.na(input$Season2) )
  {
    # print(ggplotly(ggplot(GOTKillstest, aes(bin,count))+geom_bar(stat = "identity", fill = SeasonNum)+theme_economist()+scale_color_manual(values = Season.colors)))
    ggplot(GOTKillstest, aes(bin,count))+geom_bar(stat = "identity", fill = SeasonNum)+theme_economist()+scale_color_manual(values = Season.colors)
  }
  else if ( input$Season2 == "All")
  {
    ggplot(GOTKillstest, aes(bin,count))+geom_bar(stat = "identity")+theme_economist()
    
  }
  else {
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
      filter(SeasonNum == input$Season2) %>% 
      group_by (bin)%>%
      summarize ( count = n())
     
    ggplot(GOTKillstest, aes(bin,count))+geom_bar(stat = "identity")+theme_economist()
    # print(ggplotly(ggplot(GOTKillstest, aes(bin,count))+geom_bar(stat = "identity", fill = SeasonNum)+theme_economist()+scale_color_manual(values = Season.colors)))

  }
})


# + scale_color_manual(values = Season.colors)
