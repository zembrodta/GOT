
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

houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Greyjoy" = "grey31", "Baratheon" = "chocolate4", "Tyrell" = "darkolivegreen3", "Clegane" = "steelblue")
Starkhouses.colors <- c("Stark" = "plum4", "Targaryen" = "azure3", "Lannister" = "azure3", "Greyjoy" = "azure3", "Baratheon" = "azure3", "Tyrell" = "azure3", "Clegane" = "azure3", "Free Folk" = "azure3")
Lanhouses.colors <- c("Stark" = "azure3", "Targaryen" = "azure3", "Lannister" = "peru", "Greyjoy" = "azure3", "Baratheon" = "azure3", "Tyrell" = "azure3", "Clegane" = "azure3", "Free Folk" = "azure3")
Targhouses.colors <- c("Stark" = "azure3", "Targaryen" = "firebrick", "Lannister" = "azure3", "Greyjoy" = "azure3", "Baratheon" = "azure3", "Tyrell" = "azure3", "Clegane" = "azure3", "Free Folk" = "azure3")
Greyjoyhouses.colors <- c("Stark" = "azure3", "Targaryen" = "azure3", "Lannister" = "azure3", "Greyjoy" = "grey31", "Baratheon" = "azure3", "Tyrell" = "azure3", "Clegane" = "azure3", "Free Folk" = "azure3")


output$pageStub <- renderUI( 
  fluidPage(useShinyjs(),
  theme = shinytheme('superhero'),
  
  fluidRow(
    column(3,
           h2("Stark"),
           imageOutput("image1", click = "image_click")
    ),
    column(3,
           h2("Lannister"),
           imageOutput("image2", click = "lan_click")
    ),
    column(3,
           h2("Targaryen"),
           imageOutput("image3", click = "targ_click")
    ),
    column(3, 
           h2("Greyjoy"),
           imageOutput("image4")
    )),
  fluidRow(
    column(7,
           h2("Screen Time Per Season")
    )),
  
  # mainPanel( id = "mainPlots",
             fluidRow( id = "mainPlots",
               column(4,
                      plotOutput("Season1")
               ),
               column(4,
                      plotOutput("Season2")
               ),
               column(4,
                      plotOutput("Season3")
               ),
               column(4,
                       plotOutput("Season4")
               ), 
             column(4,
                    plotOutput("Season5")
             ), 
             column(4,
                    plotOutput("Season6")
             ),
             column(4,
                    plotOutput("Season7")
  )
  ),
  
  shinyjs::hidden(
    div( id = "Stark",
    fluidRow(
      column(4,
             plotOutput("StarkSeason1")
      ),
      column(4,
             plotOutput("StarkSeason2")
      ),
      column(4,
             plotOutput("StarkSeason3")
      ),
      column(4,
             plotOutput("StarkSeason4")
      ),
      column(4,
             plotOutput("StarkSeason5")
      ),
      column(4,
             plotOutput("StarkSeason6")
      ),
      column(4,
             plotOutput("StarkSeason7")
      )
  ))), 
  shinyjs::hidden(
    div( id = "Lannister",
         fluidRow(
           column(4,
                  plotOutput("LanSeason1")
           ),
           column(4,
                  plotOutput("LanSeason2")
           ),
           column(4,
                  plotOutput("LanSeason3")
           ),
           column(4,
                  plotOutput("LanSeason4")
           ),
           column(4,
                  plotOutput("LanSeason5")
           ),
           column(4,
                  plotOutput("LanSeason6")
           ),
           column(4,
                  plotOutput("LanSeason7")
           )
         ))), 
  shinyjs::hidden(
    div( id = "Targ",
         fluidRow(
           column(4,
                  plotOutput("TargSeason1")
           ),
           column(4,
                  plotOutput("TargSeason2")
           ),
           column(4,
                  plotOutput("TargSeason3")
           ),
           column(4,
                  plotOutput("TargSeason4")
           ),
           column(4,
                  plotOutput("TargSeason5")
           ),
           column(4,
                  plotOutput("TargSeason6")
           ),
           column(4,
                  plotOutput("TargSeason7")
           )
         )))
  
 ))
  
  output$image1 = renderImage({ 
    list(src = "starkSigil.png", width = 275,
         height = 350, house = "Stark")},
    deleteFile = FALSE)
  output$image2 = renderImage({ 
    list(src ="lannisterSigil.png", width = 275,
         height = 350, hopuse = "Lannister")},
    deleteFile = FALSE)
  output$image3 = renderImage({ 
    list(src = "targaryenSigil.png", width = 275,
         height = 350, house = "Targaryen")},
    deleteFile = FALSE)
  output$image4 = renderImage({ 
    list(src = "greyjoySigil.png", width = 275,
         height = 350,house = "GreyJoy" )},
    deleteFile = FALSE)
  
  output$Season1 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Greyjoy" = "grey31", "Baratheon" = "chocolate4", "Tyrell" = "darkolivegreen3", "Clegane" = "steelblue")
    ggplot( Season1Top, aes(x= reorder(House,season1), season1))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 1 Screen Time")+
      scale_fill_manual(values = houses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$Season2 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season2Top, aes(x= reorder(House,season2), season2))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 2 Screen Time")+
      scale_fill_manual(values = houses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$Season3 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season3Top, aes(x= reorder(House,season3), season3))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 3 Screen Time")+
      scale_fill_manual(values = houses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  
  output$Season4 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season4Top, aes(x= reorder(House,season4), season4))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 4 Screen Time")+
      scale_fill_manual(values = houses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$Season5 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Greyjoy" = "grey31", "Baratheon" = "chocolate4", "Tyrell" = "darkolivegreen3", "Clegane" = "steelblue")
    ggplot( Season5Top, aes(x= reorder(House,season5), season5))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 5 Screen Time")+
      scale_fill_manual(values = houses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$Season6 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season6Top, aes(x= reorder(House,season6), season6))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 6 Screen Time")+
      scale_fill_manual(values = houses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$Season7 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season7Top, aes(x= reorder(House,season7), season7))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 7 Screen Time")+
      scale_fill_manual(values = houses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  
  
  #Stark Plots -----
  output$StarkSeason1 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    
    ggplot( Season1Top, aes(x= reorder(House,season1), season1))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 1 Screen Time")+
      scale_fill_manual(values = Starkhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$StarkSeason2 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season2Top, aes(x= reorder(House,season2), season2))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 2 Screen Time")+
      scale_fill_manual(values = Starkhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$StarkSeason3 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season3Top, aes(x= reorder(House,season3), season3))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 3 Screen Time")+
      scale_fill_manual(values = Starkhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  
  output$StarkSeason4 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season4Top, aes(x= reorder(House,season4), season4))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 4 Screen Time")+
      scale_fill_manual(values = Starkhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$StarkSeason5 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Greyjoy" = "grey31", "Baratheon" = "chocolate4", "Tyrell" = "darkolivegreen3", "Clegane" = "steelblue")
    ggplot( Season5Top, aes(x= reorder(House,season5), season5))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 5 Screen Time")+
      scale_fill_manual(values = Starkhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$StarkSeason6 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season6Top, aes(x= reorder(House,season6), season6))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 6 Screen Time")+
      scale_fill_manual(values = Starkhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$StarkSeason7 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season7Top, aes(x= reorder(House,season7), season7))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 7 Screen Time")+
      scale_fill_manual(values = Starkhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })

  
  #Lan Plots -----
  output$LanSeason1 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    
    ggplot( Season1Top, aes(x= reorder(House,season1), season1))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 1 Screen Time")+
      scale_fill_manual(values = Lanhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$LanSeason2 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season2Top, aes(x= reorder(House,season2), season2))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 2 Screen Time")+
      scale_fill_manual(values = Lanhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$LanSeason3 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season3Top, aes(x= reorder(House,season3), season3))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 3 Screen Time")+
      scale_fill_manual(values = Lanhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  
  output$LanSeason4 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Lannister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season4Top, aes(x= reorder(House,season4), season4))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 4 Screen Time")+
      scale_fill_manual(values = Lanhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$LanSeason5 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    ggplot( Season5Top, aes(x= reorder(House,season5), season5))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 5 Screen Time")+
      scale_fill_manual(values = Lanhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$LanSeason6 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    ggplot( Season6Top, aes(x= reorder(House,season6), season6))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 6 Screen Time")+
      scale_fill_manual(values = Lanhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$LanSeason7 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    ggplot( Season7Top, aes(x= reorder(House,season7), season7))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 7 Screen Time")+
      scale_fill_manual(values = Lanhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  
  
  #Targ Plots -----
  
  output$TargSeason1 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    
    ggplot( Season1Top, aes(x= reorder(House,season1), season1))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 1 Screen Time")+
      scale_fill_manual(values = Targhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$TargSeason2 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Targnister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season2Top, aes(x= reorder(House,season2), season2))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 2 Screen Time")+
      scale_fill_manual(values = Targhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$TargSeason3 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Targnister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season3Top, aes(x= reorder(House,season3), season3))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 3 Screen Time")+
      scale_fill_manual(values = Targhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  
  output$TargSeason4 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    #houses.colors <- c("Stark" = "plum4", "Targaryen" = "firebrick", "Targnister" = "peru", "Sansa Stark" = "plum4", "Greyjoy" = "grey31", "Baratheon" = "chocolate4")
    ggplot( Season4Top, aes(x= reorder(House,season4), season4))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 4 Screen Time")+
      scale_fill_manual(values = Targhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$TargSeason5 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    ggplot( Season5Top, aes(x= reorder(House,season5), season5))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 5 Screen Time")+
      scale_fill_manual(values = Targhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$TargSeason6 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    ggplot( Season6Top, aes(x= reorder(House,season6), season6))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 6 Screen Time")+
      scale_fill_manual(values = Targhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  output$TargSeason7 <- renderPlot({
    #use the scale_color_manual to get each house to have their own color 
    ggplot( Season7Top, aes(x= reorder(House,season7), season7))+
      geom_bar(stat = 'identity',aes(fill = House))+coord_flip()+
      labs( x = "", y = "Screen Time (Minutes)", title = "Season 7 Screen Time")+
      scale_fill_manual(values = Targhouses.colors)+theme_economist()+ theme(legend.position = "none")
  })
  
  
 #Observe Events ----- 
  observeEvent(input$image_click, {

    shinyjs::show("Stark")
    shinyjs::hide("Lannister")
    shinyjs::hide("mainPlots")
    shinyjs::hide("Targ")
  })
  observeEvent(input$lan_click, {
    shinyjs::show("Lannister")
    shinyjs::hide("mainPlots")
    shinyjs::hide("Stark")
    shinyjs::hide("Targ")
  })
  
  observeEvent(input$targ_click, {
    shinyjs::show("Targ")
    shinyjs::hide("mainPlots")
    shinyjs::hide("Stark")
    shinyjs::hide("Lannister")
  })
  
  
  