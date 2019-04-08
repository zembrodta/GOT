library(tidyverse)
library(readxl)
library(reshape2)
library(shiny)
library(shinythemes)
library(ggthemes)
library(shinyjs)
library(waffle)
library(igraph)
library(plotly)

ScreenTime <- read_excel("~/Documents/GameOfThrones/ScreenTime.xlsx")
houses <- read_csv("~/Documents/GitHub/GOT/houses.csv")
Goodness<- read_excel("~/Documents/GameOfThrones/Goodness.xlsx")
ScreenTime = left_join(ScreenTime, houses)
top10 = head(ScreenTime, 10)
top10melted = melt(top10, id.var = c("character", "House", "Color", "Episodes"))
starks = c("Jon Snow", "Arya Stark", "Sansa Stark")
lannister = c("Tyrion Lannister", "Cersei Lannister", "Jaime Lannister")
ScreenTime1 = left_join(ScreenTime, Goodness)

top10TimePerEpisode = top10melted %>%group_by(character) %>% summarise(timePerEp = sum(value)/ mean(Episodes))

output$pageStub <- renderUI( fluidPage(
  theme = shinytheme('superhero'),
  #titlePanel("Game of Thrones"),
  fluidRow(
    column (3,offset = 1,style = 'margin-top:3%;',
            #h4( "Select Characters"),
            selectInput("Character",
                        label = "Choose Character to display their Screen Time",
                        choices = c("Jon Snow", 
                                    "Tyrion Lannister", 
                                    "Daenerys Targaryen", 
                                    "Sansa Stark", 
                                    "Cersei Lannister", 
                                    "Arya Stark", 
                                    "Jaime Lannister", 
                                    "Samwell Tarly"), 
                        selected = "Jon Snow"),
            selectInput("Character2",
                        label = "Choose Character to display their Screen Time",
                        choices = c("None",
                                    "Jon Snow", 
                                    "Tyrion Lannister", 
                                    "Daenerys Targaryen", 
                                    "Sansa Stark", 
                                    "Cersei Lannister", 
                                    "Arya Stark", 
                                    "Jaime Lannister", 
                                    "Samwell Tarly" 
                        ), 
                        selected = "None")
          ), 
    column(6, offset = .75,
           h2("Screen Time Per Season"),
           plotlyOutput("distPlot")
          )        
    
  ),
  div(style = 'margin-top: -10%;',
      fluidRow(
        column(7, offset = 1,
               h2("Character 1 Information")
        )
      )
  ),
  div(style = 'margin-top: 0%;',
      fluidRow(
        column(2, offset =1,
               h4("House Sigil:"),
               imageOutput("image1")
        ), 
        
        column(3,
               h4( "The Average Screen Time Per Episode"),
               textOutput("Character1TimePerEpisode"), 
               tags$style("#Character1TimePerEpisode {font-size:50px;color:white;}")
        ),
        
        column(3, 
               h4( "Goodness Score:"),
               textOutput("Character1Good"), 
               tags$style("#Character1Good {font-size:50px;color:white;}")
        ), 
        column(3,
               h4( "Kill Count:")
               #textOutput("Character1KillCount"),
               #tags$style("#Character1KillCount {font-size:50px;color:white;}")
        )
        
        
      )),
  
  conditionalPanel(
    condition = "input.Character2 != 'None' ",
    style = 'margin-top: -10%;',
    fluidRow(
      column(7, offset = 1,
             h2("Character 2 Information")
      )
    ),
    fluidRow(
      column(2, offset=1,
             h4("House Sigil:"),
             imageOutput("image2")
      ),
      column(3,
             h4( "The Average Screen Time Per Episode"),
             textOutput("Character2TimePerEpisode"),
             tags$style("#Character2TimePerEpisode {font-size:50px;color:white;}")
      ),
      
      column(3,
             h4( "Goodness Score:"),
             textOutput("Character2Good"),
             tags$style("#Character2Good {font-size:50px;color:white;}")
      ),
      column(3,
             h4( "Kill Count:")
             #textOutput("Character1KillCount"),
             #tags$style("#Character1KillCount {font-size:50px;color:white;}")
      )
    )  
  )
)
)

output$distPlot <- renderPlotly({
  # make a ggplot of the screen time of character
  characters.colors <- c("Jon Snow" = "grey31", "Daenerys Targaryen" = "firebrick", "Tyrion Lannister" = "peru", "Sansa Stark" = "plum4", "Cersei Lannister" = "darkgoldenrod1", "Arya Stark" = "chartreuse4", "Jaime Lannister" = "navyblue", "Samwell Tarly"= "peachpuff2")
  df = filter(top10melted, top10melted$character == input$Character | top10melted$character == input$Character2)
  #use the scale_color_manual to get each character to have their own color 
  print(ggplotly(ggplot(df, aes(variable, value, group = character, color = character))+
    geom_line(lwd = 2) + scale_color_manual(values = characters.colors) + theme_economist()
))
})


output$image1 = renderImage({ 
  if (input$Character %in% starks)
  {
    img = "starkSigil.png"
  }
  else if ( input$Character %in% lannister)
  {
    img = "lannisterSigil.png"
  }
  else if ( input$Character == "Daenerys Targaryen")
  {
    img = "targaryenSigil.png"
  }
  else {
    img = "tarlySigil.png"
  }
  list(src = img, width = 175,
       height = 200)},
  deleteFile = FALSE)


output$image2 = renderImage({ 
  if (input$Character2 %in% starks)
  {
    img = "starkSigil.png"
  }
  else if ( input$Character2 %in% lannister)
  {
    img = "lannisterSigil.png"
  }
  else if ( input$Character2 == "Daenerys Targaryen")
  {
    img = "targaryenSigil.png"
  }
  else {
    img = "tarlySigil.png"
  }
  list(src = img, width = 175,
       height = 200)},
  deleteFile = FALSE)


output$Character1TimePerEpisode = renderText ({
  characterTime =top10TimePerEpisode %>% filter(character == input$Character)
  paste(round(characterTime$timePerEp,2))
})

output$Character2TimePerEpisode = renderText ({
  characterTime2 =top10TimePerEpisode %>% filter(character == input$Character2)
  paste(round(characterTime2$timePerEp,2))
})

output$Character1Good = renderText ({
  characterGood =ScreenTime1 %>% filter(character == input$Character) %>%summarize(Goodness = mean(Goodness))
  paste(round(characterGood$Goodness,2))
})

output$Character2Good = renderText ({
  character2Good =ScreenTime1 %>% filter(character == input$Character2) %>%summarize(Goodness = mean(Goodness))
  paste(round(character2Good$Goodness,2))
}) 