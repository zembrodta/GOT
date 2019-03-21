#Allison zembrodt
#Game of thrones shiny app
#This is the first part of a game of thrones app which will discuss the characters, 
#houses, gender themes, deaths, and predictions. See outline in photos to see original ideas
#3/20/19

#Packages needed------
library(tidyverse)
library(readxl)
library(reshape2)
library(shiny)
library(shinythemes)
library(ggthemes)

#ALLI- SOURCE THIS EVENTUALLY -----
# I would like to source this and put this code in a different file
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

# Define UI for application -----
ui <- fluidPage(
   theme = shinytheme('superhero'),
   
   # Application title
   titlePanel("Game of Thrones"),
   
   # Sidebar with a slider input for number of bins 
      # Show a plot of the generated distribution
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
                   plotOutput("distPlot")
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


#Server Function -----
server <- function(input, output) {
   output$distPlot <- renderPlot({
      # make a ggplot of the screen time of character
     characters.colors <- c("Jon Snow" = "grey31", "Daenerys Targaryen" = "firebrick", "Tyrion Lannister" = "peru", "Sansa Stark" = "plum4", "Cersei Lannister" = "darkgoldenrod1", "Arya Stark" = "chartreuse4", "Jaime Lannister" = "navyblue", "Samwell Tarly"= "peachpuff2")
     df = filter(top10melted, top10melted$character == input$Character | top10melted$character == input$Character2)
     #use the scale_color_manual to get each character to have their own color 
     ggplot(df, aes(variable, value, group = character, color = character))+
       geom_line(lwd = 2) + scale_color_manual(values = characters.colors) + theme_economist()
   })
   
   
   output$image1 = renderImage({ 
     if (input$Character %in% starks)
     {
       img = "~/Documents/GitHub/GOT/GameofThrones/starkSigil.png"
     }
     else if ( input$Character %in% lannister)
     {
       img = "~/Documents/GitHub/GOT/GameofThrones/lannisterSigil.png"
     }
     else if ( input$Character == "Daenerys Targaryen")
     {
       img = "~/Documents/GitHub/GOT/GameofThrones/targaryenSigil.png"
     }
     else {
       img = "~/Documents/GitHub/GOT/GameofThrones/tarlySigil.png"
     }
     list(src = img, width = 175,
          height = 200)},
     deleteFile = FALSE)
   
   
   output$image2 = renderImage({ 
     if (input$Character2 %in% starks)
     {
       img = "~/Documents/GitHub/GOT/GameofThrones/starkSigil.png"
     }
     else if ( input$Character2 %in% lannister)
     {
       img = "~/Documents/GitHub/GOT/GameofThrones/lannisterSigil.png"
     }
     else if ( input$Character2 == "Daenerys Targaryen")
     {
       img = "~/Documents/GitHub/GOT/GameofThrones/targaryenSigil.png"
     }
     else {
       img = "~/Documents/GitHub/GOT/GameofThrones/tarlySigil.png"
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
   
}


# Run the application 
shinyApp(ui = ui, server = server)

