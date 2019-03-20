#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(readxl)
library(reshape2)
library(shiny)
library(shinythemes)
library(ggthemes)

#ALLI- SOURCE THIS EVENTUALLY 
# I would like to source this and put this code in a different file
ScreenTime <- read_excel("~/Documents/GameOfThrones/ScreenTime.xlsx")
houses <- read_csv("~/Documents/GitHub/GOT/houses.csv")
ScreenTime = left_join(ScreenTime, houses)
top10 = head(ScreenTime, 10)
top10melted = melt(top10, id.var = c("character", "House", "Color", "Episodes"))
starks = c("Jon Snow", "Arya Stark", "Sansa Stark")
lannister = c("Tyrion Lannister", "Cersei Lannister", "Jaime Lannister")
top10TimePerEpisode = top10melted %>%group_by(character) %>% summarise(timePerEp = sum(value)/ mean(Episodes))

# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme('superhero'),
   
   # Application title
   titlePanel("Game of Thrones"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
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
                     choices = c("Jon Snow", 
                                 "Tyrion Lannister", 
                                 "Daenerys Targaryen", 
                                 "Sansa Stark", 
                                 "Cersei Lannister", 
                                 "Arya Stark", 
                                 "Jaime Lannister", 
                                 "Samwell Tarly", 
                                 "None"), 
                     selected = "None")
      ),

      # Show a plot of the generated distribution
      mainPanel(
         h2 ("Screen Time Per Season"),
         
         plotOutput("distPlot"),
         
         
         h2("Character 1 Information"),
         imageOutput("image1"),
         textOutput("Character1TimePerEpisode"),
         conditionalPanel(condition = "input.Character2 != 'None'",
                          p(h2("Character 2 Information"))),
         conditionalPanel(condition = "input.Character2 != 'None'",p(imageOutput("image2")))                 
      )
   )
)

# Define server logic required to draw a histogram
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
     if ( input$Character %in% lannister)
     {
       img = "~/Documents/GitHub/GOT/GameofThrones/lannisterSigil.png"
     }
     list(src = img, width = 200,
          height = 250)},
     deleteFile = FALSE)
   output$image2 = renderImage({ 
     if (input$Character2 %in% starks)
     {
       img = "~/Documents/GitHub/GOT/GameofThrones/starkSigil.png"
     }
     if ( input$Character2 %in% lannister)
     {
       img = "~/Documents/GitHub/GOT/GameofThrones/lannisterSigil.png"
     }
     list(src = img, width = 200,
          height = 250)},
     deleteFile = FALSE)
   output$Character1TimePerEpisode = renderText ({
     characterTime =top10TimePerEpisode %>% filter(character == input$Character)
     paste("The Average Screen Time Per Episode is: ", characterTime$timePerEp)
   })

}


# Run the application 
shinyApp(ui = ui, server = server)

