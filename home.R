output$pageStub <- renderUI( fluidPage(
  theme = shinytheme('superhero'),
  #titlePanel("Game of Thrones"),
  fluidRow( column( 8, offset = 2, 
                    imageOutput("GOT"))
  ), 
  fluidRow(  h2("By the Numbers", align = "center"))
  )
  )  

output$GOT = renderImage({ 
  img = "logo.png" 
  list(src = img, width = 175,
                       height = 200)},
  deleteFile = FALSE)
