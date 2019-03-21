output$pageStub <- renderUI( fluidPage(
  theme = shinytheme('superhero'),
  fluidRow(
    column(3,
           h2("Stark"),
           imageOutput("image1")
    ),
    column(3,
           h2("Lannister"),
           imageOutput("image2")
    ),
    column(3,
           h2("Targaryen"),
           imageOutput("image3")
    ),
    column(3, 
           h2("Tarly"),
           imageOutput("image4")
    )
  )))
  
  output$image1 = renderImage({ 
    list(src = "starkSigil.png", width = 275,
         height = 350)},
    deleteFile = FALSE)
  output$image2 = renderImage({ 
    list(src ="lannisterSigil.png", width = 275,
         height = 350)},
    deleteFile = FALSE)
  output$image3 = renderImage({ 
    list(src = "targaryenSigil.png", width = 275,
         height = 350)},
    deleteFile = FALSE)
  output$image4 = renderImage({ 
    list(src = "tarlySigil.png", width = 275,
         height = 350)},
    deleteFile = FALSE)
