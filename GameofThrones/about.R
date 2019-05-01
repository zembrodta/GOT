output$pageStub <- renderUI( fluidPage(useShinyjs(),
      theme = shinytheme('superhero'),
    
      fluidRow(column( id = "Data", 3,  style = 'margin-top:7%; margin-left: 5%;',
               imageOutput ("data", click = "dataClick")), 
      shinyjs::hidden(column(id = "DataInfo",3,style = "margin-top:7%; margin-left: 5%;font-family: 'Lucida Console;",
                             h3("There were 4 main data sets in use for this project. Each was hand compiled using articles, fan wikis, and personal viewing of the show." ),
                             h3("Goodness Scores – compiled from a Watchers on the Wall survey that was done where they ranked the characters based on their perceived goodness."), 
                             h3("Characters Data – ‘census’ data about the characters including: house, children, gender, age, etc. There are over 40 columns of information for 70 characters!"),
                             h3("Death – information on every death in Game of Thrones. The episode, the time, the killer, the person killed, and cause of death are included."),
                             h3("Screen Time – the screen time of every character, broken down by season.")
                        )),
      column(id = "Analysis", 3, style = 'margin-top:7%;margin-left: 5%;',
                       imageOutput ("analysis", click = "analysisClick")),

      shinyjs::hidden(column(id = "AnalysisInfo",3,style = "margin-top:7%; margin-left: 5%;font-family: 'Lucida Console;",
                             h3("The data was so robust, it was hard to narrow my analysis down. In the end my analysis fell into 5 categories: Character, Houses, Gender, Death, and Predictions"),
                             h3("Who is the main character?"),
                             h3("Which house is the show truly about?"), 
                             h3("Is there truly a gender diversity problem?"),
                             h3("Death is a huge part of GOT. Are their patterns to when theses deaths occur? What are the most common causes?"),
                             h3("Perhaps the most important question: Who is going to die???"))),
                             
                             
      column(id = "Viz", 4, style = 'margin-top:7%; margin-left: 5%;',
                       imageOutput ("visualization", click = "visualizationClick")),
      
      shinyjs::hidden(column(id = "VizInfo",3,style = "margin-top:7%; margin-left: 5%;font-family: 'Lucida Console;",
                             h3("Shiny is an R package that makes it easy to build interactive web apps. It combines the computational power of R with the interactivity of the web."),
                             h3("This was the perfect way to visualize my results in a way that gives the user control to learn more about their own favorite characters."), 
                             h3("My app can be used to reactively display information about the mortality and other features of the most popular characters in the Game of Thrones universe.")))
      
      )
                                
)
)

output$data = renderImage({
  list(src = "data2.png", width = 400,height = 200)}
,deleteFile = FALSE)

output$analysis = renderImage({
  list(src = "analysis2.png", width = 400,height = 200)}
,deleteFile = FALSE)

output$visualization = renderImage({
  list(src = "visualization3.png", width = 400,height = 200)}
  ,deleteFile = FALSE)

observeEvent(input$dataClick, {
  shinyjs::hide("Data")
  shinyjs::show("DataInfo")
})

observeEvent(input$analysisClick, {
  shinyjs::hide("Analysis")
  shinyjs::show("AnalysisInfo")
})

observeEvent(input$visualizationClick, {
  shinyjs::hide("Viz")
  shinyjs::show("VizInfo")
})





