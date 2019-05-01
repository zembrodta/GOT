output$pageStub <- renderUI( 
  fluidPage(useShinyjs(),theme = shinytheme('superhero'),
            fluidRow( column( 7, offset = 1, h2("WHO IS MOST LIKELY TO LIVE? ")
                              )
              ), 
            fluidRow ( id = "greyBox", align = "center",imageOutput("grey", click = "grey_click")
                      ),
            
            shinyjs::hidden(
              div( id = "ruler",
                   fluidRow( align = "center", imageOutput("brienne")
                     ), 
                   fluidRow ( align = "center ",style = 'margin-top: -5%', h3("Brienne of Tarth!")))), 
            fluidRow( column( 7, offset = 1, h2("WHO IS MOST LIKELY TO DIE?"))),
            fluidRow ( id = "greyBox2", align = "center", imageOutput("grey2", click = "grey_click2")),
            shinyjs::hidden(
              div( id = "dead",
                fluidRow( align = "center", imageOutput("sansa")),
                fluidRow ( align = "center ",style = 'margin-top: -5%', h3("Sansa Stark :(")))), 
            fluidRow(column(3, offset = 1,h2( "Most Likely to Die:")), column(3,offset = 1 ,h2( "50/50 Chance:")), column(3,offset =1,h2( "Likely to Live:"))), 
            fluidRow( style = 'margin-bottom: 10%', column( 3,offset = 1, h4 ( "1. Sansa Stark"), h4("2. Gilly"), h4("3. Arya Stark"), h4("4. Tormund Giantsbane"), h4("5. Cersei Lannister"), h4("6. Gendry")), 
                      column(3, offset = 1, h4("1. Bran Stark"), h4("2. Samwell Tarly"), h4("3. Tyrion Lannister"), h4("4. Lord Varys"), h4("5. Jon Snow"), h4("6. Sir Davos")), 
                      column ( 3, offset = 1,h4("1. Brienne of Tarth"), h4("2. Jamie Lannister"),h4("3. Jorah Mormont"), h4( "4. Theon Greyjoy"), h4("5. Daenerys Targaryen"), h4("6. Sandor 'The Hound' Clegane"))) , 
            fluidRow( column( 7, offset = 1, tags$em("My model is only based on what I call 'census' data, i.e hometown, occupation, number of children, etc.")))
            
  ))  
  


output$grey = renderImage({ 
  list(src = "click.png", width = 600,
       height = 300)},
  deleteFile = FALSE)

output$grey2 = renderImage({ 
  list(src = "click.png", width = 600,
       height = 300)},
  deleteFile = FALSE)  

output$brienne = renderImage({ 
  list(src = "brienne.jpg", width = 600,
       height = 300)},
  deleteFile = FALSE)   

output$sansa = renderImage({ 
  list(src = "sansa2.jpg", width = 600,
       height = 300)},
  deleteFile = FALSE)   
              
observeEvent(input$grey_click, {
  shinyjs::show("ruler")
  shinyjs::hide("greyBox")
})

observeEvent(input$grey_click2, {
  shinyjs::show("dead")
  shinyjs::hide("greyBox2")
})              
              
              
              
              