#Allison zembrodt
#Game of thrones shiny app
#This is the first part of a game of thrones app which will discuss the characters, 
#houses, gender themes, deaths, and predictions. See outline in photos to see original ideas
#3/20/19

# Define UI for application -----
ui <- uiOutput("uiStub")

server <- function ( input, output, session ){
  output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
    # tags$audio(src = "www/MainTheme.mp3", type = "audio/mp3", autoplay = TRUE, controls =T),
    fluidPage(                                  #     move the ui into the server function
      fluidRow(
        column(12,h1("Game of Thrones")
        )),
      
      fluidRow(
        column(12,
               HTML(
                    "<h3><a href='?characters'>Characters</a> |",
                    "<a href='?houses'>Houses</a> |",
                    "<a href='?gender'>Gender</a> |",
                    "<a href='?death'>Death</a> |",
                    "</h3>")
        )
      ),
      uiOutput("pageStub")                     # loaded server code should render the
    )                                           #    rest of the page to this output$
  ))
  validFiles = c( "characters.R", "houses.R", "gender.R", "death.R")
  
  fname = isolate(session$clientData$url_search)       # isolate() deals with reactive context
  if(nchar(fname)==0) { fname = "?characters" }              # blank means home page
  fname = paste0(substr(fname, 2, nchar(fname)), ".R") # remove leading "?", add ".R"
  
  cat(paste0("Session filename: ", fname, ".\n"))      # print the URL for this session
  
  if(!fname %in% validFiles){                          # is that one of our files?
    output$pageStub <- renderUI(tagList(              # 404 if no file with that name
      fluidRow(
        column(5,
               HTML("<h2>404 Not Found Error:</h2><p>That URL doesn't exist. Use the",
                    "menu above to navigate to the page you were looking for.</p>")
        )
      )
    ))
    return()    # to prevent a "file not found" error on the next line after a 404 error
  }
  source(fname, local=TRUE)    
}
# Run the application 
shinyApp(ui = ui, server = server)

