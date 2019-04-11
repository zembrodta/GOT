#Allison zembrodt
#Game of thrones shiny app
#This is the first part of a game of thrones app which will discuss the characters, 
#houses, gender themes, deaths, and predictions. See outline in photos to see original ideas
#3/20/19

# Define UI for application -----
ui <- uiOutput("uiStub")

server <- function ( input, output, session ){
  output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
    fluidPage(                                  #     move the ui into the server function
      fluidRow(imageOutput("GOT")),
      fluidRow(style = 'margin-top:-18%;',
        column(12,
               HTML(
                    "<h3><a href='?home'>Home</a> |",
                    "<a href='?characters'>Characters</a> |",
                    "<a href='?houses'>Houses</a> |",
                    "<a href='?gender'>Gender</a> |",
                    "<a href='?death'>Death</a> |",
                    "<a href='?predictions'>Predictions </a>",
                    "</h3>")
        )),
      uiOutput("pageStub")                     # loaded server code should render the
    )                                           #    rest of the page to this output$
  ))
  validFiles = c( "home.R","characters.R", "houses.R", "gender.R", "death.R", "predictions.R")
  
  fname = isolate(session$clientData$url_search)       # isolate() deals with reactive context
  if(nchar(fname)==0) { fname = "?home" }              # blank means home page
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
  output$GOT = renderImage({ 
    img = "logo.png" 
    list(src = img, width = 400,
         height = 100)},
    deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

