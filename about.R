output$pageStub <- renderUI( fluidPage(useShinyjs(),
      theme = shinytheme('superhero'),
      fluidRow(column( 3, offset = 1,style = 'margin-top:15%;text-align:center; vertical-align: middle;',
               imageOutput ("data", click = "dataClick"))), 
      fluidRow(column( 3, style = 'margin-top:15%;text-align:center; vertical-align: middle;',
                       imageOutput ("analysis", click = "analysisClick"))), 
      fluidRow(column( 3, style = 'margin-top:15%;text-align:center; vertical-align: middle;',
                       imageOutput ("visualization", click = "visualizationClick")))
                                
)
)

output$data = renderImage({
  list(src = "data.png", width = 300,height = 100)}
,deleteFile = FALSE)

output$data = renderImage({
  list(src = "analysis.png", width = 300,height = 100)}
,deleteFile = FALSE)

output$visualization = renderImage({
  list(src = "visualization.png", width = 300,height = 100)}
  ,deleteFile = FALSE)