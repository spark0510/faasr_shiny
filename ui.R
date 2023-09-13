ui <- fluidPage(
  fluidRow(
    column(6, titlePanel(title=span(img(height=40, width=40, src="faasr_logo.png"), "FaaSr: JSON CREATOR"))),
    column(6, align = "right",
           div(style = "height: 20px;"),
           actionButton("exit", "Exit")
    )
  ),
  fluidRow(
    column(4, wellPanel(
      selectInput("select1", "Select type:", c("FaaS Server", "Data Server", "Functions", "General Configuration")),
      br(),
      uiOutput("ui1")
    )
    ),
    column(8, wellPanel(
      fluidRow(
        column(4,
               h6(),
               fileInput("file1", "Choose JSON File",
                         width="100%",
                         multiple = FALSE,
                         accept = c("text/json",
                                    ".json")),
        ),
        column(2, 
               h5("", style = "height: 15px; visibility: hidden;"),
               actionButton("file_del","Remove")),
        column(2, br()),
        column(4, 
               h5(HTML("<b>Download JSON File</b>")),
               downloadButton("downjson", "Download JSON")
        )
      )
    ), wellPanel(
      fluidRow(
        column(12, grVizOutput("fsm_func", height="245px"),
               br())
      ),
      fluidRow(
        column(6, grVizOutput("fsm_data", height="140px")),
        column(6, grVizOutput("fsm_faas", height="140px"))
      )
    )
    )
  )
)