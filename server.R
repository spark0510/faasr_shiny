library("shiny")
library("DiagrammeR")

server <- function(input, output) {
  json_data <- reactiveVal(NULL)  
  
  observe({
    if (!is.null(input$file1)) {
      json_path <- input$file1$datapath
      json <- readLines(json_path)
      json_data(json)
    }
  })
  
  observeEvent(input$file_del, {
    json_data(NULL)
  })
  
  observeEvent(input$exit, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    stopApp(json)
  })
  
  output$ui1 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    exampleid <- uuid::UUIDgenerate()
    if (is.null(json$InvocationID)){
      json$InvocationID <- exampleid
    }
    
    if (is.null(input$select1))
      return()
    
    switch (input$select1,
            "Functions" = list(
              textInput("func_name", "Function Name:", placeholder= "Initial_function_name"),
              uiOutput("ui5")
            ),
            "Data Server" = list(
              textInput("data_name", "Data Server Name:", placeholder = "Initial_data_server_name"),
              uiOutput("ui3")
            ),
            "FaaS Server" = list(
              textInput("faas_name", "FaaS Server Name:", placeholder = "Initial_faas_server_name"),
              uiOutput("ui4")
            ),
            "General Configuration" = list(
              selectInput("function_invoke", "First Function to be executed:", names(json$FunctionList), selected=json$FunctionInvoke),
              selectInput("logging_server", "Logging Server to leave logs:", names(json$DataStores), selected=json$LoggingServer),
              textInput("faasr_log", "Log file name(Optional, default=FaaSr):", value = json$FaaSrLog, placeholder = "FaaSr"),
              textInput("invocation_id", "InvocationID(Optional, must follow UUID format):", value = json$InvocationID, placeholder = exampleid),
              fluidRow(
                column(6,
                       actionButton("gen_apply", label = "Apply")),
                column(6,
                       div(style = "position:absolute;right:1em;", 
                           actionButton("gen_delete", label = "Delete")))
              )
            )
    )
  })
  
  output$ui5 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    return(
      list(
        textInput("func_act", "Function Action Name:", value= json$FunctionList[[input$func_name]]$Actionname, placeholder = "F1_Action_1"),
        uiOutput("ui2")
      )
    )
  })
  
  
  output$ui2 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    return(
      list(
        selectInput("func_faas", "Function FaaS Server:", names(json$ComputeServers), selected = json$FunctionList[[input$func_name]]$FaaSServer),
        textAreaInput("func_args", "Function Arguments:", value = unretrieve(json$FunctionList[[input$func_name]]$Arguments), placeholder = "arg1=input1.csv,\narg2:input2.csv", height = "100px", resize = "vertical"),
        textInput("func_next", "Function Next Invoke:", value = unretrieve(json$FunctionList[[input$func_name]]$InvokeNext), placeholder = "F2, F3"),
        textInput("func_container", "Function's Action Container(Optional):", value= json$ActionContainers[[input$func_act]], placeholder = "faasr/github-actions-tidyverse"),
        textInput("func_gh_repo", "Repository/Path, where the function is stored:", value = unretrieve(json$FunctionGitRepo[[input$func_name]]), placeholder = "username/reponame, https://url.git"),
        textInput("func_gh_package", "Dependencies - Github Package for the function:", value = unretrieve(json$FunctionGitHubPackage[[input$func_name]]), placeholder = "username/package_reponame"),
        textInput("func_cran_repo", "Dependencies - Repository/Path for the function:", value = unretrieve(json$FunctionCRANPackage[[input$func_name]]), placeholder = "CRAN_package_name, dplyr"),
        fluidRow(
          column(6,
                 actionButton("func_apply", label = "Apply")),
          column(6,
                 div(style = "position:absolute;right:1em;", 
                     actionButton("func_delete", label = "Delete")))
        )
      )
    )
  })
  
  
  output$ui3 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    return(
      list(
        textInput("data_endpoint", "Data Server Endpoint(optional for s3):", value = json$DataStores[[input$data_name]]$Endpoint, placeholder = "https://play.min.io"),
        textInput("data_bucket", "Data Server Bucket:", value = json$DataStores[[input$data_name]]$Bucket, placeholder = "my_bucket"),
        textInput("data_region", "Data Server Region (optional for minio):", value = json$DataStores[[input$data_name]]$Region, placeholder = "us-east-1"),
        textInput("data_writable", "Data Server Writable permission:", value = json$DataStores[[input$data_name]]$Writable, placeholder = "TRUE"),
        fluidRow(
          column(6,
                 actionButton("data_apply", label = "Apply")),
          column(6,
                 div(style = "position:absolute;right:1em;", 
                     actionButton("data_delete", label = "Delete")))
        )
      )
    )
  })
  
  output$ui4 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    return(
      list(
        selectInput("faas_type", "Select type:", c("GitHubActions", "OpenWhisk", "Lambda"), selected = json$ComputeServers[[input$faas_name]]$FaaSType),
        conditionalPanel(
          condition = "input.faas_type == 'GitHubActions'",
          textInput("faas_gh_user", "GitHubActions User name:", value = json$ComputeServers[[input$faas_name]]$UserName, placeholder = "user_name"),
          textInput("faas_gh_repo", "GitHubActions Action Repository name:", value = json$ComputeServers[[input$faas_name]]$ActionRepoName, placeholder = "repo_name"),
          textInput("faas_gh_ref", "GitHubActions Branch/Ref(Optional, default=main):", value = json$ComputeServers[[input$faas_name]]$Ref, placeholder = "main")
        ),
        conditionalPanel(
          condition = "input.faas_type == 'Lambda'",
          textInput("faas_ld_region", "Lambda Region:", value = json$ComputeServers[[input$faas_name]]$Region, placeholder = "us-east-1")
        ),
        conditionalPanel(
          condition = "input.faas_type == 'OpenWhisk'",
          textInput("faas_ow_end", "OpenWhisk Endpoint(Optional, default=IBMcloud):", value = json$ComputeServers[[input$faas_name]]$Endpoint, placeholder = "https://00.00.00.00"),
          textInput("faas_ow_name", "OpenWhisk Namespace:", value = json$ComputeServers[[input$faas_name]]$Namespace, placeholder = "namespace_name"),
          textInput("faas_ow_region", "OpenWhisk Region:", value = json$ComputeServers[[input$faas_name]]$Region, placeholder = "us-west")
        ),
        fluidRow(
          column(6,
                 actionButton("faas_apply", label = "Apply")),
          column(6,
                 div(style = "position:absolute;right:1em;", 
                     actionButton("faas_delete", label = "Delete")))
        )
      )
    )
  })
  
  observeEvent(input$func_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else {
      json <- list()
    }
    func_name <- input$func_name
    if (is.null(func_name)){
      return()
    }
    json$FunctionList[[func_name]] <- NULL
    json$FunctionCRANPackage[[func_name]] <- NULL
    json$FunctionGitHubPackage[[func_name]] <-NULL
    json$FunctionGitRepo[[func_name]] <- NULL
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
  })
  
  observeEvent(input$faas_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else {
      json <- list()
    }
    faas_name <- input$faas_name
    if (is.null(faas_name)){
      return()
    }
    json$ComputeServers[[faas_name]] <- NULL
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
  })
  
  observeEvent(input$data_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else {
      json <- list()
    }
    data_name <- input$data_name
    if (is.null(data_name)){
      return()
    }
    json$DataStores[[data_name]] <- NULL
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
  })
  
  observeEvent(input$gen_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    json$FunctionInvoke <- NULL
    json$InvocationID <- NULL
    json$FaaSrLog <- NULL
    json$LoggingServer <- NULL
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
    
  })
  
  observeEvent(input$func_apply, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    func_name <- input$func_name
    if (is.null(func_name)){
      return()
    }
    json$FunctionList[[func_name]]$Actionname <- input$func_act
    json$FunctionList[[func_name]]$FaaSServer <- input$func_faas
    json$FunctionList[[func_name]]$Arguments <- retrieve(input$func_args)
    json$FunctionList[[func_name]]$InvokeNext <- retrieve(input$func_next)
    json$ActionContainers[[input$func_act]] <- input$func_container
    json$FunctionCRANPackage[[func_name]] <- retrieve(input$func_cran_repo)
    json$FunctionGitHubPackage[[func_name]] <-retrieve(input$func_gh_package)
    json$FunctionGitRepo[[func_name]] <- retrieve(input$func_gh_repo)
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
  })
  
  observeEvent(input$faas_apply, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    faas_name <- input$faas_name
    if (is.null(faas_name)){
      return()
    }
    json$ComputeServers[[faas_name]]$FaaSType <- input$faas_type
    switch(json$ComputeServers[[faas_name]]$FaaSType,
           "GitHubActions"={
             json$ComputeServers[[faas_name]]$UserName <- input$faas_gh_user
             json$ComputeServers[[faas_name]]$ActionRepoName <- input$faas_gh_repo
             json$ComputeServers[[faas_name]]$Ref <- input$faas_gh_ref
           },
           "Lambda"={
             json$ComputeServers[[faas_name]]$Region <- input$faas_ld_region
           },
           "OpenWhisk"={
             json$ComputeServers[[faas_name]]$Endpoint <- input$faas_ow_end
             json$ComputeServers[[faas_name]]$Namespace <- input$faas_ow_name
             json$ComputeServers[[faas_name]]$Region <- input$faas_ow_region
           }
    )
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    
    json_data(json_pretty)
  })
  
  observeEvent(input$data_apply, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    data_name <- input$data_name
    if (is.null(data_name)){
      return()
    }
    json$DataStores[[data_name]]$Endpoint <- input$data_endpoint
    json$DataStores[[data_name]]$Bucket <- input$data_bucket
    json$DataStores[[data_name]]$Region <- input$data_region
    json$DataStores[[data_name]]$Writable <- input$data_writable
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    
    json_data(json_pretty)
  })
  
  observeEvent(input$gen_apply, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    json$FunctionInvoke <- input$function_invoke
    json$InvocationID <- input$invocation_id
    json$FaaSrLog <- input$faasr_log
    json$LoggingServer <- input$logging_server
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
    
  })
  
  unretrieve <- function(val){
    if (!is.null(val)){
      if (!is.list(val)){
        val_merged_char <- NULL
        for (val_key in val){
          if (val_key == val[length(val)]){
            val_merged_char <- paste0(val_merged_char, val_key)
          } else {
            val_merged_char <- paste0(val_merged_char, val_key,", ")
          }
        }
        return(val_merged_char)
      } else {
        val_merged <- NULL
        for (val_key in names(val)){
          if (val_key == names(val[length(val)])){
            val_merged <- paste0(val_merged, val_key, "=",val[[val_key]])
          } else{
            val_merged <- paste0(val_merged, val_key, "=",val[[val_key]], ",\n")  
          }
        }
        return(val_merged)
      }
    } else {
      return(val)
    }
  }
  
  
  retrieve <- function(val) {
    if (!is.null(val)) {
      val_list <- unlist(strsplit(strsplit(val, '\n')[[1]], ','))
      text_list <- list()
      for (text in val_list){
        parts <- strsplit(text, ':|=')
        if (length(parts[[1]])<2){
          text_trim <- trimws(text)
          text_list <- unlist(c(text_list, text_trim))
        } else {
          re_input_key <- trimws(gsub('[\'"]', '', parts[[1]][1]))
          re_input_val <- trimws(gsub('[\'"]', '', parts[[1]][2]))
          text_list[[re_input_key]] <- re_input_val
        }
      }
      return(text_list)
    } else {
      return(val)
    }
  }
  
  output$downjson <- downloadHandler(
    filename = function() {
      "payload.json"
    },
    content = function(file) {
      json_result <- json_data()
      if (!is.null(json_result)) {
        writeLines(json_result, file)
      }
    }
  )
  
  output$fsm_func <- renderGrViz({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    fsm_func_edge <- NULL
    fsm_func_name <- NULL
    fsm_first_func <- NULL
    
    for (funcname in names(json$FunctionList)) {
      if (!is.null(json$FunctionInvoke) && funcname == json$FunctionInvoke){
        fsm_first_func <- paste0(funcname, " [label=",funcname,"];")
      } else{
        fsm_func_name <- paste0(fsm_func_name,funcname," [label=",funcname,"];")
      }
      for (next_func in json$FunctionList[[funcname]]$InvokeNext){
        fsm_func_edge <- paste0(fsm_func_edge,funcname,"->",next_func,";")
      }
    }
    
    
    grViz(paste0("digraph{
      graph[rankdir = LR, fontname=Helvetica];
      subgraph cluster_0 {
        graph[shape = rectangle];
        style = rounded;
        bgcolor = LemonChiffon;
        color = LemonChiffon;
        label = 'Functions';
        node[width=3, fixedsize=shape, fontsize=32, shape = doublecircle, style=filled, fontname=Helvetica, fillcolor=white, color=gray32];
        ",fsm_first_func,"
        node[width=3, fixedsize=shape, fontsize=32, shape = circle, style=filled, fontname=Helvetica, fillcolor=white, color=white];
        ",fsm_func_name,"
      };
      edge[color=black, arrowsize=1];
      ",fsm_func_edge,"
    }"))
    
  })
  
  output$fsm_data <- renderGrViz({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    fsm_data_name <- NULL
    fsm_logging <- NULL
    
    for (dataname in names(json$DataStores)) {
      if (!is.null(json$LoggingServer) && dataname == json$LoggingServer){
        fsm_logging <- paste0(dataname," [label=",dataname,"];")
      } else {
        fsm_data_name <- paste0(fsm_data_name,dataname," [label=",dataname,"];")
      }
    }
    
    grViz(paste0("digraph{
      graph[rankdir = LR, fontname=Helvetica];
      subgraph cluster_1 {
        graph[shape = rectangle];
        style = rounded;
        bgcolor = darkolivegreen3;
        color = darkolivegreen3;
        label = 'Data Servers';
        node[width=3, fixedsize=shape, fontsize=16, shape = folder, style=filled, fontname=Helvetica, fillcolor=white, color=gray32];
        ",fsm_logging,"
        node[width=3, fixedsize=shape, fontsize=16, shape = folder, style=filled, fontname=Helvetica, fillcolor=white, color=white];
        ",fsm_data_name,"
      };
    }"))
    
  })
  
  output$fsm_faas <- renderGrViz({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    fsm_faas_name <- NULL
    
    
    for (faasname in names(json$ComputeServers)) {
      fsm_faas_name <- paste0(fsm_faas_name,faasname," [label=",faasname,"];")
    }
    
    grViz(paste0("digraph{
      graph[rankdir = LR, fontname=Helvetica];
      subgraph cluster_1 {
        graph[shape = rectangle];
        style = rounded;
        bgcolor = Lightskyblue;
        label = 'FaaS Servers';
        color=Lightskyblue;
        node[width=3, fixedsize=shape, fontsize=16, shape = tab, style=filled, fontname=Helvetica, fillcolor=white, color=white];
        ",fsm_faas_name,"
      };
    }"))
    
  })
  
  observe({
    req(input$fsm_func_click)
    new_func <- input$fsm_func_click$nodeValues[[1]]
    updateTextInput(inputId="func_name", value=new_func)
    updateSelectInput(inputId="select1", selected="Functions")
  })
  observe({
    req(input$fsm_faas_click)
    new_faas <- input$fsm_faas_click$nodeValues[[1]]
    updateTextInput(inputId="faas_name", value=new_faas)
    updateSelectInput(inputId="select1", selected="FaaS Server")
  })
  observe({
    req(input$fsm_data_click)
    new_data <- input$fsm_data_click$nodeValues[[1]]
    updateTextInput(inputId="data_name", value=new_data)
    updateSelectInput(inputId="select1", selected="Data Server")
  })
}
