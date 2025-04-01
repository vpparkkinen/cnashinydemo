library(shiny)
library(bslib)
library(cna)
library(shinythemes)
library(DT)
library(frscore)
library(visNetwork)
library(causalHyperGraph)
library(DiagrammeR)

ui <- page_fluid(
  navset_tab(id = "panel",
             nav_panel("vanilla cna", value = "cna",
                       
                       page_sidebar(
                         sidebar = sidebar(
                           selectInput("data1", 
                                       label = "data", 
                                       choices = c("d.error", "d.educate")),
                           actionButton("disp", label = "view data"),
                           #tableOutput("dataset"),
                           sliderInput("con", 
                                       label = "consistency", 
                                       min = 0.5, max = 1, value = 1),
                           sliderInput("cov", 
                                       label = "coverage", 
                                       min = 0.5, max = 1, value = 1),
                           varSelectInput("out", 
                                          label = "outcome", 
                                          "data1", multiple = TRUE),
                           actionButton("goButton", "Run"), offset = 1
                         ),
                         navset_card_underline(
                           title = "Results",
                           nav_panel("Models", DTOutput("cnares")),
                           nav_panel("Hypergraphs", 
                                     #verbatimTextOutput("hyper"))
                                     #grVizOutput("hyper"))
                                     uiOutput("hyper"))
                         )
                       )
             ),
             nav_panel("frscored_cna", value = "frscored_cna",
                       page_sidebar(
                         sidebar = sidebar(
                           selectInput("data2", 
                                       label = "data", 
                                       choices = c("d.error", "d.educate")),
                           actionButton("disp", label = "view data"),
                           tableOutput("dataset"),
                           sliderInput("fit.range", 
                                       label = "fit.range", 
                                       min = 0.5, max = 1, value = c(0.7, 1)),
                           numericInput("granularity", 
                                        label = "granularity", 0.05, 0.1, 
                                        step = 0.05),
                           varSelectInput("out", 
                                          label = "outcome", 
                                          "data", multiple = TRUE),
                           actionButton("goButton", "Run"), offset = 1
                         ),
                         navset_card_underline(
                           title = "Results",
                           nav_panel("Models", DTOutput("frscoreres")),
                           nav_panel("Submodel graph", visNetworkOutput("plot"))
                         )
                       )
             )
  )
)
  


server <- function(input, output, session){
  
  shared_state <- reactiveValues(data = "d.error")
  
  
  observeEvent(input$data1, {
    shared_state$data <- input$data1
    updateSelectInput(session, "data2", selected = shared_state$data)
  })
  
  observeEvent(input$data2, {
    shared_state$data <- input$data2
    updateSelectInput(session, "data1", selected = shared_state$data)
  })
  
  dat <- reactive({switch(shared_state$data,
                          "d.error" = frscore::d.error,
                          "d.educate" = cna::d.educate)})
  
  
  observeEvent(input$disp, {
    showModal((modalDialog(
      title = input$data,
      renderTable(dat()),
      easyClose = TRUE,
      footer = NULL
    )))
  })
  
  ll <- reactive({
    c(as.character(input$out))
  })
  
  # observe({
  #   updateSelectInput(session, "data", label = "data", choices = c("d.error",
  #                                                                  "d.educate"))
  # })
  # 
  # 
  # observeEvent(dat(), {
  #   updateVarSelectInput(session, "out", data = dat())
  # })
  
  
  rescna <- eventReactive(input$goButton, {
    if (input$panel == "cna") 
      csf(cna(dat(), outcome = if (length(ll()) == 0) TRUE else ll(),
              con = input$con, cov = input$cov))
  })
  
  resfrscore <- eventReactive(input$goButton, {
    if (input$panel == "frscored_cna") 
      frscored_cna(dat(), outcome = if (length(ll()) == 0) TRUE else ll(),
                   fit.range = input$fit.range, 
                   granularity = input$granularity)
  })
  
  output$cnares <-  
    renderDT({if(input$panel == "cna"){
      rescna()}
      })
  
  output$frscoreres <-  
    renderDT(if(input$panel == "frscored_cna") 
      formatRound(datatable(resfrscore()[[1]]), 
                  names(resfrscore()[[1]][3:length(names(resfrscore()[[1]]))])))
  
  # TODO: vizs separately for cna/frscore 
  plot <- reactive({
    #cat(file = stderr(), class(res()))
    if(input$panel == "frscored_cna"){
    plot_submodel_network(resfrscore())
  }})
  output$plot <- renderVisNetwork(plot())
  
  # hgraph <- reactive({
  #   if(input$panel == "cna"){chg(rescna()$condition)[[1]]$graph}
  # })

############## dynamic UI version
    
  hgraph <- reactive({
    if(input$panel == "cna"){lapply(rescna()$condition, \(x) chg(x)[[1]]$graph)}
  })

  output$hyper <- renderUI({
    pl <- lapply(seq_along(hgraph()), \(x){
      plname <- paste("plot", x, sep = "")
      grVizOutput(plname)
    })
    do.call(tagList, pl)
  })

  observe({
    for(i in seq_along(hgraph())){
      local({
        ic <- i
        plotname <- paste0("plot", ic)
        print(ic)
        output[[plotname]] <- renderGrViz(hgraph()[[ic]])
      })
    }
  })
  
############
  #output$hyper <- renderGrViz({hgraph()})
  
}


shinyApp(ui = ui, server = server)