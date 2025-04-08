library(shiny)
library(bslib)
library(cna)
library(shinythemes)
library(DT)
library(frscore)
library(visNetwork)
library(causalHyperGraph)
library(DiagrammeR)
library(sortable)

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
                           actionButton("order.button1",
                                        label = "Enter ordering"),
                           actionButton("goButton", "Run"), offset = 1
                         ),
                         navset_card_underline(
                           title = "Results",
                           nav_panel("Models", DTOutput("cnares")),
                           nav_panel("Hypergraphs", 
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
                                        label = "granularity", 0.1, 0.05, 
                                        step = 0.05),
                           varSelectInput("out", 
                                          label = "outcome", 
                                          "data", multiple = TRUE),
                           actionButton("order.button2",
                                        label = "Enter ordering"),
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
  
  orl <- reactiveVal()
  
  rlist_items <- function(dataset){
    lapply(seq_along(dataset), function(x) {
      add_rank_list(
        text = paste("level", x),
        labels =  if (x == 1) colnames(dataset) else character(0L),
        input_id = paste0("rlist",x)
      )
    })
  }
  
  # share `ordering` 
  order_states <- reactiveValues(
    blists = NULL,
    cnaordering = NULL
    )
  
  
  ordering_modal <- function(button, dataset){
    #rank_list_items <- rlist_items(dataset)
    rank_list_items <- if(is.null(order_states$blists)){
      rlist_items(dataset)
    } else {
      order_states$blists
    }
    #print(rank_list_items)
    showModal(
      modalDialog(
        title = "Ordering",
        renderUI({
          do.call("bucket_list", args = c(
            list(header = "",
                 group_name = "order_bucket",
                 orientation = "horizontal"),
            
            rank_list_items
          ))
        }),
        footer = tagList(
          actionButton(button, "Update"),
          modalButton("Close")
        )
      )
    )
  }
  
  upd_listvals <- function(){
    upd_vals <- lapply(seq_along(dat()), function(x) {
      input[[paste0("rlist", x)]]
    })
    upd_vals <- upd_vals[sapply(upd_vals, \(x) length(x)>0)] 
    return(upd_vals)
  }
  
  upd_ranklist <- function(dataset){
    upr <- lapply(seq_along(dataset), \(x){
      add_rank_list(
        text = paste("level", x),
        labels = input[[paste0("rlist", x)]],
        input_id = paste0("rlist",x)
      )
    })
    return(upr)
  }
  
  observeEvent(input$order.button1, {
    ordering_modal("update.button1", dat())
  })
  
  observeEvent(input$update.button1, {
    order_states$blists <- upd_ranklist(dat())
    order_states$cnaordering <- upd_listvals()
    print(order_states$blists)
    removeModal()
  })
  
  observeEvent(input$order.button2, {
    ordering_modal("update.button2", dat())
  })
  
  observeEvent(input$update.button2, {
    order_states$blists <- upd_ranklist(dat())
    order_states$cnaordering <- upd_listvals()
    removeModal()
  })
  
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
  
  rescna <- eventReactive(input$goButton, {
    if (input$panel == "cna") 
      csf(cna(dat(), outcome = if (length(ll()) == 0) TRUE else ll(),
              ordering = order_states[["cnaordering"]],
              con = input$con, cov = input$cov))
  })
  
  resfrscore <- eventReactive(input$goButton, {
    if (input$panel == "frscored_cna") 
      frscored_cna(dat(), outcome = if (length(ll()) == 0) TRUE else ll(),
                   ordering = order_states[["cnaordering"]],
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
  

  plot <- reactive({
    #cat(file = stderr(), class(res()))
    if(input$panel == "frscored_cna"){
    plot_submodel_network(resfrscore())
  }})
  output$plot <- renderVisNetwork(plot())
  
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
        #print(ic)
        output[[plotname]] <- renderGrViz(hgraph()[[ic]])
      })
    }
  })
  


  
}


shinyApp(ui = ui, server = server)