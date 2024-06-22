library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(Giotto)
#cargar RDATA de spatial transcriptomic y path con 
#setwd()

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "SCHEMA Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("UMAP", tabName = "umap", icon = icon("th")),
      menuItem("Spatial Plot", icon = icon("chart-line"),
               menuSubItem("Total", tabName = "spattotal", icon = icon("angle-right")),
               menuSubItem("Region", tabName = "spatregion", icon = icon("angle-right"))
      ),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("chart-pie")),
      menuItem("Gráfico 4", tabName = "tab4", icon = icon("chart-area")),
      menuItem("Gráfico 5", tabName = "tab5", icon = icon("chart-bar"))
    ),
    fileInput("file", "Upload your csv", accept = c(".csv"))
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .left-title {
          background-color: #007bff;
          color: white;
          padding: 10px;
          font-size: 20px;
          font-weight: bold;
          display: flex;
          align-items: center;
          justify-content: center;
          width: 100px;
        }
        .plot-container {
          display: flex;
          align-items: stretch;
          height: 100%;
        }
        .plot-output {
          flex-grow: 1;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "umap",
              fluidRow(
                box(
                  title = "UMAP Parameters",
                  width = 12, status = "primary", solidHeader = TRUE,
                  selectInput("color_by", "Color by:",
                              choices = c("Condition", "Sex", "Age", "cell_types")),
                  actionButton("run_umap", "Run UMAP"),
                  downloadButton("downloadUMAP", "Download UMAP Plot"),
                  downloadButton("downloadViolin", "Download Violin Plot")
                ),
                box(title = "UMAP Plot", status = "primary", solidHeader = TRUE, width = 6, height = 600,
                    plotOutput("umapPlot", height = "550")
                ),
                box(title = "Violin Plot", status = "primary", solidHeader = TRUE, width = 6, height = 600,
                    plotOutput("violinPlot", height = "550")
                )
              )
      ),
      tabItem(tabName = "spattotal",
              fluidRow(
                box(
                  title = "SpatPlot Parameters",
                  width = 4, status = "primary", solidHeader = TRUE,
                  selectInput("color_by_spat", "Color by:",
                              choices = c("Region", "Sex", "Age", "cell_types")),
                  actionButton("run_spattotal", "Run Spatplot"),
                  downloadButton("downloadSpatTotal", "Download Spat Total Plot")
                ),
                box(title = "SpatPlot", status = "primary", solidHeader = TRUE, width = 8, height = 600,
                    plotOutput("spattotalplot", height = "550")
                )
              )
      ),
      tabItem(tabName = "spatregion",
              fluidRow(
                box(
                  title = "SpatPlot Parameters",
                  width = 4, status = "primary", solidHeader = TRUE,
                  selectInput("sub_region", "Subregion by:",
                              choices = data_regions),
                  actionButton("run_spatregion", "Run SpatPlot"),
                  downloadButton("downloadSpatRegion", "Download Spat Region Plot")
                ),
                box(title = "SpatPlot", status = "primary", solidHeader = TRUE, width = 8, height = 600,
                    plotOutput("spatregionplot", height = "550")
                )
              )
      ),
      tabItem(tabName = "heatmap",
              fluidRow(
                box(
                  title = "Heatmap Parameters",
                  width = 12, status = "primary", solidHeader = TRUE,
                  selectInput("heatmap_by", "Heatmap by:",
                              choices = c("Age", "Sex")),
                  actionButton("run_heatmap", "Generate Heatmap"),
                  downloadButton("downloadHeatmap1", "Download Heatmap 1"),
                  downloadButton("downloadHeatmap2", "Download Heatmap 2")
                ),
                box(width = 12, status = "primary", solidHeader = TRUE, height = 400,
                    div(class = "plot-container",
                        div(class = "left-title", "ELS"),
                        div(class = "plot-output",
                            plotOutput("heatmapPlot1", height = "370")
                        )
                    )
                ),
                box(width = 12, status = "primary", solidHeader = TRUE, height = 400,
                    div(class = "plot-container",
                        div(class = "left-title", "SR"),
                        div(class = "plot-output",
                            plotOutput("heatmapPlot2", height = "370")
                        )
                    )
                )
              )
      ),
      tabItem(tabName = "tab4",
              fluidRow(
                box(title = "Coming Soon", plotOutput("plot4"), width = 6,
                    downloadButton("downloadPlot4", "Download Plot 4"))
              )
      ),
      tabItem(tabName = "tab5",
              fluidRow(
                box(plotOutput("plot5"), width = 12,
                    downloadButton("downloadPlot5", "Download Plot 5"))
              )
      )
    )
  )
)
# Define server logic required to draw a histogram

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  umap_plot <- reactive({
    req(input$run_umap)
    plotUMAP(gobject = subsetsg,
             cell_color = input$color_by,
             point_size = 1.5,
             show_center_label = FALSE,
             point_alpha = 0.7)
  })
  output$umapPlot <- renderPlot({
    umap_plot()
  })
  
  output$downloadUMAP <- downloadHandler(
    filename = function() { paste("UMAP_Plot", Sys.Date(), ".png", sep="") },
    content = function(file) {
      ggsave(file, plot = umap_plot())
    }
  )
  
  #VIOLIN 
  violin_plot <- reactive({
    req(input$run_umap)
    violinPlot(subsetsg, feats = marker_genes, expression_values = 'scaled',
               cluster_column = input$color_by)
  })
  
  output$violinPlot <- renderPlot({
    violin_plot()
  })
  
  output$downloadViolin <- downloadHandler(
    filename = function() { paste("Violin_Plot", Sys.Date(), ".png", sep="") },
    content = function(file) {
      ggsave(file, plot = violin_plot())
    }
  )
  
  #SPATTOTAL
  spattotal_plot <- reactive({
    req(input$run_spattotal)
    spatPlot2D(gobject = filtered_sg, point_alpha = 0.7,
               cell_color = input$color_by_spat, show_legend = TRUE)
  })
  
  output$spattotalplot <- renderPlot({
    spattotal_plot()
  })
  
  output$downloadSpatTotal <- downloadHandler(
    filename = function() { paste("Spat_Total_Plot", Sys.Date(), ".png", sep="") },
    content = function(file) {
      ggsave(file, plot = spattotal_plot())
    }
  )
  
  #SPATREGION
  spatregion_plot <- reactive({
    req(input$run_spatregion)
    region <- input$sub_region
    region_data <- region_data_list[[region]]
    spatPlot2D(gobject = region_data, point_alpha = 0.7,
               cell_color = 'Subregion', show_legend = TRUE)
  })
  
  output$spatregionplot <- renderPlot({
    spatregion_plot()
  })
  
  output$downloadSpatRegion <- downloadHandler(
    filename = function() { paste("Spat_Region_Plot", Sys.Date(), ".png", sep="") },
    content = function(file) {
      ggsave(file, plot = spatregion_plot())
    }
  )
  
  #HEATMAP ELS
  
  heatmap_plot1 <- reactive({
    req(input$run_heatmap)
    if (input$heatmap_by == "Age") {
      corr_list <- list(corr.p10_ELS, corr.p21_ELS, corr.p40_ELS)
      titles <- c("P 10", "P 21", "P 40")
    } else {
      corr_list <- list(corr.male_ELS, corr.female_ELS)
      titles <- c("M", "F")
    }
    list(corr_list = corr_list, titles = titles)
  })
  
  output$heatmapPlot1 <- renderPlot({
    heatmap <- heatmap_plot1()
    par(mfrow = c(1, length(heatmap$corr_list)), mar = c(5, 4, 4, 2) + 0.1)
    for (i in 1:length(heatmap$corr_list)) {
      corrplot(heatmap$corr_list[[i]], order = 'original', method = 'color', title = heatmap$titles[i])
      title(main = titles[i], line = 3, cex.main = 1.5)
    }
    par(mfrow = c(1, 1))
  })
  
  output$downloadHeatmap1 <- downloadHandler(
    filename = function() { paste("Heatmap_Plot1", Sys.Date(), ".png", sep="") },
    content = function(file) {
      heatmap <- heatmap_plot1()
      save_corrplot(file, heatmap$corr_list, heatmap$titles)
    }
  )
  
  #HEATMAP SR
  heatmap_plot2 <- reactive({
    req(input$run_heatmap)
    if (input$heatmap_by == "Age") {
      corr_list <- list(corr.p10, corr.p21, corr.p40) # listas alternativas
      titles <- c("P 10", "P 21", "P 40")
    } else {
      corr_list <- list(corr.male, corr.female) # listas alternativas
      titles <- c("M", "F")
    }
    list(corr_list = corr_list, titles = titles)
  })
  
  output$heatmapPlot2 <- renderPlot({
    heatmap <- heatmap_plot2()
    par(mfrow = c(1, length(heatmap$corr_list)), mar = c(5, 4, 4, 2) + 0.1)
    for (i in 1:length(heatmap$corr_list)) {
      corrplot(heatmap$corr_list[[i]], order = 'original', method = 'color')
      title(main = titles[i], line = 3, cex.main = 1.5)
    }
    par(mfrow = c(1, 1))
  })
  
  output$downloadHeatmap2 <- downloadHandler(
    filename = function() { paste("Heatmap_Plot2", Sys.Date(), ".png", sep="") },
    content = function(file) {
      heatmap <- heatmap_plot2()
      save_corrplot(file, heatmap$corr_list, heatmap$titles)
    }
  )
  
  #IMAGE
  output$plot4 <- renderPlot({
    img_path <- "image.png" 
    img <- png::readPNG(img_path)
    grid::grid.raster(img)
  })
  
  output$plot5 <- renderPlot({
    req(data())
    ggplot(data(), aes(x = Column8, y = Column9, color = Column10)) + 
      geom_point() +
      theme_minimal()
  })
  
  # Handlers para descargar los gráficos usando ggsave

  output$downloadPlot3 <- downloadHandler(
    filename = function() { paste("Plot3", Sys.Date(), ".png", sep="") },
    content = function(file) {
      plot3 <- ggplot(data(), aes(x = Column8, y = Column9, color = Column10)) + 
        geom_point() +
        theme_minimal()
      ggsave(file, plot = plot3)
    }
  )
  
  output$downloadPlot4 <- downloadHandler(
    filename = function() { paste("Plot4", Sys.Date(), ".png", sep="") },
    content = function(file) {
      png(file)
      img_path <- "image.png" 
      img <- png::readPNG(img_path)
      grid::grid.raster(img)
      dev.off()
    }
  )
  
  output$downloadPlot5 <- downloadHandler(
    filename = function() { paste("Plot5", Sys.Date(), ".png", sep="") },
    content = function(file) {
      plot5 <- ggplot(data(), aes(x = Column8, y = Column9, color = Column10)) + 
        geom_point() +
        theme_minimal()
      ggsave(file, plot = plot5)
    }
  )
}

shinyApp(ui, server)
