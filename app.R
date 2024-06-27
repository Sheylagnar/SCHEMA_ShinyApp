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
      menuItem("Correlation Bar Plot", icon = icon("chart-area"),
               menuSubItem("Total", tabName = "cortotal", icon = icon("angle-right")),
               menuSubItem("Region", tabName = "corregion", icon = icon("angle-right"))
      ),
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
                            plotOutput("heatmapPlot1", height = "375")
                        )
                    )
                ),
                box(width = 12, status = "primary", solidHeader = TRUE, height = 400,
                    div(class = "plot-container",
                        div(class = "left-title", "SR"),
                        div(class = "plot-output",
                            plotOutput("heatmapPlot2", height = "375")
                        )
                    )
                )
              )
      ),
      tabItem(tabName = "cortotal",
              fluidRow(
                box(
                  title = "Parameters",
                  width = 12, status = "primary", solidHeader = TRUE,
                  selectInput("corplot_by", "Corplot by:",
                              choices = c("Age", "Sex")),
                  actionButton("run_corplot", "Generate Corplot"),
                  downloadButton("downloadCorplotELS", "Download Plots ELS"),
                  downloadButton("downloadCorplotSR", "Download Plots SR")
                ),
                box(width = 12, status = "primary", solidHeader = TRUE, height = 400,
                    div(class = "plot-container",
                        div(class = "left-title", "ELS"),
                        div(class = "plot-output",
                            plotOutput("corPlotELS", height = "370")
                        )
                    )
                ),
                box(width = 12, status = "primary", solidHeader = TRUE, height = 400,
                    div(class = "plot-container",
                        div(class = "left-title", "SR"),
                        div(class = "plot-output",
                            plotOutput("corPlotSR", height = "370")
                        )
                    )
                )
              )
      ),
      tabItem(tabName = "corregion",
              fluidRow(
                box(
                  title = "Parameters",
                  width = 12, status = "primary", solidHeader = TRUE,
                  selectInput("corplot_reg_by", "Corplot by:",
                              choices = data_regions),
                  actionButton("run_corplot_reg", "Generate Corplot"),
                  downloadButton("downloadCorplotRegELS", "Download Plots ELS"),
                  downloadButton("downloadCorplotRegSR", "Download Plots SR")
                ),
                box(width = 12, status = "primary", solidHeader = TRUE, height = 400,
                    div(class = "plot-container",
                        div(class = "left-title", "ELS"),
                        div(class = "plot-output",
                            plotOutput("corPlotRegELS", height = "370")
                        )
                    )
                ),
                box(width = 12, status = "primary", solidHeader = TRUE, height = 400,
                    div(class = "plot-container",
                        div(class = "left-title", "SR"),
                        div(class = "plot-output",
                            plotOutput("corPlotRegSR", height = "370")
                        )
                    )
                )
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
    generate_correlations(df, "ELS", input$heatmap_by)
  })
  
  output$heatmapPlot1 <- renderPlot({
    heatmap_plot1()()
  })
  
  output$downloadHeatmap1 <- downloadHandler(
    filename = function() { paste("Heatmap_Plot1", Sys.Date(), ".png", sep="") },
    content = function(file) {
      png(file)
      heatmap_plot1()() # Llama a la misma función para generar el gráfico y guardarlo
      dev.off()
    }
  )
  
  #HEATMAP SR
  heatmap_plot2 <- reactive({
    req(input$run_heatmap)
    generate_correlations(df, "SR", input$heatmap_by)
  })

  output$heatmapPlot2 <- renderPlot({
    heatmap_plot2()()
  })
  
  output$downloadHeatmap2 <- downloadHandler(
    filename = function() { paste("Heatmap_Plot2", Sys.Date(), ".png", sep="") },
    content = function(file) {
      png(file)
      heatmap_plot2()() # Llama a la misma función para generar el gráfico y guardarlo
      dev.off()
    }
  )
  
  #CORPLOT TOTAL 
  plot_ELS <- reactive({
    req(input$run_corplot)
    generate_plots(df, "ELS", input$corplot_by)
  })
  
  output$corPlotELS <- renderPlot({
    plot_ELS()
  })
  
  output$downloadCorplotELS <- downloadHandler(
    filename = function() {
      paste("combined_plot_ELS_", input$corplot_by, ".png", sep = "")
    },

    content = function(file) {
      ggsave(file, plot = plot_ELS(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )
  
  #CORPLOT TOTAL SR
  
  plot_SR <- reactive({
    req(input$run_corplot)
    generate_plots(df, "SR", input$corplot_by)
  })
  
  output$corPlotSR <- renderPlot({
    plot_SR()
  })
  
  output$downloadCorplotSR <- downloadHandler(
    filename = function() {
      paste("combined_plot_SR_", input$corplot_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_SR(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )

  #  #CORPLOT BY REGION TOTAL 
  plot_reg_ELS <- reactive({
    req(input$run_corplot_reg)
    generate_plots(df, "ELS", "Age", input$corplot_reg_by)
  })
  
  output$corPlotRegELS <- renderPlot({
    plot_reg_ELS()
  })
  
  output$downloadCorplotRegELS <- downloadHandler(
    filename = function() {
      paste("combined_plot_ELS_reg_", input$corplot_by, ".png", sep = "")
    },
    
    content = function(file) {
      ggsave(file, plot = plot_ELS(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )
  
  #CORPLOT TOTAL BY REGIION SR
  
  plot_reg_SR <- reactive({
    req(input$run_corplot_reg)
    generate_plots(df, "SR", "Age", input$corplot_reg_by)
  })
  
  output$corPlotRegSR <- renderPlot({
    plot_reg_SR()
  })
  
  output$downloadCorplotRegSR <- downloadHandler(
    filename = function() {
      paste("combined_plot_SR_reg_", input$corplot_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_SR(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )
  
  # Handlers para descargar los gráficos usando ggsave

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

