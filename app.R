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
      menuItem("Heatmap", tabName = "tab3", icon = icon("chart-pie")),
      menuItem("Gráfico 4", tabName = "tab4", icon = icon("chart-area")),
      menuItem("Gráfico 5", tabName = "tab5", icon = icon("chart-bar"))
    ),
    fileInput("file", "Upload your csv", accept = c(".csv"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "umap",
              fluidRow(
                box(
                  title = "UMAP Parameters",
                  width = 12, status = "primary", solidHeader = TRUE,
                  selectInput("color_by", "Color by:",
                              choices = c("Condition", "Sex", "Age", "cell_types")),
                  actionButton("run_umap", "Run UMAP")
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
                  actionButton("run_spattotal", "Run SpatPlot")
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
                  actionButton("run_spatregion", "Run SpatPlot")
                ),
                box(title = "SpatPlot", status = "primary", solidHeader = TRUE,  width = 8, height = 600,
                    plotOutput("spatregionplot", height = "550")
                )
              )
      ),
      tabItem(tabName = "tab3",
              fluidRow(
                box(plotOutput("plot3"), width = 12)
              )
      ),
      tabItem(tabName = "tab4",
              fluidRow(
                box(title = "Coming Soon", plotOutput("plot4"), width = 6)
              )
      ),
      tabItem(tabName = "tab5",
              fluidRow(
                box(plotOutput("plot5"), width = 12)
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
  
  output$umapPlot <- renderPlot({
    req(input$run_umap)
    plotUMAP(gobject = subsetsg,
             cell_color = input$color_by,
             point_size = 1.5,
             show_center_label = FALSE,
             point_alpha = 0.7)
  })
  
  output$violinPlot <- renderPlot({
    req(input$run_umap) # Requiere que el botón Run UMAP sea presionado
    violinPlot(subsetsg, feats = marker_genes, expression_values = 'scaled',
               cluster_column = input$color_by)
  })
  
  output$spattotalplot <- renderPlot({
    req(input$run_spattotal)
    spatPlot2D(gobject = filtered_sg, point_alpha = 0.7,
               cell_color = input$color_by_spat, show_legend = TRUE)
  })
  
  output$spatregionplot <- renderPlot({
    req(input$run_spatregion)
    region <- input$sub_region
    region_data = region_data_list[[region]]
    spatPlot2D(gobject = region_data, point_alpha = 0.7,
               cell_color = 'Subregion', show_legend = TRUE)
  })
  
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
}

shinyApp(ui, server)
