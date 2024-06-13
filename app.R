library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(Giotto)
#cargar RDATA de spatial transcriptomic y path con setwd()

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "SCHEMA Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("UMAP", tabName = "umap", icon = icon("th")),
      menuItem("Gráfico 2", tabName = "tab2", icon = icon("chart-line")),
      menuItem("Gráfico 3", tabName = "tab3", icon = icon("chart-pie")),
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
                  width = 4, status = "primary", solidHeader = TRUE,
                  selectInput("color_by", "Color by:",
                              choices = c("Condition", "Sex", "Age", "cell_types")),
                  actionButton("run_umap", "Run UMAP")
                ),
                box(title = "UMAP Plot", status = "primary", solidHeader = TRUE,
                    plotOutput("umapPlot")
                )
              )
      ),
      tabItem(tabName = "tab2",
              fluidRow(
                box(plotOutput("plot2"), width = 12)
              )
      ),
      tabItem(tabName = "tab3",
              fluidRow(
                box(plotOutput("plot3"), width = 12)
              )
      ),
      tabItem(tabName = "tab4",
              fluidRow(
                box(plotOutput("plot4"), width = 12)
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
  
  output$plot2 <- renderPlot({
    req(data())
    ggplot(data(), aes(x = Column3, y = Column4)) + 
      geom_bar(stat = "identity") +
      theme_minimal()
  })
  
  output$plot3 <- renderPlot({
    req(data())
    ggplot(data(), aes(x = factor(Column5), y = Column6)) + 
      geom_boxplot() +
      theme_minimal()
  })
  
  output$plot4 <- renderPlot({
    req(data())
    ggplot(data(), aes(x = Column7)) + 
      geom_histogram(binwidth = 10) +
      theme_minimal()
  })
  
  output$plot5 <- renderPlot({
    req(data())
    ggplot(data(), aes(x = Column8, y = Column9, color = Column10)) + 
      geom_point() +
      theme_minimal()
  })
}

shinyApp(ui, server)
