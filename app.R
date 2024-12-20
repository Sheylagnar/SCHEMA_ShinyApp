source("functions_spatial_rnaseq.R")
library(shiny)
library(shinydashboard)

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
      menuItem("Gene Expression Comparison", icon = icon("chart-bar"),
               menuSubItem("Region", tabName = "comregion", icon = icon("angle-right")),
               menuSubItem("Cell type", tabName = "comcelltype", icon = icon("angle-right"))
      ),
      menuItem("Dotplot", icon = icon("chart-bar"),
               menuSubItem("Condition", tabName = "condotplot", icon = icon("angle-right")),
               menuSubItem("Sex", tabName = "sexdotplot", icon = icon("angle-right")),
               menuSubItem("Age", tabName = "agedotplot", icon = icon("angle-right"))
               
      ),
      menuItem("Dispersion",tabName = "disp", icon = icon("chart-bar")
      ),
      menuItem("Area heatmap",tabName = "area", icon = icon("chart-bar")
      )
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
          height: 100% !important; /* Asegura que el plotOutput use todo el espacio */
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
      ),
      tabItem(tabName = "comregion",
              fluidRow(
                box(
                  title = "Parameters",
                  width = 3, status = "primary", solidHeader = TRUE,
                  selectInput("corregion_by", "Plot by:",
                              choices = c("Age", "Sex")),
                  actionButton("run_corregion", "Run Heatmap"),
                  downloadButton("downloadcorregion", "Download Plot")
                ),
                box(title = "Comparison Heatmpat", status = "primary", solidHeader = TRUE, width = 9, height = 600,
                    plotOutput("corregionplot", height = "550", width = "100%")
                )
              )
      ),
      tabItem(tabName = "comcelltype",
              fluidRow(
                box(
                  title = "Parameters",
                  width = 3, status = "primary", solidHeader = TRUE,
                  selectInput("comcell_by", "Plot by:",
                              choices = c("Age", "Sex")),
                  actionButton("run_comcell", "Run Heatmap"),
                  downloadButton("downloadcomcell", "Download Plot")
                ),
                box(title = "Comparison Heatmpat", status = "primary", solidHeader = TRUE, width = 9, height = 600,
                    plotOutput("comcellplot", height = "550", width = "100%")
                )
              )
      ),
      tabItem(tabName = "condotplot",
              fluidRow(
                box(
                  title = "Parameters",
                  width = 4, status = "primary", solidHeader = TRUE,
                  radioButtons("total_filter_by", "Filter by:", choices = c("Sex", "Age", "All")),
                  conditionalPanel(
                    condition = "input.total_filter_by == 'Sex'",
                    selectInput("total_sex", "Select Sex:", choices = c("F", "M"))
                  ),
                  conditionalPanel(
                    condition = "input.total_filter_by == 'Age'",
                    selectInput("total_age", "Select Age:", choices = c(10, 21, 40, 90))
                  ),
                  actionButton("run_total_condotplot", "Run Dotplot"),
                  downloadButton("downloadtotalcondotplot", "Download Plot")
                ),
                box(title = "Dotplot", status = "primary", solidHeader = TRUE, width = 8, height = 400,
                    plotOutput("condottotalPlot", height = "350", width = "100%")
                ), 
                box(
                  title = "Parameters",
                  width = 4, status = "primary", solidHeader = TRUE,
                  selectInput("gene_by", "Plot by gene:", choices = schema_genes),
                  radioButtons("filter_by", "Filter by:", choices = c("Sex", "Age", "All")),
                  conditionalPanel(
                    condition = "input.filter_by == 'Sex'",
                    selectInput("sex", "Select Sex:", choices = c("F", "M"))
                  ),
                  conditionalPanel(
                    condition = "input.filter_by == 'Age'",
                    selectInput("age", "Select Age:", choices = c(10, 21, 40, 90))
                  ),
                  actionButton("run_condotplot", "Run Dotplot"),
                  downloadButton("downloadcondotplot", "Download Plot")
                ),
                box(title = "Dotplot", status = "primary", solidHeader = TRUE, width = 8, height = 400,
                    plotOutput("condotPlot", height = "350", width = "100%")
                )
              )
      ),
      tabItem(tabName = "sexdotplot",
              fluidRow(
                box(
                  title = "Parameters",
                  width = 4, status = "primary", solidHeader = TRUE,
                  radioButtons("total_filter_by", "Filter by:", choices = c("Condition", "Age", "All")),
                  conditionalPanel(
                    condition = "input.total_filter_by == 'Condition'",
                    selectInput("total_condition", "Select Condition:", choices = c("ELS", "SR"))
                  ),
                  conditionalPanel(
                    condition = "input.total_filter_by == 'Age'",
                    selectInput("total_age", "Select Age:", choices = c(10, 21, 40, 90))
                  ),
                  actionButton("run_total_sexdotplot", "Run Dotplot"),
                  downloadButton("downloadtotalsexdotplot", "Download Plot")
                ),
                box(title = "Dotplot", status = "primary", solidHeader = TRUE, width = 8, height = 400,
                    plotOutput("sexdottotalPlot", height = "350", width = "100%")
                ), 
                box(
                  title = "Parameters",
                  width = 4, status = "primary", solidHeader = TRUE,
                  selectInput("gene_by", "Plot by gene:", choices = schema_genes),
                  radioButtons("filter_by", "Filter by:",
                              choices = c("All", "Age", "Condition"), selected = "All"),
                  conditionalPanel(
                    condition = "input.filter_by == 'Age'",
                    selectInput("age", "Select Age:", choices = c(10, 21, 40, 90), selected = 10),
                    selectInput("additional_filter_age", "Additional Filter:",
                                choices = c("None", "ELS", "SR"), selected = "None")
                  ),
                  conditionalPanel(
                    condition = "input.filter_by == 'Condition'",
                    selectInput("condition", "Select Condition:", choices = c("ELS", "SR"), selected = "Condition1"),
                    selectInput("additional_filter_condition", "Additional Filter:",
                                choices = c("None", 10, 21, 40, 90), selected = "None")
                  ),
                  actionButton("run_sexdotplot", "Run Dotplot"),
                  downloadButton("downloadsexdotplot", "Download Plot")
                ),
                box(title = "Dotplot", status = "primary", solidHeader = TRUE, width = 8, height = 400,
                    plotOutput("sexdotPlot", height = "350", width = "100%")
                )
              )
      ),
      tabItem(tabName = "agedotplot",
              fluidRow(
                box(
                  title = "Parameters",
                  width = 4, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(6, selectInput("age_group_1_by", "Age 1:", choices = c(10, 21, 40, 90))),
                    column(6, selectInput("age_group_2_by", "Age 2:", choices = c(10, 21, 40, 90)))
                  ),
                  radioButtons("total_filter_by", "Filter by:", choices = c("Condition", "Sex", "All")),
                  conditionalPanel(
                    condition = "input.total_filter_by == 'Condition'",
                    selectInput("total_condition", "Select Condition:", choices = c("ELS", "SR"))
                  ),
                  conditionalPanel(
                    condition = "input.total_filter_by == 'Sex'",
                    selectInput("total_age", "Select Age:", choices = c("M", "F"))
                  ),
                  actionButton("run_total_agedotplot", "Run Dotplot"),
                  downloadButton("downloadtotalagedotplot", "Download Plot")
                ),
                box(title = "Dotplot", status = "primary", solidHeader = TRUE, width = 8, height = 400,
                    plotOutput("agedottotalPlot", height = "350", width = "100%")
                ),    
                box(
                  title = "Parameters",
                  width = 4, status = "primary", solidHeader = TRUE,
                  selectInput("gene_by", "Plot by gene:", choices = schema_genes),
                  fluidRow(
                    column(6, selectInput("group_1_by", "Age 1:", choices = c(10, 21, 40, 90))),
                    column(6, selectInput("group_2_by", "Age 2:", choices = c(10, 21, 40, 90)))
                  ),
                  radioButtons("filter_by", "Filter by:",
                              choices = c("Sex", "Condition", "All"), selected = "Sex"),
                  conditionalPanel(
                    condition = "input.filter_by == 'Sex'",
                    selectInput("sex", "Select Sex:", choices = c("M", "F"), selected = "M"),
                    selectInput("additional_filter_age", "Additional Filter:",
                                choices = c("None", "ELS", "SR"), selected = "None")
                  ),
                  conditionalPanel(
                    condition = "input.filter_by == 'Condition'",
                    selectInput("condition", "Select Condition:", choices = c("ELS", "SR"), selected = "Condition1"),
                    selectInput("additional_filter_condition", "Additional Filter:",
                                choices = c("None", "M", "F"), selected = "None")
                  ),
                  actionButton("run_agedotplot", "Run Dotplot"),
                  downloadButton("downloadagedotplot", "Download Plot")
                ),
                box(title = "Dotplot", status = "primary", solidHeader = TRUE, width = 8, height = 400,
                    plotOutput("agedotPlot", height = "350", width = "100%")
                )
              )
      ),
      tabItem(tabName = "disp",
              fluidRow(
                box(
                  title = "Disp Parameters",
                  width = 4, status = "primary", solidHeader = TRUE,
                  selectInput("gene_by", "Plot by gene:", choices = schema_genes),
                  radioButtons("col_by", "Color by:",
                              choices =  c("Region", "foldchange")),
                  actionButton("run_disp", "Run DispPlot"),
                  downloadButton("downloadDispPlot", "Download Disp Plot")
                ),
                box(title = "DispPlot", status = "primary", solidHeader = TRUE, width = 8, height = 600,
                    plotOutput("displot", height = "550"),
                )
              )
      ), 
      tabItem(tabName = "area",
              fluidRow(
                box(
                  title = "Area Parameters",
                  width = 4, status = "primary", solidHeader = TRUE,
                  selectInput("gene_by", "Plot by gene:", choices = schema_genes),
                  selectInput("region_by", "Plot by region:", choices = data_regions),
                  selectInput("con_by", "Plot by condition:", choices = c("ELS", "SR")),
                  radioButtons("col_by", "Color by:",
                               choices =  c("Age", "Sex")),
                  actionButton("run_area", "Run AreaPlot"),
                  downloadButton("downloadAreaPlot", "Download Area Plot")
                ),
                box(title = "Percentage Heatmap", status = "primary", solidHeader = TRUE, width = 8, height = 405,
                    plotOutput("percheat", height = "350")
                ),
                box(title = "Puncta Heatmaps", status = "primary", solidHeader = TRUE, width = 12, height = 405,
                    plotOutput("punctheat", height = '350')
                )
              )
      )
    )
  )
)
# Define server logic required to draw a histogram

server <- function(input, output) {
  dtbs <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  #UMAP
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
    generate_correlations(data_harm_scaled, "ELS", input$heatmap_by)
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
    generate_correlations(data_harm_scaled, "SR", input$heatmap_by)
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
  
  #CORBARPLOT TOTAL 
  plot_ELS <- reactive({
    req(input$run_corplot)
    generate_plots(data_harm_raw, "ELS", input$corplot_by)
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
    generate_plots(data_harm_raw, "SR", input$corplot_by)
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
    generate_plots(data_harm_raw, "ELS", "Age", input$corplot_reg_by)
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
    generate_plots(data_harm_raw, "SR", "Age", input$corplot_reg_by)
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
  
  # COMPARISON PHEATMAP REGION 

  plot_comheat <- reactive({
    req(input$run_corregion)
    generate_combined_heatmaps(data_harm_scaled, schema_genes, input$corregion_by, "Region")
  })
  
  output$corregionplot <- renderPlot({
    plot_comheat()
  })
  
  output$downloadcorregion <- downloadHandler(
    filename = function() {
      paste("comparison_heatmap_reg_", input$corregion_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_comheat(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )
  
  
  # COMPARISON PHEATMAP CELLTYPE 
  
  plot_comheat_cell <- reactive({
    req(input$run_comcell)
    generate_combined_heatmaps(data_harm_scaled, schema_genes, input$comcell_by, "cell_types")
  })
  
  output$comcellplot <- renderPlot({
    plot_comheat_cell()
  })
  
  output$downloadcorregion <- downloadHandler(
    filename = function() {
      paste("comparison_heatmap_reg_", input$comcell_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_comheat_cel(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )
  

  # CONDITIONS TOTAL DOTPLOT  
  
  plot_contotaldot <- reactive({
    req(input$run_total_condotplot)
    age <- NULL
    sex <- NULL
    
    if (input$total_filter_by == "Sex") {
      sex <- input$total_sex  # Asigna el sexo seleccionado por el usuario
    } else if (input$total_filter_by == "Age") {
      age <- input$total_age # Asigna la edad seleccionada por el usuario
    } else if (input$total_filter_by == "All") {
      age <- NULL
      sex <- NULL
    }
    
    generate_fold_change_plots(data_harm_scaled, schema_genes, "Condition", "ELS", "SR", NULL, age, sex)
    
  })
  
  output$condottotalPlot <- renderPlot({
    plot_contotaldot()
  })
  
  output$downloadtotalcondotplot <- downloadHandler(
    filename = function() {
      paste("fold_change_plot_", input$gene_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_condot(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )
  
  # CONDITIONS GEN DOTPLOT  
  
  plot_condot <- reactive({
    req(input$run_condotplot)
    age <- NULL
    sex <- NULL
    
    if (input$filter_by == "Sex") {
      sex <- input$sex  # Asigna el sexo seleccionado por el usuario
    } else if (input$filter_by == "Age") {
      age <- input$age # Asigna la edad seleccionada por el usuario
    } else if (input$filter_by == "All") {
      age <- NULL
      sex <- NULL
    }
    
    generate_fold_change_plots(data_harm_scaled, NULL, "Condition", "ELS", "SR", input$gene_by, age, sex)
    
  })
  
  output$condotPlot <- renderPlot({
    plot_condot()
  })
  
  output$downloadcondotplot <- downloadHandler(
    filename = function() {
      paste("fold_change_plot_", input$gene_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_condot(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )
  
  # SEX TOTAL DOTPLOT
  plot_sextotaldot <- reactive({
    req(input$run_total_sexdotplot)
    age <- NULL
    condition <- NULL
    
    if (input$total_filter_by == "Condition") {
      condition <- input$total_condition  # Asigna el sexo seleccionado por el usuario
    } else if (input$total_filter_by == "Age") {
      age <- input$total_age # Asigna la edad seleccionada por el usuario
    } else if (input$total_filter_by == "All") {
      condition <- NULL
      age <- NULL
    }
    
    generate_fold_change_plots(data_harm_scaled, schema_genes, "Sex", "F", "M", NULL, age, NULL, condition)
    
  })
  
  output$sexdottotalPlot <- renderPlot({
    plot_sextotaldot()
  })
  
  output$downloadtotalsexdotplot <- downloadHandler(
    filename = function() {
      paste("fold_change_plot_", input$gene_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_condot(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )
  # SEX GEN DOTPLOT  
  
  plot_sexdot <- reactive({
    req(input$run_sexdotplot)
    age <- NULL
    condition <- NULL
    
    if (input$filter_by == "Age") {
      age <- input$age  # Asigna la edad seleccionada por el usuario
      if (input$additional_filter_age != "None") {
        condition <- input$additional_filter_age  # Asigna la condición adicional seleccionada por el usuario
      }
    } else if (input$filter_by == "Condition") {
      condition <- input$condition  # Asigna la condición seleccionada por el usuario
      if (input$additional_filter_condition != "None") {
        age <- input$additional_filter_condition  # Asigna la edad adicional seleccionada por el usuario
      }
    }

    generate_fold_change_plots(data_harm_scaled, NULL, "Sex", "F", "M", input$gene_by, age, NULL, condition)
  })
  
  output$sexdotPlot <- renderPlot({
    plot_sexdot()
  })
  
  output$downloadsexdotplot <- downloadHandler(
    filename = function() {
      paste("fold_change_plot_", input$gene_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_sexdot(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )

#   AGE TOTAL DOTPLOT   
  
  plot_agetotaldot <- reactive({
    req(input$run_total_agedotplot)
    condition <- NULL
    sex <- NULL
    
    if (input$total_filter_by == "Condition") {
      condition <- input$total_condition  # Asigna el sexo seleccionado por el usuario
    } else if (input$total_filter_by == "Sex") {
      sex <- input$total_sex # Asigna la edad seleccionada por el usuario
    } else if (input$total_filter_by == "All") {
      condition <- NULL
      age <- NULL
    }
    
    generate_fold_change_plots(data_harm_scaled, schema_genes, "Age", input$age_group_1_by, input$age_group_2_by, NULL, NULL, sex, condition)
    
  })
  
  output$agedottotalPlot <- renderPlot({
    plot_agetotaldot()
  })
  
  output$downloadtotalagedotplot <- downloadHandler(
    filename = function() {
      paste("fold_change_plot_", input$gene_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_condot(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )
#   AGE GEN DOTPLOT  
  
  plot_agedot <- reactive({
    req(input$run_agedotplot)
    sex <- NULL
    condition <- NULL
    
    if (input$filter_by == "Sex") {
      sex <- input$sex  # Asigna la edad seleccionada por el usuario
      if (input$additional_filter_age != "None") {
        condition <- input$additional_filter_age  # Asigna la condición adicional seleccionada por el usuario
      }
    } else if (input$filter_by == "Condition") {
      condition <- input$condition  # Asigna la condición seleccionada por el usuario
      if (input$additional_filter_condition != "None") {
        sex <- input$additional_filter_condition  # Asigna la edad adicional seleccionada por el usuario
      }
    }
    
    generate_fold_change_plots(data_harm_scaled, NULL, "Age", input$group_1_by, input$group_2_by, input$gene_by, NULL, sex, condition)
    
  })
  
  output$agedotPlot <- renderPlot({
    plot_agedot()
  })
  
  output$downloadagedotplot <- downloadHandler(
    filename = function() {
      paste("fold_change_plot_", input$gene_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_agedot(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )
  
  # DISPLOT 
  
  plot_dispersion <- reactive({
    req(input$run_disp)
    plot_gene_expression(data_harm_scaled, input$gene_by, input$col_by)
  })
  
  output$displot <- renderPlot({
    plot_dispersion ()
  })
  
  output$downloadDispPlot <- downloadHandler(
    filename = function() {
      paste("displot_", input$gene_by, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_dispersion(), width = 14, height = 7, units = "in", dpi = 300)
    }
  )


# AREA HAE PERC 

plot_area <- reactive({
  req(input$run_area)
  generate_heatmap_area(data, input$region_by, input$con_by, input$gene_by, input$col_by, "Percent")
})

output$percheat <- renderPlot({
  plot_area ()
})

output$downloadAreaPlot <- downloadHandler(
  filename = function() {
    paste("area_", input$gene_by, ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot_area(), width = 14, height = 7, units = "in", dpi = 300)
  }
)

# AREA HAE PUNCTA

plot_area_pun <- reactive({
  req(input$run_area)
  generate_heatmap_area(data, input$region_by, input$con_by, input$gene_by, input$col_by, "Puncta")
})

output$punctheat <- renderPlot({
  plot_area_pun ()
})

output$downloadAreaPlot <- downloadHandler(
  filename = function() {
    paste("area_", input$gene_by, ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot_area(), width = 14, height = 7, units = "in", dpi = 300)
  }
)
}


shinyApp(ui, server)

