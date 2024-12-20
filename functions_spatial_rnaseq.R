library(corrplot)
library(umap)
library(ggplot2)
library(dplyr)
library(reshape)
library(GGally)
library(corrr)
library(tidyr)
library(data.table)
library(gridExtra)
library(pheatmap)
library(purrr)
library(grid)
library(Giotto)
library(dplyr)
library(tibble)
library(ggrepel)

#setwd('/home/shey/Documentos/neuro/Spatial_Transcriptomic')
#load("2024-04-29_df_noNorm-allages-SRP21fix.RData")

#######################
#LOAD DATA
#######################
load("variables_schema_10.RData")
data_harm_raw = read.csv('2024-08-14data_raw_harm_xshift.csv')
data_harm_scaled = read.csv('2024-08-14data_scaled_harm_xshift.csv')
schema_genes = c("Cacna1g","Cul1","Gria3","Grin2a","Herc1","Rb1cc1","Setd1a","Sp4","Trio","Xpo7")
marker_genes = c("Chat","Cx3cr1","Olig1","Olig2","PV","S100b","Slc32a1","Th","Vglut1","Vglut2")
datasubset = data_harm_raw
regions = unique(datasubset$Region)
data_regions = regions[!regions == ""]
data = read.csv('2024-11-20-data.csv')

#data area
data$SubregionNoLayer = gsub(",\\s*([Ll]ayer|[Ll]ayers)\\s+\\d+[a-zA-Z]?(/\\d+[a-zA-Z]?)?$", "", data$Subregion)
data$SubregionNoLayer = gsub(data$SubregionNoLayer,pattern = ", 6a",replacement = "")
data$SubregionNoLayer = gsub(data$SubregionNoLayer,pattern = ", 6b",replacement = "")
regions = regions[!regions == ""]

area_classification <- data.frame(
  SubregionNoLayer = c(
    "Primary motor area", "Secondary motor area", "Primary somatosensory area, lower limb",
    "Primary somatosensory area, trunk", "Primary somatosensory area, upper limb",
    "Primary visual area", "Anteromedial visual area", "posteromedial visual area"
  ),
  AreaType = "Motor and Sensory Area"
)
additional_areas <- data.frame(
  SubregionNoLayer = c("Frontal pole", "Orbital area, lateral part", "Agranular insular area, dorsal part"
                       , "Endopiriform nucleus, dorsal part", "Anterior area", "Infralimbic area"
                       , "Retrosplenial area, lateral agranular part", "Retrosplenial area, dorsal part"
                       , "Retrosplenial area, ventral part", "Claustrum", "Agranular insular area, ventral part"
                       , "Cortical subplate", "Posterior amygdalar nucleus", "Hippocampo-amygdalar transition area"
                       , "Prelimbic area", "Orbital area, medial part", "Orbital area, ventrolateral part"
                       , "Anterior cingulate area, dorsal part", "Anterior cingulate area, ventral part"),
  AreaType = "Limbic and Higher Cognitive Area"
)
area_classification <- rbind(area_classification, additional_areas)


#######################
#HEATMAP DE CORRELACION
#######################

generate_correlations<- function(data, condition, group_by) {
  # Filtrar los datos según la condición
  df_condition <- data[data$Condition == condition, ]
  subset <- df_condition[, c("Sex", "Age", "Condition", "Chat", "Fos", "Npas4", "Vglut2", "Sp4", "Setd1a", "Cul1", "Trio", "Cacna1g", "Vglut1", "Herc1", "Xpo7", "Rb1cc1", "Grin2a", "Gphn", "Gria3", "Th", "PV", "Slc32a1", "S100b", "Olig1", "Olig2", "C4b", "Cx3cr1")]
  
  # Dividir los datos según el grupo especificado (Age o Sex)
  groups <- split(subset, subset[[group_by]])
  
  # Calcular las matrices de correlación para cada grupo
  corr_list <- lapply(groups, function(group) cor(group[, 4:27]))
  titles <- names(groups)
  
  # Crear la función de plot y devolverla
  plot_function <- function() {
    par(mfrow = c(1, length(corr_list)), mar = c(2, 1, 3, 1) + 0.1)
    for (i in 1:length(corr_list)) {
      corrplot(corr_list[[i]], order = 'original', method = 'circle')
      title(main = paste(titles[i]), line = 2, cex.main = 1.5)
    }
    par(mfrow = c(1, 1))
  }
  
  return(plot_function)
}
#generate_correlations(data_harm_raw, "SR", "Age")()

#####################
#BARRAS DE CORELACION 
#####################


generate_plots <- function(data, condition, group_by_var, region = NULL) {
  # Filtrar datos por condición
  filtered_data <- data[data$Condition == condition, ]
  
  # Filtrar por región si se especifica
  if (!is.null(region) && region != "") {
    filtered_data <- filtered_data[filtered_data$Region == region, ]
  }
  
  # Seleccionar el subconjunto de datos
  subset <- filtered_data[, c("Sex", "Age", "Condition", "Region", "Chat", "Fos", "Npas4", "Vglut2", "Sp4", "Setd1a", "Cul1", "Trio", "Cacna1g", "Vglut1", "Herc1", "Xpo7", "Rb1cc1", "Grin2a", "Gphn", "Gria3", "Th", "PV", "Slc32a1", "S100b", "Olig1", "Olig2", "C4b", "Cx3cr1")]
  
  # Calcular las correlaciones
  cor_results <- sapply(subset[, -c(1:4)], function(gene) cor(subset$Age, gene, use="complete.obs"))
  
  # Crear un DataFrame para el gráfico de correlación
  cor_df <- data.frame(Gene = names(cor_results), Correlation = cor_results)
  # Ordenar el DataFrame por la correlación
  cor_df <- cor_df[order(cor_df$Correlation), ]
  gene_order <- cor_df$Gene
  
  # Convertir el subconjunto a data.table
  dt_genes <- as.data.table(subset)
  
  # Calcular la expresión media por gen y grupo
  summary_data <- dt_genes[, lapply(.SD, mean, na.rm = TRUE), by = group_by_var, .SDcols = gene_order]
  
  # Transformar los datos resumidos a formato largo
  long_summary_data <- melt(summary_data, id.vars = group_by_var, variable.name = "Gene", value.name = "Expression")
  long_summary_data$Gene <- factor(long_summary_data$Gene, levels = gene_order)
  
  # Crear los gráficos
  plot1 <- ggplot(cor_df, aes(x = Correlation, y = factor(Gene, levels = gene_order))) +
    geom_bar(stat = "identity") +
    labs(title = paste("Correlation between Gene Expression and Age (", condition, " Condition)", sep = ""), x = "Correlation", y = "Gene") +
    theme_minimal()
  
  plot2 <- ggplot(long_summary_data, aes(x = Expression, y = factor(Gene, levels = gene_order), fill = as.factor(get(group_by_var)))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Gene Expression Levels at Different ", group_by_var, "s", sep = ""), x = "Expression Level", y = "Gene") +
    theme_minimal() +
    scale_fill_manual(values = c("red", "green", "blue", "purple"), name = group_by_var)
  
  combined_plot <- grid.arrange(plot1, plot2, ncol = 2)
  
  return(combined_plot)
}

#generate_plots(data_harm_raw, "SR", "Sex")

#############
### PHEATMAP
#############
generate_heatmap <- function(datasubset, schema_genes, group_var, group_value, heatmap_var) {
  groups <- unique(datasubset[[heatmap_var]])
  groups <- groups[groups != ""]
  
  corr.heatmap <- matrix(nrow = length(schema_genes), ncol = length(groups))
  rownames(corr.heatmap) <- schema_genes
  colnames(corr.heatmap) <- groups
  
  for (gene in schema_genes) {
    for (group in groups) {
      ELS <- datasubset %>% filter(!!sym(heatmap_var) == group, Condition == "ELS") %>% pull(!!sym(gene))
      SR <- datasubset %>% filter(!!sym(heatmap_var) == group, Condition == "SR") %>% pull(!!sym(gene))
      corr.heatmap[gene, group] <- mean(ELS, na.rm = TRUE) / mean(SR, na.rm = TRUE)
    }
  }
  
  heatmap_plot <- pheatmap(corr.heatmap, silent = TRUE, main = paste(group_var, ": ", group_value, sep = ""))
  return(heatmap_plot[[4]])  # Devuelve el objeto 'gtable' del plot
}

# Función principal para automatizar la generación de heatmaps
generate_combined_heatmaps <- function(data, schema_genes, group_by_var, heatmap_var) {
  plots_list <- list()
  
  groups <- unique(data[[group_by_var]])
  grid_dims <- if (group_by_var == "Age") c(2, 2) else c(1, 2)  # 2x2 para edades, 1x2 para sexos
  
  for (group in groups) {
    datasubset <- data %>% filter(!!sym(group_by_var) == group)
    print(names(datasubset))
    heatmap <- generate_heatmap(datasubset, schema_genes, group_by_var, group, heatmap_var)
    plots_list[[as.character(group)]] <- heatmap  # El objeto 'gtable' del plot
  }
  
  combined_heatmaps <- do.call(grid.arrange, c(plots_list, ncol = grid_dims[2], nrow = grid_dims[1]))
  return(combined_heatmaps)
}
#generate_combined_heatmaps(data_harm_scaled, schema_genes, "Sex", "cell_types")



#############
###DOTPLOTS
############

generate_fold_change_plots <- function(data, genes = NULL, comparison_type, value1, value2, gene_specific = NULL, age = NULL, sex = NULL, condition = NULL) {
  
  # Filtrar los datos según los parámetros especificados
  filtered_data <- data %>%
    filter(get(comparison_type) %in% c(value1, value2)) %>%
    filter(if (!is.null(age)) Age == age else TRUE) %>%
    filter(if (!is.null(sex)) Sex == sex else TRUE) %>%
    filter(if (!is.null(condition)) Condition == condition else TRUE) %>%
    filter(Region != "")
  
  # Función auxiliar para calcular fold change y valor p
  calculate_fold_change <- function(data, gene, group_vars) {
    data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        mean_gene_value1 = mean(get(gene)[get(comparison_type) == value1], na.rm = TRUE),
        mean_gene_value2 = mean(get(gene)[get(comparison_type) == value2], na.rm = TRUE),
        p_value = tryCatch({
          if (n_distinct(get(comparison_type)) == 2 && n() >= 2) {
            t.test(get(gene) ~ get(comparison_type))$p.value
          } else {
            NA
          }
        }, error = function(e) NA),
        .groups = 'drop'
      ) %>%
      mutate(
        p_adj = p.adjust(p_value, method = "BH"),
        fold_change = ifelse(!is.na(mean_gene_value1) & !is.na(mean_gene_value2) & mean_gene_value2 != 0,
                             mean_gene_value1 / mean_gene_value2,
                             NA),
        Gene = gene,
        group_var_value = interaction(across(all_of(group_vars)))
      )
  }
  
  # Función auxiliar para crear el gráfico
  create_dot_grid_plot <- function(data, x_var, y_var, plot_title, x_label, y_label) {
    ggplot(data, aes_string(x = x_var, y = y_var, size = '-log10(p_adj)', color = 'fold_change')) +
      geom_point() + 
      scale_size_area() +                    
      scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 1, name = "Fold Change") +
      theme_minimal() +
      labs(title = plot_title, x = x_label, y = y_label, size = "-log10(p-adj)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Crear el título del gráfico
  base_plot_title <- paste0(" Expression (", value1, "/", value2, ")")
  if (!is.null(age)) base_plot_title <- paste0(base_plot_title, " for Age ", age)
  if (!is.null(sex)) base_plot_title <- paste0(base_plot_title, " for Sex ", sex)
  if (!is.null(condition)) base_plot_title <- paste0(base_plot_title, " for Condition ", condition)
  
  #Diccionario para titulos 
  group_var_labels <- c("cell_types" = "Cell Types", "Region" = "Region")
  
  # Generar gráficos
  plots <- list()
  
  if (!is.null(genes)) {
    for (group_var in c("cell_types", "Region")) {
      fold_change_data <- bind_rows(lapply(genes, function(gene) calculate_fold_change(filtered_data, gene, group_var)))
      plot_title <- paste0("SCHEMA Gene", base_plot_title, " by ", group_var_labels[group_var])
      plot <- create_dot_grid_plot(fold_change_data, "group_var_value", "Gene", plot_title, group_var_labels[group_var], "Genes")
      plots <- c(plots, list(plot))
    }
    combined_plot <- grid.arrange(grobs = plots, ncol = 2)
    print(combined_plot)
  }
  
  if (!is.null(gene_specific)) {
    fold_change_data <- calculate_fold_change(filtered_data, gene_specific, c("Region", "cell_types"))
    plot <- create_dot_grid_plot(fold_change_data, "cell_types", "Region", 
                                 paste0(gene_specific, base_plot_title), "Cell Types", "Regions")
    print(plot)
  }
}
#generate_fold_change_plots(data_harm_scaled, genes = NULL, comparison_type = "Age", value1 = "90", value2 = "10", gene_specific = 'Sp4', NULL, "F")

#############
#DISPERSION
#############

##funcion automatizada
plot_gene_expression <- function(data, gene, color_by = "foldchange") {
  
  regions <- data %>% 
    filter(Condition == "SR") %>%
    group_by(Subregion) %>%
    summarize(mean = mean(get(gene), na.rm = TRUE), n = n()) %>%
    arrange(desc(mean))
  
  # Filtrar y agrupar datos para condición ELS
  ELSregions <- data %>% 
    filter(Condition == "ELS") %>%
    group_by(Subregion) %>%
    summarize(mean = mean(get(gene), na.rm = TRUE), n = n()) %>%
    arrange(desc(mean))
  
  # Unir los datos de las dos condiciones
  merged <- merge(x = regions %>% arrange(Subregion),
                  y = ELSregions %>% arrange(Subregion),
                  by = "Subregion", suffixes = c(".SR", ".ELS")) %>%
    filter(n.SR > 5, n.ELS > 5)
  
  # Calcular fold change
  merged <- merged %>% mutate(foldchange = mean.ELS / mean.SR)
  
  # Añadir información de la región
  merged$Region <- data$Region[match(x = merged$Subregion, table = data$Subregion)]
  
  # Crear el plot
  plot <- merged %>% ggplot(aes(x = mean.SR, y = mean.ELS, color = !!sym(color_by))) + 
    geom_point() + 
    geom_text_repel(aes(label = Subregion), max.overlaps = 30) +
    xlab(paste0("SR ", gene, " Expression")) + 
    ylab(paste0("ELS ", gene, " Expression")) + 
    theme_minimal() + 
    geom_abline(intercept = 0, slope = 1)
  
  # Añadir la escala de colores correcta
  if (color_by == "foldchange") {
    plot <- plot + scale_color_viridis()
  }
  return(plot)
}


#plot_gene_expression(data_harm_scaled, "Npas4", "Region")

###########################
#AREAS CLASIFICATION HEATMAP
############################

generate_heatmap_area <- function(data, region, condition, gene, group_by_var, plot_type = "Percent") {
  
  # Filter
  filtered_data <- data %>%
    filter(Region == region, Condition == condition) %>%
    left_join(area_classification, by = "SubregionNoLayer") %>%
    mutate(AreaType = ifelse(is.na(AreaType), "Other", AreaType))
  
  # clasification
  prepare_summary <- function(data, group_by_var, gene, type) {
    if (type == "Percent") {
      data %>%
        group_by(SubregionNoLayer, !!sym(group_by_var)) %>%
        summarise(Value = sum(get(paste0(gene, "_Pos"))) / n() * 100, .groups = "drop")
    } else {
      data %>%
        group_by(SubregionNoLayer, !!sym(group_by_var)) %>%
        summarise(Value = mean(get(gene)), .groups = "drop")
    }
  }
  
  # create heatmap
  create_heatmap <- function(temp_data, title_suffix) {
    mat <- temp_data %>%
      rename_with(~ "GroupVar", .cols = !!sym(group_by_var)) %>%
      pivot_wider(names_from = GroupVar, values_from = Value, values_fill = 0) %>%
      column_to_rownames("SubregionNoLayer") %>%
      as.matrix()
    
    # annotate and order
    anno <- data.frame(AreaType = filtered_data$AreaType[match(rownames(mat), filtered_data$SubregionNoLayer)])
    rownames(anno) <- rownames(mat)
    order_index <- order(anno$AreaType)
    mat <- mat[order_index, ]
    anno <- anno[order_index, , drop = FALSE]
    
    # colors
    unique_area_types <- unique(anno$AreaType)
    #area_type_colors <- setNames(rainbow(length(unique_area_types)), unique_area_types)
    area_type_colors <- setNames(c("red", "blue", "gray")[seq_along(unique_area_types)], unique_area_types)
    # heatmap
    p <- pheatmap(
      mat,
      cluster_cols = FALSE,
      annotation_row = anno,
      annotation_colors = list(AreaType = area_type_colors),
      main = paste(gene, title_suffix, "by", group_by_var),
      cluster_rows = FALSE,
      angle_col = 0
    )
    
    return(p$gtable)
  }
  
  # Generate
  if (plot_type == "Percent") {
    temp <- prepare_summary(filtered_data, group_by_var, gene, "Percent")
    create_heatmap(temp, "Percent")
    
  } else if (plot_type == "Puncta") {
    temp_all <- prepare_summary(filtered_data, group_by_var, gene, "Puncta")
    temp_pos <- prepare_summary(filtered_data %>% filter(get(paste0(gene, "_Pos")) == 1), group_by_var, gene, "Puncta")
    heatmap_all <- create_heatmap(temp_all, "Puncta - All Cells")
    heatmap_pos <- create_heatmap(temp_pos, "Puncta - Positive Cells")
    grid.arrange(heatmap_all, heatmap_pos, ncol = 2)
  }
}
#generate_heatmap_area(data, "Cortex", condition = "SR", gene = "Gria3", group_by_var = "Age", plot_type = "Percent")
