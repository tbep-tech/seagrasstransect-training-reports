#' Create subplots of scores across years for a given group
#' 
#' @param allyrscrs data frame of all group scores across years
#' @param grp character, group to plot
allyrscrplo_fun <- function(allyrscrs, grpsel){
  
  toplo <- allyrscrs |> 
    dplyr::filter(grp %in% !!grpsel)
  
  # Define grade breaks and colors using your specific grading system
  grades <- c('A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D')
  grdbrk <- c(110, 95, 90, 85, 80, 75, 70, 65, 60, 55, 40)
  
  # Convert 101 to max value and 0 to min value for plotly
  grade_breaks <- grdbrk
  max_score <- max(toplo$scr, na.rm = TRUE)
  min_score <- min(toplo$scr, na.rm = TRUE)
  # Create red to green gradient colors (with transparency)
  grade_colors <- c("rgba(0,255,0,0.3)", "rgba(51,255,0,0.3)", "rgba(102,255,0,0.3)", 
                    "rgba(153,255,0,0.3)", "rgba(204,255,0,0.3)", "rgba(255,255,0,0.3)",
                    "rgba(255,204,0,0.3)", "rgba(255,153,0,0.3)", "rgba(255,102,0,0.3)", "rgba(255,0,0,0.3)")
  
  # Get unique variables for faceting
  vars <- unique(toplo$var)
  
  # Create subplot function
  create_subplot <- function(var_name, data) {
    
    # Filter data for this variable
    plot_data <- data[data$var == var_name, ]
    
    # Create base plot
    p <- plotly::plot_ly()
    
    # Add background rectangles for each grade
    for(i in 1:length(grades)) {
      p <- p |> 
        plotly::add_ribbons(
          x = c(min(plot_data$yr) - 10, max(plot_data$yr) + 10),
          ymin = grade_breaks[i+1], 
          ymax = grade_breaks[i],
          fillcolor = grade_colors[i],
          line = list(color = "transparent"),
          showlegend = FALSE,
          hoverinfo = "text",
          text = paste("Grade:", grades[i])
        )
    }
    
    # Add trend line (linear regression)
    lm_model <- lm(scr ~ yr, data = plot_data)
    trend_line <- predict(lm_model, newdata = data.frame(yr = plot_data$yr))
    
    p <- p |>
      plotly::add_lines(
        data = plot_data,
        x = ~yr, 
        y = trend_line,
        line = list(color = "#1f77b4", dash = "solid"),
        name = "Trend",
        showlegend = FALSE
      ) |>
      plotly::add_markers(
        data = plot_data,
        x = ~yr, 
        y = ~round(scr, 1),
        marker = list(color = "#1f77b4"),
        showlegend = FALSE,
        name = "Score",
      ) #|>
    # plotly::add_lines(
    #   data = plot_data,
    #   x = ~yr, 
    #   y = ~scr,
    #   line = list(color = "#1f77b4", dash = "dot"),
    #   name = "Data Line",
    #   showlegend = FALSE
    # )
    
    # Add grade labels on right y-axis
    grade_positions <- c(97.5, 92.5, 87.5, 82.5, 77.5, 72.5, 67.5, 62.5, 57.5, 52.5)
    
    # Create annotations for grade labels
    annotations <- list()
    for(i in 1:length(grades)) {
      annotations[[i]] <- list(
        x = max(allyrscrs$yr) + 0.1,
        y = grade_positions[i],
        text = grades[i],
        xref = "x",
        yref = "y",
        xanchor = "left",
        showarrow = FALSE,
        font = list(size = 10, color = "black")
      )
    }
    
    p <- p |>
      plotly::layout(
        xaxis = list(
          title = '',
          range = c(min(allyrscrs$yr) - 0.1, max(allyrscrs$yr) + 0.2), 
          dtick = 1, 
          tickmode = 'linear'
        ),
        yaxis = list(
          title = paste(var_name, "Score"),
          side = "left", 
          range = c(50, 100)
        ),
        annotations = annotations
      )
    
    return(p)
  }
  
  # Create individual plots for each variable
  plot_list <- lapply(vars, function(var) create_subplot(var, toplo))
  
  # Combine into subplots (vertical arrangement)
  out <- plotly::subplot(
    plot_list, 
    nrows = length(vars), 
    shareX = TRUE,
    titleY = TRUE,
    heights = rep(1/length(vars), length(vars))
  )
  
  return(out)
  
}