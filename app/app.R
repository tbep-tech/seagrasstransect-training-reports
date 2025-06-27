library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(munsell)

# Source your functions (uncomment when using with your actual setup)
source('R/funcs.R')

# Load your data (uncomment when using with your actual setup)
data(allyrscrs)

# Get unique groups
grps <- sort(unique(allyrscrs$grp))
             
# Define UI
ui <- page_sidebar(
  title = div(
    style = "display: flex; align-items: center; gap: 15px;",
    img(src = "tarponlogo.png", 
        height = "40px", 
        style = "max-height: 40px; width: auto;"),
    span("Tampa Bay Interagency Seagrass Monitoring Program: Intercalibration Assessment",
         style = "font-family: 'Rubik', sans-serif; font-weight: 600; color: #005293;")
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E86AB",
    secondary = "#A23B72"
  ),
  
  # Custom CSS for TBEP branding and styling
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Rubik:wght@500;600;700&display=swap", rel = "stylesheet"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      /* TBEP Authentic Color Scheme */
      :root {
        --tbep-primary: #005293;
        --tbep-teal: #00806E;
        --tbep-orange: #db5b25;
        --tbep-orange-hover: #f57d05;
        --tbep-yellow: #F0AD4E;
        --tbep-yellow-active: #ea6f17;
        --tbep-gray: #636363;
        --tbep-light-gray: #958984;
        --tbep-bg-gray: #95898430;
      }
      
      /* TBEP Typography */
      body {
        font-family: 'Roboto', sans-serif;
        font-weight: 400;
        font-size: 18px;
        color: var(--tbep-gray);
      }
      
      h1, h2, h3, h4, h5, h6 {
        font-weight: 500;
        color: var(--tbep-primary);
        font-family: 'Rubik', Helvetica, Arial, Lucida, sans-serif;
      }
      
      h1 { font-size: 40px; }
      h2 { font-size: 32px; }
      h3 { font-size: 24px; }
      h4 { font-size: 18px; }
      h5 { font-size: 16px; }
      h6 { font-size: 14px; }
      
      /* Links */
      a {
        color: var(--tbep-orange);
        font-family: 'Roboto', Helvetica, Arial, Lucida, sans-serif;
        font-weight: 500;
        text-decoration: none;
      }
      
      a:visited {
        color: var(--tbep-orange);
      }
      
      a:hover {
        color: var(--tbep-orange-hover);
      }
      
      /* Sidebar styling */
      .bslib-sidebar-layout > .sidebar {
        background-color: var(--tbep-bg-gray);
        color: var(--tbep-light-gray);
        border-radius: 8px;
        font-family: 'Roboto', sans-serif;
        font-weight: 300;
      }
      
      .bslib-sidebar-layout > .sidebar h4 {
        color: var(--tbep-primary);
        font-family: 'Rubik', Helvetica, Arial, Lucida, sans-serif;
        font-weight: 500;
      }
      
      .bslib-sidebar-layout > .sidebar h6 {
        color: var(--tbep-light-gray);
        font-family: 'Roboto', sans-serif;
        font-weight: 300;
      }
      
      /* Form controls */
      .bslib-sidebar-layout > .sidebar .form-control {
        font-family: 'Roboto', sans-serif;
        font-weight: 300;
        color: var(--tbep-gray);
      }
      
      .bslib-sidebar-layout > .sidebar .control-label {
        font-family: 'Roboto', sans-serif;
        font-weight: 300;
        color: var(--tbep-light-gray);
      }
      
      /* Select dropdown styling */
      .selectize-dropdown .option.selected {
        background: var(--tbep-teal) !important;
      }
      
      /* Button styling */
      .btn-outline-secondary {
        color: var(--tbep-teal);
        border-color: var(--tbep-teal);
        font-family: 'Roboto', sans-serif;
        font-weight: 300;
      }
      
      .btn-outline-secondary:hover {
        background-color: var(--tbep-teal);
        border-color: var(--tbep-teal);
        color: white;
      }
      
      /* Main content styling */
      .main h3 {
        color: var(--tbep-primary);
        font-family: 'Rubik', Helvetica, Arial, Lucida, sans-serif;
        font-weight: 500;
      }
      
      .lead {
        color: var(--tbep-gray);
        font-family: 'Roboto', sans-serif;
        font-weight: 300;
      }
      
      /* Plot container */
      .plot-container {
        background: white;
        border: 1px solid #E5E5E5;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
      }
      
      /* Custom scrollbar to match TBEP colors */
      .plot-container::-webkit-scrollbar {
        width: 8px;
      }
      
      .plot-container::-webkit-scrollbar-track {
        background: #E5E5E5;
        border-radius: 4px;
      }
      
      .plot-container::-webkit-scrollbar-thumb {
        background: var(--tbep-teal);
        border-radius: 4px;
      }
      
      .plot-container::-webkit-scrollbar-thumb:hover {
        background: var(--tbep-primary);
      }
      
      /* Text colors */
      .text-primary {
        color: var(--tbep-primary) !important;
      }
      
      .text-muted {
        color: var(--tbep-light-gray) !important;
      }
      
      /* Small text styling */
      .small {
        font-family: 'Roboto', sans-serif;
        font-weight: 300;
        color: var(--tbep-light-gray);
      }
    "))
    
    ),
    
  # Sidebar with controls
  sidebar = sidebar(
    title = "Controls",
    width = 500,
    
    selectInput(
      inputId = "group_select",
      label = "Select Training Group:",
      choices = grps,
      selected = grps[1]
    ),
    
    hr(),
    
    h6("Group Navigation:"),
    div(
      class = "d-grid gap-2",
      actionButton(
        inputId = "prev_group",
        label = "Previous Group",
        class = "btn-outline-secondary btn-sm"
      ),
      actionButton(
        inputId = "next_group",
        label = "Next Group",
        class = "btn-outline-secondary btn-sm"
      )
    ),
    
    hr(),
    
    div(
      class = "small text-muted",
      p("Use the dropdown or navigation buttons to iterate through different training groups."),
      p("The plot shows score trends over years with a fitted trend line."), 
      p(HTML("Contact <a href='mailto:sscolaro@tbep.org' target='_blank'>Sheila Scolaro</a> or <a href='mailto:mbeck@tbep.org' target='_blank'>Marcus Beck</a> for more information."))
    )
    
  ),
  
  # Main content area
  div(
    style = "height: calc(100vh - 100px); overflow-y: auto; border: 1px solid #dee2e6; border-radius: 0.375rem; padding: 1rem; background-color: white;",
    plotlyOutput(
      outputId = "score_plot",
      height = "900px"
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to track current group index
  current_group_index <- reactiveVal(1)
  
  # Update current group index when dropdown changes
  observeEvent(input$group_select, {
    new_index <- which(grps == input$group_select)
    current_group_index(new_index)
  })
  
  # Handle previous group button
  observeEvent(input$prev_group, {
    current_index <- current_group_index()
    new_index <- if(current_index > 1) current_index - 1 else length(grps)
    current_group_index(new_index)
    updateSelectInput(session, "group_select", selected = grps[new_index])
  })
  
  # Handle next group button
  observeEvent(input$next_group, {
    current_index <- current_group_index()
    new_index <- if(current_index < length(grps)) current_index + 1 else 1
    current_group_index(new_index)
    updateSelectInput(session, "group_select", selected = grps[new_index])
  })
  
  # Generate plot
  output$score_plot <- renderPlotly({
    req(input$group_select)
    allyrscrplo_fun(allyrscrs, input$group_select)
  })
}

# Run the application
shinyApp(ui = ui, server = server)