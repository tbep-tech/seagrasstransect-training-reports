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
  
  # css styling
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css')
  ),

  # Sidebar with controls
  sidebar = sidebar(
    title = "Controls",
    width = 600,
    
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
      p(HTML("Contact <a href='mailto:sscolaro@tbep.org' target='_blank'>Sheila Scolaro</a> or <a href='mailto:mbeck@tbep.org' target='_blank'>Marcus Beck</a> for more information. View the <a href='https://github.com/tbep-tech/seagrasstransect-training-reports' target='_blank'>source code</a> on GitHub."))
    )
    
  ),
  
  # Main content area
  div(
    style = "height: calc(100vh - 100px); overflow-y: auto; padding: 1rem; background-color: white;",
    shinycssloaders::withSpinner(
      plotlyOutput(
        outputId = "score_plot",
        height = "1000px"
      ),
      type = 6, 
      color = "#2E86AB"  
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