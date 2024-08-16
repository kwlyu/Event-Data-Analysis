# Load A BUNCH of packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(tidyverse)
library(reactable)
library(reactablefmtr)
library(shinycssloaders)
library(lubridate)
library(ggplot2)
library(shinyjs)
library(viridis)
library(viridisLite)
library(purrr)
library(stringr)
library(forcats)
library(gganimate)
library(ggthemes)
library(leaflet)
library(patchwork)
library(probably)
library(readr)
library(janitor)
library(shinyalert)
library(shinyWidgets)
library(tidyr)
library(tidymodels)
library(yardstick)
library(plotly)
library(hardhat)
library(vip)
library(rpart.plot)
library(ranger)
library(broom)
library(googlesheets4)
remotes::install_github("timelyportfolio/dataui")
gs4_auth(email = "lyuk@carleton.edu", cache = ".secrets")

################################ DATA WRANGLING ################################

# Define the Google Sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1a0wHpBMmUMoeKrTK23nHcYvFpQ2djmcYKmjJqEJWX1I/edit#gid=265403245"

# Get the list of sheet names from the Google Sheet
sheet_names_list <- sheet_names(sheet_url)

# Extract the terms from the sheet names dynamically
extracted_terms <- sheet_names_list %>%
  str_extract("^[A-Z]\\d{2}") %>%
  na.omit()  # Remove any NA values in case some sheet names do not follow the pattern

# Ensure unique and sorted terms
unique_terms <- sort(unique(extracted_terms))

# Create a function to read and clean a sheet
read_and_clean_event_file <- function(sheet_name) {
  # Read the sheet using googlesheets4
  data <- read_sheet(sheet_url, sheet = sheet_name)
  
  # Clean column names and ensure all columns are character type
  clean_names(data) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(term = str_sub(sheet_name, 1, 3)) # Extract the term from the sheet name
}

# Use purrr to read all sheets and store them in a named list
event_data_list <- set_names(map(sheet_names_list, read_and_clean_event_file), sheet_names_list)

# Combine all the data frames into one
combined_data <- reduce(event_data_list, full_join)

# Define the term start dates (hardcoded as per the original logic)
term_start_dates <- data.frame(
  term = c("F14", "W15", "S15", 
           "F15", "W16", "S16", 
           "F16", "W17", "S17", 
           "F17", "W18", "S18", 
           "F18", "W19", "S19", 
           "F19", "W20", "S20", 
           "F20", "W21", "S21", 
           "F21", "W22", "S22", 
           "F22", "W23", "S23", 
           "F23", "W24", "S24"),  # Add all relevant terms
  start_date = as.Date(c("2014-09-15", "2015-01-05", "2015-03-30",
                         "2015-09-14", "2016-01-04", "2016-03-28",
                         "2016-09-12", "2017-01-04", "2017-03-27",
                         "2017-09-11", "2018-01-03", "2018-03-26",
                         "2018-09-10", "2019-01-07", "2019-04-01",
                         "2019-09-16", "2020-01-06", "2020-03-30",
                         "2020-09-14", "2021-01-04", "2021-03-29",
                         "2021-09-15", "2022-01-05", "2022-03-28",
                         "2022-09-12", "2023-01-04", "2023-03-27",
                         "2023-09-11", "2024-01-03", "2024-03-25"))  # Adjust start dates accordingly
)

# Function to calculate the week of term
calculate_week_of_term <- function(event_date, term) {
  start_date <- term_start_dates %>%
    filter(term == !!term) %>%
    pull(start_date)
  
  if(length(start_date) == 0) return(NA_integer_)  # Return NA if no start_date is found
  
  # Calculate the week of the term based on Monday as the first day of the week
  week_of_term <- as.integer((floor_date(event_date, unit = "week", week_start = 1) - 
                                floor_date(start_date, unit = "week", week_start = 1)) / 7) + 1
  return(week_of_term)
}

# Apply the new function to calculate week_of_term dynamically
combined_data_filtered <- combined_data %>%
  filter(!is.na(what), what != "") %>%
  filter(what != "Choir & Jazz Rehearsal") %>%
  filter(what != "Jazz Rehearsal") %>%
  mutate(date = as.Date(ymd(date))) %>%
  mutate(support_level = if_else(support_level == "N" | support_level == "Y", "L", support_level)) %>%
  mutate(
    department = str_replace_all(department, "WCC", "ODOA"),
    department = str_replace_all(department, "MSUC", "MUSC"),
    department = str_replace_all(department, "French Dept|French", "FREN"),
    department = str_replace_all(department, "English", "ENGL"),
    department = str_replace_all(department, "Pres. Office", "PRES"),
    department = str_replace_all(department, "History", "HIST"),
    department = str_replace_all(department, "THD", "THDA"),
    department = str_replace_all(department, "Inclusion & Equity", "IEC"),
    venue = str_replace_all(venue, "Skinner Chapel", "Chapel"),
    department = str_replace_all(department, "/", " & "),
    department = str_replace_all(department, ",", " &"),
    venue = str_replace_all(venue, ",", " &"),
    department = str_replace_all(department, "\\s+", " ")  # Remove extra spaces
  ) %>%
  select(-wk) %>%
  mutate(department_type = case_when(
    department == "MUSC" ~ "MUSC",
    department == "ODOA" ~ "ODOA",
    department == "CSA" ~ "CSA",
    str_detect(department, "&") ~ "Collab",
    TRUE ~ "Others"
  )) %>%
  mutate(what = str_replace_all(what, "Jazz Ensemble Concert|Jazz Area Concert", "Jazz Concert"),
         what = str_replace_all(what, "Symphony Band Concert", "Symphony Concert"),
         what = str_replace_all(what, "Composition Recital", "Composition Showcase Recital"),
         what = str_replace_all(what, "Harpichord", "Harpsichord"),
         what = str_replace_all(what, "Emsemble", "Ensemble"),
         what = str_replace_all(what, "Juest Cellin'", "Just Cellin'"),
         what = str_replace_all(what, "Facutly|FACULTY|Mazariello", "Faculty")) %>%
  mutate(event_type = case_when(
    str_detect(what, "GUEST|ODOA|Concert Series|SPCO") ~ "Guest",
    str_detect(what, "Faculty") ~ "Faculty Recital",
    str_detect(what, "Student|Senior|Junior|Piano Recital: |Johnson|Verma Jameson") ~ "Student Recital",
    str_detect(what, "Studio Recital|Organ & Harpsichord|Composition Showcase Recital|Chamber Recital|Chamber Music Recital|Chamber Music|Organ Recital|Strings Recital|Violin & Viola|Violin/Viola|Drum Ensemble|Drum Recital|Voice Showcase Recital|Chinese Music Recital|Piano Studios Recital|Jazz Chamber|Piano Recital|Comps Fest|Recorder Recital|Music Ensemble|Studio") ~ "Studio Recital",
    str_detect(what, "Orchestra Concert|Jazz Concert|Symphony Concert|Symphony Band|Choir Concert|Orchestra and Choir|Chinese & Global|Chinese Global Concert|Chinese and Global|Chinese Music Concert|Chinese Music Ensemble|Chinese Ensemble|Music Comps|Jazz Vocal Concert") ~ "Ensemble Concert",
    str_detect(what, "CSA|Just Cellin|Lunar New Year|ACA|A Cappella|Accidentals|Exit 69|Date Knight|Knights|Knightingales|International Festival") ~ "Student Activity",
    str_detect(what, "Masterclass|Lecture|Symposium") ~ "Masterclass",
    str_detect(what, "Trustees|Trustee's|Presidents|Conference|President's|Presentation") ~ "Presentation",
    str_detect(what, "Clinic|Music Fest|Music Department Showcase|Melinda Russell|Launch|Event|Opening") ~ "Special Events",
    TRUE ~ "Guest"
  )) %>%
  mutate(year = term_to_year(term)) %>%
  mutate(term = factor(term, levels = unique_terms, ordered = TRUE)) %>%
  arrange(year, term) %>%
  mutate(
    term_category = case_when(
      str_detect(term, "^F") ~ "Fall",
      str_detect(term, "^W") ~ "Winter",
      str_detect(term, "^S") ~ "Spring"
    )
  ) %>%
  mutate(term_category = factor(term_category, levels = c("Spring", "Winter", "Fall"), ordered = TRUE)) %>%
  # Apply week_of_term calculation
  rowwise() %>%
  mutate(week_of_term = calculate_week_of_term(date, term)) %>%
  ungroup()

# Example event summary after filtering and transformation
event_summary <- combined_data_filtered %>%
  group_by(year, term) %>%
  summarize(term_total = n(), .groups = 'drop') %>%
  group_by(year) %>%
  mutate(year_total = sum(term_total)) %>%
  ungroup()

################################# SHINY APP ####################################
# UI
ui <- function(request) {
  dashboardPage(
    options = list(sidebarExpandOnHover = TRUE),
    header = dashboardHeader(title = tagList(
      span(class = "logo-lg", "PAC Event Data Analysis"), 
      icon("violin")),
      dropdownMenuOutput("logoutbtn"),
      controlbarIcon = icon("gear")
    ),
    sidebar = dashboardSidebar(
      minified = TRUE, collapsed = TRUE,
      sidebarMenu(
        id = "sidebarID",
        menuItem(
          "Data",
          icon = icon("chart-pie"),
          tabName = "analysis",
          badgeLabel = "Look Here!",
          badgeColor = "light-blue"
        ),
        menuItem(
          "Results",
          icon = icon("newspaper"),
          tabName = "results",
          badgeLabel = "Also Here!",
          badgeColor = "aqua"
        ),
        menuItem(
          "Contact",
          icon = icon("calendar"),
          tabName = "contact",
          badgeLabel = "DON'T",
          badgeColor = "red"
        ),
        menuItem(
          "About",
          icon = icon("scale-balanced"),
          tabName = "readme",
          badgeLabel = "READ!",
          badgeColor = "purple",
          selected = TRUE
        )
      )
    ),
    body = dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(
          tabName = "analysis",
          h3('Upload and Check Data Here'),
          tabsetPanel(id = "tabs",
                      tabPanel("Upload New Term Data", 
                               fluidRow(
                                 column(
                                   width = 8,
                                   shinycssloaders::withSpinner(plotlyOutput("Plot1"))
                                 ),
                                 column(
                                   width = 4,
                                   box(
                                     width = 12,
                                     title = "Instruction", 
                                     closable = FALSE, 
                                     status = "warning", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     dropdownMenu = boxDropdown(
                                       boxDropdownItem("Click me", id = "play1", icon = icon("heart")),
                                       tags$div(id = "audio_container1"),
                                     ), 
                                     div(
                                       h1("Data Exploration", align = "center", style = "font-weight:bold"),
                                       br(),
                                       h4("Something something"),
                                       br(),
                                       h4("Bla bla bla")
                                     )
                                   )
                                 )
                               )
                      ),
                      tabPanel("Check Processed Data Here",
                               fluidRow(
                                 column(
                                   width = 8,
                                   shinycssloaders::withSpinner(plotlyOutput("Plot2"))
                                 ),
                                 column(
                                   width = 4,
                                   box(
                                     width = 12,
                                     title = "Data Summary", 
                                     closable = FALSE, 
                                     status = "warning", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     dropdownMenu = boxDropdown(
                                       boxDropdownItem("Click me", id = "play2", icon = icon("heart")),
                                       tags$div(id = "audio_container2"),
                                     ), 
                                     div(
                                       h1("Data Summary", align = "center", style = "font-weight:bold"),
                                       br(),
                                       h4("Something something by year"),
                                       br(),
                                       h4("Bla bla bla look at that")
                                     )
                                   )
                                 )
                               )
                      )
          )
        ),
        tabItem(
          tabName = "results",
          h3('Visualizations'),
          tabsetPanel(id = "tabs2",
                      tabPanel("Overall Event Summary", 
                               fluidRow(
                                 column(
                                   width = 8,
                                   shinycssloaders::withSpinner(imageOutput("kmeans_plot_init"))
                                 ),
                                 column(
                                   width = 4,
                                   box(
                                     width = 12,
                                     title = "Analysis", 
                                     closable = FALSE, 
                                     status = "warning", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     dropdownMenu = boxDropdown(
                                       boxDropdownItem("Click me", id = "play3", icon = icon("heart")),
                                       tags$div(id = "audio_container3"),
                                     ), 
                                     div(
                                       h1("Overall Event Summary", align = "center", style = "font-weight:bold"),
                                       br(),
                                       h4("Year"),
                                       br(),
                                       h4("The graph shows bla bla bla")
                                     )
                                   )
                                 )
                               )
                      ),
                      tabPanel("Breakdown of Events by Support Level",
                               fluidRow(
                                 column(
                                   width = 8,
                                   shinycssloaders::withSpinner(imageOutput("importance_plot_init"))
                                 ),
                                 column(
                                   width = 4,
                                   box(
                                     width = 12,
                                     title = "Yep", 
                                     closable = FALSE, 
                                     status = "warning", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     dropdownMenu = boxDropdown(
                                       boxDropdownItem("Click me", id = "play4", icon = icon("heart")),
                                       tags$div(id = "audio_container4"),
                                     ), 
                                     div(
                                       h1("Support Levels Analysis", align = "center", style = "font-weight:bold"),
                                       br(),
                                       h4("The graph shows la la la"),
                                       br(),
                                       h4("Given this results, we suggest xxxxx")
                                     )
                                   )
                                 )
                               )
                      ),
                      tabPanel("Breakdown of Events by Department/Source",
                               fluidRow(
                                 column(
                                   width = 8,
                                   shinycssloaders::withSpinner(imageOutput("tree_plot_init"))
                                 ),
                                 column(
                                   width = 4,
                                   box(
                                     width = 12,
                                     title = "Department Analysis", 
                                     closable = FALSE, 
                                     status = "warning", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     dropdownMenu = boxDropdown(
                                       boxDropdownItem("Click me", id = "play5", icon = icon("heart")),
                                       tags$div(id = "audio_container5"),
                                     ), 
                                     div(
                                       h1("Decision Tree", align = "center", style = "font-weight:bold"),
                                       br(),
                                       h4("The graph confirms that xxxx")
                                     )
                                   )
                                 )
                               )
                      ),
                      tabPanel("Breakdown of Music, Collab & ODOA Events by Type",
                               fluidRow(
                                 column(
                                   width = 8,
                                   shinycssloaders::withSpinner(imageOutput("tree_plot_init"))
                                 ),
                                 column(
                                   width = 4,
                                   box(
                                     width = 12,
                                     title = "MUSC Events Analysis", 
                                     closable = FALSE, 
                                     status = "warning", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     dropdownMenu = boxDropdown(
                                       boxDropdownItem("Click me", id = "play5", icon = icon("heart")),
                                       tags$div(id = "audio_container5"),
                                     ), 
                                     div(
                                       h1("Decision Tree", align = "center", style = "font-weight:bold"),
                                       br(),
                                       h4("The graph confirms that xxxx")
                                     )
                                   )
                                 )
                               )
                      )
          )
        ),
        tabItem(
          tabName = "contact",
          h2("Don't contact me!"),
          br(),
          box(
            width = 10,
            title = "But if you really want to contact me", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE, 
            collapsed = TRUE,
            "You can try our one and only software development team:", 
            br(), 
            br(),
            fluidRow(
              box(title = "Kunwu Lyu", status = "primary", "lyuk@carleton.edu")
            ),
            br(),
            "or the person who pays me to do this:",
            fluidRow(box(title = "Alexi Carlson", status = "primary", "acarlson4@carleton.edu")), 
            br(),
            "and we'll get back to you when we're done with String Recitals."
          )
        ),
        tabItem(
          tabName = "readme",
          div(
            h1("Welcome to the PAC Office Event Data Analyzer", align = "center", style = "font-weight:bold"),
            br(),
            h4("Navigate to the Data tab to upload and check the new term data!"),
            br(),
            h4("Navigate to the Results tab to see visualizations and trends across the past years"),
            br(),
            h4("For optimal viewing, collapse the menu on the top left corner."),
            br(),
            h4("PS: use the gear icon on the top right corner to adjust parameters", align = "center", style = "font-weight:bold")
          )
        )
      )
    ),
    controlbar = dashboardControlbar(
      width = 300,
      h4("Parameters Control"),
      uiOutput("plot1Control"), 
      uiOutput("plot2Control"),
      overlay = FALSE
    ),
    footer = dashboardFooter(
      left = "By Kunwu Lyu",
      right = "Northfield, MN, 2024"
    ),
    scrollToTop = TRUE
  )
}

# Server
server <- function(input, output, session) {
  
  ############################## Plot 1 Overview ###############################
  
  # Function to render by country plot
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  renderCountriesPlot <- function(countries) {
    fig <- life_expect_cleaned %>%
      filter(country %in% countries) %>% 
      accumulate_by(~year) %>%
      plot_ly(
        x = ~ year, 
        y = ~ life_expectancy,
        split = ~ country,
        frame = ~ frame, 
        type = 'scatter',
        mode = 'lines+markers', 
        line = list(simplyfy = FALSE)
      ) %>% 
      layout(
        xaxis = list(
          title = "Year",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Life Expectancy",
          zeroline = FALSE
        )
      ) %>% 
      animation_opts(
        frame = 500, 
        transition = 1, 
        redraw = FALSE
      ) %>% 
      animation_slider(
        hide = FALSE
      ) %>% animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom"
      )
  }
  
  # Initial rendering of the countries plot
  output$Plot1 <- renderPlotly({
    renderCountriesPlot(c("Afghanistan", "Belgium"))
  })
  
  # UI for controlling countries parameters.
  output$plot1Control <- renderUI({
    conditionalPanel(condition = 'input.tabs == "Life Expectancy by Countries"',
                     selectizeInput("countries", "Compare Countries:",
                                    choices = country_names, multiple = TRUE, selected = c("Afghanistan", "Belgium")),
                     actionButton("updatePlot1", "Update"),
                     actionButton("resetPlot1", "Reset!")
    )
  })
  
  # Updating plot based on selected countries
  observeEvent(input$updatePlot1, {
    req(input$countries)
    output$Plot1 <- renderPlotly({
      isolate({
        renderCountriesPlot(input$countries)
      })
    })
  })
  
  # Resetting plot to default countries
  observeEvent(input$resetPlot1, {
    updateSelectizeInput(session, "countries", selected = c("Afghanistan", "Belgium"))
    output$Plot1 <- renderPlotly({
      renderCountriesPlot(c("Afghanistan", "Belgium"))
    })
  })
  
  ############################### Plot 2 Overview ##############################
  
  renderContinentPlot <- function(compare, log_scale) {
    # Debugging: Print the inputs received
    print(paste("compare:", compare))
    print(paste("log_scale:", log_scale))
    
    # Get the label corresponding to the selected variable
    x_axis_label <- switch(compare,
                           status = "Status",
                           adult_mortality = "Adult Mortality",
                           infant_deaths = "Infant Deaths",
                           alcohol = "Alcohol",
                           percentage_expenditure = "Percentage Expenditure",
                           hepatitis_b = "Hepatitis B",
                           measles = "Measles",
                           bmi = "BMI",
                           under_five_deaths = "Under Five Deaths per 1000",
                           polio = "Polio",
                           total_expenditure = "Total Expenditure",
                           diphtheria = "Diphtheria",
                           hiv_aids = "HIV and AIDS",
                           population = "Population",
                           thinness_1_19_years = "Thinness 1-19 Years",
                           thinness_5_9_years = "Thinness 5-9 Years",
                           income_composition_of_resources = "Income Composition of Resources",
                           schooling = "Schooling",
                           gdp_pcap = "GDP per Capita")
    
    # Determine the x-axis scale based on the log_scale argument
    x_scale <- if (log_scale) "log" else "linear"
    
    # Subset the data based on the selected variable
    data <- life_expect_cleaned %>%
      select(year, country, gdp_pcap, life_expectancy, population, continent, !!sym(compare))
    
    # Create the plotly animation
    p <- data %>% 
      plot_ly(
        x = ~get(compare), 
        y = ~life_expectancy, 
        size = ~population, 
        color = ~continent, 
        frame = ~year, 
        text = ~country, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      ) %>%
      layout(
        xaxis = list(
          title = x_axis_label,  # Set the x-axis label dynamically
          type = x_scale
        ),
        yaxis = list(
          title = "Life Expectancy"
        )
      ) %>% animation_slider(
        hide = FALSE
      ) %>% animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom"
      )
    
    return(p)
  }
  
  # Initial rendering of the plot
  output$Plot2 <- renderPlotly({
    renderContinentPlot(compare = "gdp_pcap", log_scale = TRUE)
  })
  
  # Render control bar UI elements
  output$plot2Control <- renderUI({
    conditionalPanel(condition = 'input.tabs == "Life Expectancy vs. Something"',
                     selectInput("compare_var", 
                                 "Compare Life Expectancy with:", 
                                 choices = c(Status = "status", 
                                             `Adult Mortality` = "adult_mortality", 
                                             `Infant Deaths` = "infant_deaths",
                                             Alcohol = "alcohol", 
                                             `Percentage Expenditure` = "percentage_expenditure", 
                                             `Hepatitis B` = "hepatitis_b",
                                             Measles = "measles", 
                                             BMI = "bmi",
                                             `Under Five Deaths per 1000` = "under_five_deaths", 
                                             Polio = "polio",
                                             `Total Expenditure` = "total_expenditure", 
                                             Diphtheria = "diphtheria",
                                             `HIV and AIDS` = "hiv_aids",
                                             `Population` = "population",
                                             `Thinness 1-19 Years` = "thinness_1_19_years",
                                             `Thinness 5-9 Years` = "thinness_5_9_years",
                                             `Income Composition of Resources` = "income_composition_of_resources",
                                             `Schooling` = "schooling",
                                             `GDP per Capita` = "gdp_pcap"),
                                 selected = "gdp_pcap",
                                 multiple = FALSE),
                     prettyToggle(inputId = "log_scale", 
                                  label_on = "Log Scaled",
                                  label_off = "Linear Scaled", 
                                  icon_on = icon("check-square"), 
                                  icon_off = icon("square"),
                                  status_on = "info", 
                                  status_off = "warning",
                                  value = TRUE),
                     actionButton("updatePlot2", "Update"),
                     actionButton("resetPlot2", "Reset!")
    )
  })
  
  # Update plot based on selected variable
  observeEvent(input$updatePlot2, {
    req(input$compare_var, input$log_scale)
    output$Plot2 <- renderPlotly({
      isolate({
        renderContinentPlot(input$compare_var, input$log_scale)
      })
    })
  })
  
  # Reset the plot to default variable
  observeEvent(input$resetPlot2, {
    updateSelectInput(session, "compare_var", selected = "gdp_pcap")
    updatePrettyToggle(session, "log_scale", value = TRUE)
    output$Plot2 <- renderPlotly({
      renderContinentPlot(compare = "gdp_pcap", TRUE)
    })
  })
  
  
  ################################## Refresh page ##############################
  output$logoutbtn <- renderUI({
    tags$li(a(icon("arrows-rotate"), "Refresh Page",
              href="javascript:window.location.reload(true)"),
            class = "dropdown",
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  ##################### ML Plots #############
  
  output$kmeans_plot_init <- renderImage({
    list(src = "kmeans_plot.png", contentType = 'image/png',width = 800, height = 600,
         alt = "Website under construction")
  })
  
  output$importance_plot_init <- renderImage({
    list(src = "importance_plot.png", contentType = 'image/png',width = 800, height = 600,
         alt = "Website under construction")
  })
  
  output$tree_plot_init <- renderImage({
    list(src = "tree_plot.png", contentType = 'image/png',width = 800, height = 600,
         alt = "Website under construction")
  })

  ################SURPRISE################
  
  observeEvent((input$play1), {
    insertUI(selector = "#play1",
             where = "afterEnd",
             # audio.wav should be in /www of the shiny app
             ui = tags$audio(src = "audio.wav", type = "audio/wav", autoplay = F, controls = NA, style="display:none;")
    )
  })
  
  observeEvent((input$play2), {
    insertUI(selector = "#play2",
             where = "afterEnd",
             # audio.wav should be in /www of the shiny app
             ui = tags$audio(src = "audio.wav", type = "audio/wav", autoplay = F, controls = NA, style="display:none;")
    )
  })
  
  observeEvent((input$play3), {
    insertUI(selector = "#play3",
             where = "afterEnd",
             # audio.wav should be in /www of the shiny app
             ui = tags$audio(src = "audio.wav", type = "audio/wav", autoplay = F, controls = NA, style="display:none;")
    )
  })
  
  observeEvent((input$play4), {
    insertUI(selector = "#play4",
             where = "afterEnd",
             # audio.wav should be in /www of the shiny app
             ui = tags$audio(src = "audio.wav", type = "audio/wav", autoplay = F, controls = NA, style="display:none;")
    )
  })
  
  observeEvent((input$play5), {
    insertUI(selector = "#play5",
             where = "afterEnd",
             # audio.wav should be in /www of the shiny app
             ui = tags$audio(src = "audio.wav", type = "audio/wav", autoplay = F, controls = NA, style="display:none;")
    )
  })
  
}

# Run the app
shinyApp(ui, server)
