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
library(DT)
library(sodium)
remotes::install_github("timelyportfolio/dataui")
gs4_auth(email = "lyuk@carleton.edu", cache = ".secrets")

############################### USERNAME / PASSWORD ############################
# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: myuser  Password: mypass"),
                     br(),
                     tags$code("Username: myuser1  Password: mypass1")
                   ))
)

credentials <- data.frame(
  username_id = c("myuser", "myuser1"),
  password   = sapply(c("mypass", "mypass1"), password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = FALSE
)

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

term_to_year <- function(term) {
  year <- as.numeric(str_sub(term, 2, 3))
  season <- str_sub(term, 1, 1)
  start_year <- if_else(season == "F", 2000 + year, 2000 + year - 1)
  end_year <- start_year + 1
  return(paste0(start_year, "-", end_year))
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

sidebar <- dashboardSidebar(minified = TRUE, collapsed = TRUE,
                            uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

# Define menu items
dataMenu <- menuItem(
  "Data",
  icon = icon("chart-pie"),
  tabName = "analysis",
  badgeLabel = "Look Here!",
  badgeColor = "light-blue"
)

resultsMenu <- menuItem(
  "Results",
  icon = icon("newspaper"),
  tabName = "results",
  badgeLabel = "Also Here!",
  badgeColor = "aqua"
)

contactMenu <- menuItem(
  "Contact",
  icon = icon("calendar"),
  tabName = "contact",
  badgeLabel = "DON'T",
  badgeColor = "red"
)

aboutMenu <- menuItem(
  "About",
  icon = icon("scale-balanced"),
  tabName = "readme",
  badgeLabel = "READ!",
  badgeColor = "purple",
  selected = TRUE
)

# Define tab items
analysisTab <- tabItem(
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
                               tags$div(id = "audio_container1")
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
                               tags$div(id = "audio_container2")
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
)

resultsTab <- tabItem(
  tabName = "results",
  h3('Visualizations'),
  tabsetPanel(id = "tabs2",
              tabPanel("Overall Event Summary", 
                       fluidRow(
                         column(
                           width = 8,
                           shinycssloaders::withSpinner(plotlyOutput("Plot1"))
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
                               tags$div(id = "audio_container3")
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
                           shinycssloaders::withSpinner(plotlyOutput("Plot2"))
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
                               tags$div(id = "audio_container4")
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
                           shinycssloaders::withSpinner(plotlyOutput("Plot3"))
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
                               tags$div(id = "audio_container5")
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
                           shinycssloaders::withSpinner(plotlyOutput("Plot4"))
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
                               boxDropdownItem("Click me", id = "play6", icon = icon("heart")),
                               tags$div(id = "audio_container6")
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
)

contactTab <- tabItem(
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
)

readmeTab <- tabItem(
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


################################# SHINY APP ####################################
# UI
ui <- function(request) {
  dashboardPage(
    options = list(sidebarExpandOnHover = TRUE),
    header = dashboardHeader(title = tagList(
      span(class = "logo-lg", "PAC Event Data Analysis"), 
      icon("heart")),
      dropdownMenuOutput("logoutbtn"),
      controlbarIcon = icon("gear")
    ),
    sidebar,
    body,
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
  
  ############################## AUTHENTICATION ################################
  USER <- reactiveValues(login = FALSE)
  
  observeEvent(input$login, {
    Username <- isolate(input$userName)
    Password <- isolate(input$passwd)
    
    if (nrow(credentials[credentials$username_id == Username, ]) == 1) {
      stored_password <- credentials$password[credentials$username_id == Username]
      if (password_verify(stored_password, Password)) {
        USER$login <- TRUE
      } else {
        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
      }
    } else {
      shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
      shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
    }
  })
  
  ################################## Refresh page ##############################
  output$logoutbtn <- renderUI({
    if (USER$login) {
      tags$li(a(icon("sign-out"), "Logout", 
                href = "javascript:window.location.reload(true)"),
              class = "dropdown", 
              style = "background-color: #eee !important; border: 0;
                      font-weight: bold; margin:5px; padding: 10px;")
    }
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        sidebarMenu(
          id = "sidebarID",
          dataMenu,
          resultsMenu,
          contactMenu,
          aboutMenu
        )
      }
      else{
        sidebarMenu(
          id = "sidebarID",
          resultsMenu,
          contactMenu,
          aboutMenu
        )
      }
    }
  })
  
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        tabItems(
          analysisTab,
          resultsTab,
          contactTab,
          readmeTab
        )
      } 
      else {
        tabItem(
          resultsTab,
          contactTab,
          readmeTab)
      }
    }
    else {
      loginpage
    }
  })
  
  
  ############################## Plot 1 Overview ###############################
  
  # Function to get sheet names from Google Sheets
  get_sheet_names <- function(sheet_url) {
    sheet_names <- sheets_sheets(sheet_url)
    return(sheet_names)
  }
  
  # Function to extract term from sheet name
  extract_term <- function(sheet_name) {
    # Extract term from sheet name assuming format "F14 - Event Data", "W15 - Event Data", etc.
    term_match <- str_extract(sheet_name, "^[FWS]\\d{2}")
    return(term_match)
  }
  
  # Function to process and sort event summary based on dynamic term levels
  process_event_summary <- function(event_summary, sheet_names) {
    # Extract terms from sheet names
    terms <- unique(sapply(sheet_names, extract_term))
    
    # Define term levels based on sheet names
    term_levels <- c()
    for (season in c("F", "W", "S")) {
      term_levels <- c(term_levels, sort(terms[str_detect(terms, paste0("^", season))]))
    }
    
    event_summary %>%
      mutate(term = factor(term, levels = term_levels, ordered = TRUE)) %>%
      arrange(year, term) %>%
      mutate(
        term_category = case_when(
          str_detect(term, "^F") ~ "Fall",
          str_detect(term, "^W") ~ "Winter",
          str_detect(term, "^S") ~ "Spring"
        )
      ) %>%
      mutate(term_category = factor(term_category, levels = c("Fall", "Winter", "Spring"), ordered = TRUE))
  }
  
  # URL of the Google Sheets
  sheet_url <- "https://docs.google.com/spreadsheets/d/1a0wHpBMmUMoeKrTK23nHcYvFpQ2djmcYKmjJqEJWX1I/edit?gid=265403245"
  
  # Fetch sheet names
  sheet_names <- sheet_names(sheet_url)
  
  # Assuming event_summary is already loaded
  # Process the event summary data frame
  event_summary_processed <- process_event_summary(event_summary, sheet_names)
  
  # Create the stacked bar chart using ggplot2
  ggplot_event_summary <- ggplot(event_summary_processed, aes(x = year, y = term_total, fill = term_category)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = term_total), 
              position = position_stack(vjust = 0.5), 
              size = 3, 
              color = "black") +
    scale_fill_manual(values = c("Fall" = "#FF9999", "Winter" = "#99CCFF", "Spring" = "#99FF99")) +
    labs(x = "Year", y = "Total Events", fill = "Term") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convert the ggplot object to a plotly object for interactivity
  plotly_event_summary <- ggplotly(ggplot_event_summary)
  
  # Initial rendering of the countries plot
  output$Plot1 <- renderPlotly({
    plotly_event_summary
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
  
  # Function to summarize and pivot data for each year
  summarize_and_pivot <- function(current_year) {
    combined_data_filtered %>%
      filter(year == current_year) %>%
      group_by(term_category, support_level) %>%
      summarize(Support = n(), .groups = 'drop') %>%
      pivot_wider(names_from = support_level, values_from = Support, values_fill = list(Support = 0)) %>%
      mutate(year = current_year)  # Add a year column to identify the table
  }
  
  # List of years to iterate over
  years <- unique(combined_data_filtered$year)
  
  # Use purrr::map to apply summarize_and_pivot function for each year
  support_tables <- map_dfr(years, summarize_and_pivot)  # Combine into a single data frame
  
  # Melt the data for ggplot
  support_tables_melted <- support_tables %>%
    pivot_longer(cols = -c(term_category, year), names_to = "support_level", values_to = "Support")
  
  # Ensure the term_category and support_level are factors with the correct order
  support_tables_melted <- support_tables_melted %>%
    filter(support_level != "NA") %>% 
    mutate(term_category = factor(term_category, levels = c("Fall", "Winter", "Spring"), ordered = TRUE),
           support_level = factor(support_level, levels = c("NA", "H", "M", "L"), ordered = TRUE))
  
  # Calculate percentages for each segment
  support_tables_melted <- support_tables_melted %>%
    group_by(year, term_category) %>%
    mutate(total_support = sum(Support),
           percentage = (Support / total_support) * 100)
  
  # Create the ggplot chart
  ggplot_support_summary <- ggplot(support_tables_melted, aes(x = term_category, y = Support, fill = support_level)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(Support, " (", round(percentage, 1), "%)")),
              position = position_stack(vjust = 0.5), size = 2, color = "black") +
    facet_wrap(~ year) +
    labs(x = "Term", y = "Support Count", fill = "Support Level") +
    scale_fill_manual(values = c("H" = "#FF9999", "M" = "#99CCFF", "L" = "#99FF99", "NA" = "black")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convert to a plotly object
  plotly_support_summary <- ggplotly(ggplot_support_summary)
  
  # Initial rendering of the plot
  output$Plot2 <- renderPlotly({
    # Display the plotly chart
    plotly_support_summary
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
  
  ############################### Plot 3 Overview ##############################
  # Function to summarize and pivot data for each year
  summarize_and_pivot_department <- function(current_year) {
    combined_data_filtered %>%
      filter(year == current_year) %>%
      group_by(term_category, department_type) %>%
      summarize(DepartmentCount = n(), .groups = 'drop') %>%
      pivot_wider(names_from = department_type, values_from = DepartmentCount, values_fill = list(DepartmentCount = 0)) %>%
      mutate(year = current_year)  # Add a year column to identify the table
  }
  
  # List of years to iterate over
  years <- unique(combined_data_filtered$year)
  
  # Use purrr::map to apply summarize_and_pivot function for each year
  department_tables <- map_dfr(years, summarize_and_pivot_department)  # Combine into a single data frame
  
  # Melt the data for ggplot
  department_tables_melted <- department_tables %>%
    pivot_longer(cols = -c(term_category, year), names_to = "department_type", values_to = "DepartmentCount")
  
  # Ensure the term_category and department_type are factors with the correct order
  department_tables_melted <- department_tables_melted %>%
    mutate(term_category = factor(term_category, levels = c("Fall", "Winter", "Spring"), ordered = TRUE),
           department_type = factor(department_type, levels = c("MUSC", "ODOA", "CSA", "Collab", "Others"), ordered = TRUE))
  
  # Filter out NA department types (if any)
  department_tables_melted <- department_tables_melted %>%
    filter(department_type != "NA")
  
  # Calculate percentages for each segment
  department_tables_melted <- department_tables_melted %>%
    group_by(year, term_category) %>%
    mutate(total_count = sum(DepartmentCount, na.rm = TRUE),
           percentage = (DepartmentCount / total_count) * 100)
  
  # Create the ggplot chart
  ggplot_department_summary <- ggplot(department_tables_melted, aes(x = term_category, y = DepartmentCount, fill = department_type)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(DepartmentCount, " (", round(percentage, 1), "%)")),
              position = position_stack(vjust = 0.5), size = 2, color = "black",
              check_overlap = TRUE) +
    facet_wrap(~ year) +
    labs(x = "Term", y = "Department Count", fill = "Department Type") +
    scale_fill_manual(values = c("MUSC" = "#FF9999", "ODOA" = "#99CCFF", "CSA" = "#99FF99", "Collab" = "#FFD700", "Others" = "#FFA500")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convert to a plotly object
  plotly_department_summary <- ggplotly(ggplot_department_summary)
  
  # Initial rendering of the plot
  output$Plot3 <- renderPlotly({
    # Display the plotly chart
    plotly_department_summary
  })
  
  # Render control bar UI elements
  output$plot3Control <- renderUI({
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
  observeEvent(input$updatePlot3, {
    req(input$compare_var, input$log_scale)
    output$Plot2 <- renderPlotly({
      isolate({
        renderContinentPlot(input$compare_var, input$log_scale)
      })
    })
  })
  
  # Reset the plot to default variable
  observeEvent(input$resetPlot3, {
    updateSelectInput(session, "compare_var", selected = "gdp_pcap")
    updatePrettyToggle(session, "log_scale", value = TRUE)
    output$Plot2 <- renderPlotly({
      renderContinentPlot(compare = "gdp_pcap", TRUE)
    })
  })
  
  ############################### Plot 4 Overview ##############################
  # Function to summarize and pivot data for each year
  summarize_and_pivot_event <- function(current_year) {
    combined_data_filtered %>%
      filter(year == current_year) %>%
      group_by(term_category, event_type) %>%
      summarize(EventCount = n(), .groups = 'drop') %>%
      pivot_wider(names_from = event_type, values_from = EventCount, values_fill = list(EventCount = 0)) %>%
      mutate(year = current_year)  # Add a year column to identify the table
  }
  
  # List of years to iterate over
  years <- unique(combined_data_filtered$year)
  
  # Use purrr::map to apply summarize_and_pivot function for each year
  event_tables <- map_dfr(years, summarize_and_pivot_event)  # Combine into a single data frame
  
  # Melt the data for ggplot
  event_tables_melted <- event_tables %>%
    pivot_longer(cols = -c(term_category, year), names_to = "event_type", values_to = "EventCount")
  
  # Ensure term_category and event_type are characters before converting to factors
  event_tables_melted <- event_tables_melted %>%
    mutate(
      term_category = as.character(term_category),
      event_type = as.character(event_type)
    )
  
  # Apply the factor conversion with ordering
  event_tables_melted <- event_tables_melted %>%
    mutate(
      term_category = factor(term_category, levels = c("Fall", "Winter", "Spring"), ordered = TRUE),
      event_type = factor(event_type, levels = c("Ensemble Concert", "Student Activity", "Studio Recital",
                                                 "Guest", "Faculty Recital", "Student Recital",
                                                 "Special Events", "Presentation", "Masterclass"),
                          ordered = TRUE)
    )
  
  # Calculate percentages for each segment
  event_tables_melted <- event_tables_melted %>%
    group_by(year, term_category) %>%
    mutate(total_count = sum(EventCount, na.rm = TRUE),
           percentage = (EventCount / total_count) * 100)
  
  # Create the ggplot chart
  ggplot_event_summary <- ggplot(event_tables_melted, aes(x = term_category, y = EventCount, fill = event_type)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(EventCount, " (", round(percentage, 1), "%)")),
              position = position_stack(vjust = 0.5), size = 2, color = "black",
              check_overlap = TRUE) +
    facet_wrap(~ year) +
    labs(x = "Term", y = "Event Count", fill = "Event Type") +
    scale_fill_brewer(palette = "Set2") +  # Using a Brewer palette for default colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convert to a plotly object
  plotly_event_summary <- ggplotly(ggplot_event_summary)
  
  # Initial rendering of the plot
  output$Plot4 <- renderPlotly({
    # Display the plotly chart
    plotly_event_summary
  })
  
  # Render control bar UI elements
  output$plot4Control <- renderUI({
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
  observeEvent(input$updatePlot4, {
    req(input$compare_var, input$log_scale)
    output$Plot2 <- renderPlotly({
      isolate({
        renderContinentPlot(input$compare_var, input$log_scale)
      })
    })
  })
  
  # Reset the plot to default variable
  observeEvent(input$resetPlot4, {
    updateSelectInput(session, "compare_var", selected = "gdp_pcap")
    updatePrettyToggle(session, "log_scale", value = TRUE)
    output$Plot2 <- renderPlotly({
      renderContinentPlot(compare = "gdp_pcap", TRUE)
    })
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