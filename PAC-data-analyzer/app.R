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
library(formattable)
library(shinyfullscreen)
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
                     br()
                   ))
)

credentials <- data.frame(
  username_id = c("KPSstu", "pac", "I left a back door"),
  password   = sapply(c("ShowComp", " pac", "Kunwu says LDC at 7:30"), password_store),
  permission  = c("basic", "advanced", "advanced"), 
  stringsAsFactors = FALSE
)

################################ DATA WRANGLING ################################

## Google Sheet Version
# # Define the Google Sheet URL
# sheet_url <- "https://docs.google.com/spreadsheets/d/1a0wHpBMmUMoeKrTK23nHcYvFpQ2djmcYKmjJqEJWX1I/edit#gid=265403245"
# 
# # Get the list of sheet names from the Google Sheet
# sheet_names_list <- sheet_names(sheet_url)
# 
# # Extract the terms from the sheet names dynamically
# extracted_terms <- sheet_names_list %>%
#   str_extract("^[A-Z]\\d{2}") %>%
#   na.omit()  # Remove any NA values in case some sheet names do not follow the pattern
# 
# # Ensure unique and sorted terms
# unique_terms <- sort(unique(extracted_terms))
# 
# # Create a function to read and clean a sheet
# read_and_clean_event_file <- function(sheet_name) {
#   # Read the sheet using googlesheets4
#   data <- read_sheet(sheet_url, sheet = sheet_name)
#   
#   # Clean column names and ensure all columns are character type
#   clean_names(data) %>%
#     mutate(across(everything(), as.character)) %>%
#     mutate(term = str_sub(sheet_name, 1, 3)) # Extract the term from the sheet name
# }
# 
# term_to_year <- function(term) {
#   year <- as.numeric(str_sub(term, 2, 3))
#   season <- str_sub(term, 1, 1)
#   start_year <- if_else(season == "F", 2000 + year, 2000 + year - 1)
#   end_year <- start_year + 1
#   return(paste0(start_year, "-", end_year))
# }
# 
# # Use purrr to read all sheets and store them in a named list
# event_data_list <- set_names(map(sheet_names_list, read_and_clean_event_file), sheet_names_list)
# 
# # Combine all the data frames into one
# combined_data <- reduce(event_data_list, full_join)

## Local version
# Define the directory containing the files
data_dir <- "data/"

# Define the file codes
file_codes <- c("F14", "W15", "S15", "F15", "W16", "S16", "F16", "W17", "S17", 
                "F17", "W18", "S18", "F18", "W19", "S19", "F19", "W20", "S20", 
                "F20", "W21", "S21", "F21", "W22", "S22", "F22", "W23", "S23", 
                "F23", "W24", "S24")

# Create a function to read a file given its code
read_and_clean_event_file <- function(code) {
  file_path <- paste0(data_dir, "2015-2024 Events Data - ", code, " - Event Data.csv")
  data <- read_csv(file_path)
  clean_names(data) %>% 
    mutate(across(everything(), as.character)) %>% 
    mutate(term = code)
}

term_to_year <- function(term) {
  year <- as.numeric(str_sub(term, 2, 3))
  season <- str_sub(term, 1, 1)
  start_year <- if_else(season == "F", 2000 + year, 2000 + year - 1)
  end_year <- start_year + 1
  return(paste0(start_year, "-", end_year))
}

# Use purrr to read all files and store them in a named list
event_data_list <- set_names(map(file_codes, read_and_clean_event_file), file_codes)

combined_data <- reduce(event_data_list, full_join)

# Define the term start dates (hardcoded as per the original logic)
term_start_dates <- data.frame(
  term = file_codes,  # Add all relevant terms
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
  mutate(
    livestream = coalesce(livestream, live_stream)  # If 'livestream' is NA, use 'live_stream'
  ) %>%
  select(-live_stream) %>%  
  mutate(department = ifelse(str_detect(what, "CSA|Just Cellin|Lunar New Year|ACA|A Cappella|Accidentals|Exit 69|Date Knight|Knights|Knightingales|International Festival"), "CSA", department)) %>% 
  mutate(
    support_level = fct_relevel(factor(support_level), "H", "M", "L"),
    audio_needs = fct_relevel(factor(audio_needs), "H", "M", "L"),
    stage_needs = fct_relevel(factor(stage_needs), "H", "M", "L"),
    lighting_needs = fct_relevel(factor(lighting_needs), "H", "M", "L"),
    projection = fct_relevel(factor(projection), "Y", "N"),
    video_recording = fct_relevel(factor(video_recording), "Y", "N"),
    livestream = fct_relevel(factor(livestream), "Y", "N"),
    poster = fct_relevel(factor(poster), "Y", "N"),
    program = fct_relevel(factor(program), "Y", "N"),
    reception = fct_relevel(factor(reception), "Y", "N")
  ) %>%
  mutate(
    venue = factor(venue),
    department = factor(department)
  ) %>%  
  mutate(
    audience_count = as.numeric(ifelse(grepl("^[0-9]+$", audience_count), audience_count, NA)),
    days_committed = as.numeric(ifelse(grepl("^[0-9]+$", days_committed), days_committed, NA)),
    av_staff = as.numeric(ifelse(grepl("^[0-9]+$", av_staff), av_staff, NA)),
    pac_staff = as.numeric(ifelse(grepl("^[0-9]+$", pac_staff), pac_staff, NA))
  ) %>%  
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

combined_data_filtered %>% filter(event_type == "Guest",
                                  str_detect(department, "MUSC")) %>% 
  filter(!str_detect(what, "Masterclass")) -> guest_only

# Get the unique years and append the "All" option
year_choices <- c("All", combined_data_filtered %>% pull(year) %>% unique())

############################ UI ################################################


sidebar <- dashboardSidebar(minified = TRUE, collapsed = TRUE,
                            uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"),
                      tags$script(HTML("
    $(document).on('keypress', function(e) {
      if(e.which == 13) {
        $('#login').click();
      }
    });
  ")))

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
                           width = 6,
                           br(),
                           textInput("termID", "Term:", placeholder = "e.g., F24"),
                           br(),
                           dateInput("termDate", "First Day of Class:", format =  "yyyy-mm-dd",
                                     weekstart = 1),
                           br(),
                           actionBttn("submitNewTerm", "Submit", style = "material-flat",
                                      color = "primary")
                         ),
                         column(
                           width = 6,
                           shinycssloaders::withSpinner(fullscreen_this(reactableOutput(outputId = "term_table")))
                         )
                       )
              ),
              tabPanel("Check Processed Data Here",
                       fluidRow(
                           box(
                             width = 12,
                             title = "Data Summary", 
                             closable = FALSE, 
                             status = "danger", 
                             solidHeader = TRUE, 
                             collapsible = TRUE,
                             dropdownMenu = boxDropdown(
                               boxDropdownItem("Click me", id = "play2", icon = icon("heart")),
                               tags$div(id = "audio_container2"),
                               tags$div(id = "dropdownMenu-container",
                               dropdownDivider(),
                                        selectInput("exclude_tab_vars", tags$span(style = "color:black;", "Exclude Variables:"), 
                                                    choices = c(
                                                      "Venue" = "venue",
                                                      "Date" = "date",
                                                      "Event" = "what",
                                                      "Department" = "department",
                                                      "Days Committed" = "days_committed",
                                                      "AV Staff" = "av_staff",
                                                      "PAC Staff" = "pac_staff",
                                                      "Support Level" = "support_level",
                                                      "Audio Needs" = "audio_needs",
                                                      "Stage Needs" = "stage_needs",
                                                      "Lighting Needs" = "lighting_needs",
                                                      "Projection" = "projection",
                                                      "Video Recording" = "video_recording",
                                                      "Livestream" = "livestream",
                                                      "Audience Count" = "audience_count",
                                                      "Poster" = "poster",
                                                      "Program" = "program",
                                                      "Reception" = "reception",
                                                      "Term" = "term",
                                                      "Department Type" = "department_type",
                                                      "Event Type" = "event_type",
                                                      "Year" = "year",
                                                      "Term Category" = "term_category",
                                                      "Week of Term" = "week_of_term"
                                                    ),  
                                                    multiple = TRUE,
                                                    selected = c("days_committed", "poster", "program", 
                                                                 "reception", "term", "department_type", 
                                                                 "year", "term_category", "week_of_term")
                                        )
                               ),
                               # Green "Update" button
                               actionBttn(inputId = "update_tab", label = "Update", 
                                          style = "fill", color = "success"),
                               # Red "Reset" button
                               actionBttn(inputId = "reset_tab", label = "Reset", 
                                          style = "fill", color = "danger"),
                               dropdownDivider(),
                               boxDropdownItem("Generate Music Guest Info", id = "MUSC_Guest", 
                                               icon = icon("copy")),
                               
                               # Clipboard.js script
                               tags$head(
                                 tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js")
                               ),
                               tags$script(HTML("
                                Shiny.addCustomMessageHandler('txt', function(message) {
                                  var copyButton = document.createElement('button');
                                  copyButton.setAttribute('id', 'copyButton');
                                  copyButton.setAttribute('data-clipboard-text', message);
                                  document.body.appendChild(copyButton);
                            
                                  var clipboard = new ClipboardJS('#copyButton');
                            
                                  clipboard.on('success', function(e) {
                                    Shiny.setInputValue('clipboard_status', 'success');
                                    document.body.removeChild(copyButton);
                                  });
                            
                                  clipboard.on('error', function(e) {
                                    Shiny.setInputValue('clipboard_status', 'error');
                                    document.body.removeChild(copyButton);
                                  });
                            
                                  copyButton.click();
                                });
                              ")),
                                                           
                                                           # Fix for dropdown menu closing issue
                                                           tags$script(HTML("
                                $(document).on('click', '#dropdownMenu-container', function(event) {
                                  event.stopPropagation();
                                });
                              "))
                             ), 
                             div(
                               h1("Data Summary", align = "center", style = "font-weight:bold"),
                               shinycssloaders::withSpinner(fullscreen_this(reactableOutput("combined_table")))
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
                               dropdownDivider(),
                               
                               # Wrap the selectInput inside a div and prevent event propagation
                               tags$div(id = "dropdownMenu-container",
                                        selectInput(
                                          inputId = "year_select_plot1",
                                          label = tags$span(style = "color:black;", "Select Year:"),  # Label with black color
                                          choices = year_choices,
                                          selected = "All",  # Default selection
                                          multiple = TRUE    # Allow multiple selections
                                        )
                               ),
                               
                               # Green "Update" button
                               actionBttn(inputId = "update_plot1", label = "Update", 
                                          style = "fill", color = "success"),
                               
                               # Red "Reset" button
                               actionBttn(inputId = "reset_plot1", label = "Reset", 
                                          style = "fill", color = "danger"),
                               
                               # Fix the dropdown menu closing issue using JavaScript
                               tags$script(HTML("
    $(document).on('click', '#dropdownMenu-container', function(event) {
      event.stopPropagation();
    });
  "))
                             ), 
                             div(
                               h1("Overall Event Summary", align = "center", style = "font-weight:bold"),
                               br(),
                               shinycssloaders::withSpinner(fullscreen_this(plotlyOutput("Plot1")))
                             )
                           )
                       ),
                       fluidRow(div(br(), 
                                    h3("  maybe something else to talk about this")))
              ),
              tabPanel("Breakdown of Events by Support Level",
                       fluidRow(
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
                               dropdownDivider(),
                               # Year select input for plot2
                               tags$div(id = "dropdownMenu-container",
                                        selectInput(
                                          inputId = "year_select_plot2",
                                          label = tags$span(style = "color:black;", "Select Year:"),
                                          choices = year_choices,
                                          selected = "All", 
                                          multiple = TRUE
                                        )
                               ),
                               
                               # Update and Reset buttons for plot2
                               actionBttn(inputId = "update_plot2", label = "Update", 
                                          style = "fill", color = "success"),
                               actionBttn(inputId = "reset_plot2", label = "Reset", 
                                          style = "fill", color = "danger"),
                               dropdownDivider(),
                               # Toggle for audience count
                               switchInput(
                                 inputId = "toggle_audience",
                                 onLabel = tags$span(style = "color:black;", "Show Audience Count"),
                                 offLabel = tags$span(style = "color:black;", "Hide Audience Count"),
                                 onStatus = "success",
                                 offStatus = "danger",
                                 size = "small"
                               ),
                               
                               # Fix the dropdown menu closing issue
                               tags$script(HTML("
    $(document).on('click', '#dropdownMenu-container', function(event) {
      event.stopPropagation();
    });
  "))
                             ), 
                             div(
                               h1("Support Levels Analysis", align = "center", style = "font-weight:bold"),
                               br(),
                               shinycssloaders::withSpinner(fullscreen_this(plotlyOutput("Plot2")))
                             )
                           )
                       ),
                       fluidRow(div(br(), 
                                    h3("  maybe something else to talk about this")))
              ),
              tabPanel("Breakdown of Events by Department/Source",
                       fluidRow(
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
                               dropdownDivider(),
                               # Wrap the selectInput inside a div and prevent event propagation
                               tags$div(id = "dropdownMenu-container",
                                        selectInput(
                                          inputId = "year_select_plot3",
                                          label = tags$span(style = "color:black;", "Select Year:"),  # Label with black color
                                          choices = year_choices,
                                          selected = "All",  # Default selection
                                          multiple = TRUE    # Allow multiple selections
                                        )
                               ),
                               
                               # Green "Update" button
                               actionBttn(inputId = "update_plot3", label = "Update", 
                                          style = "fill", color = "success"),
                               
                               # Red "Reset" button
                               actionBttn(inputId = "reset_plot3", label = "Reset", 
                                          style = "fill", color = "danger"),
                               
                               # Fix the dropdown menu closing issue using JavaScript
                               tags$script(HTML("
    $(document).on('click', '#dropdownMenu-container', function(event) {
      event.stopPropagation();
    });
  "))
                             ), 
                             div(
                               h1("Sponsor Types", align = "center", style = "font-weight:bold"),
                               br(),
                               shinycssloaders::withSpinner(fullscreen_this(plotlyOutput("Plot3")))
                             )
                           )
                       ),
                       fluidRow(div(br(), 
                                    h3("  maybe something else to talk about this")))
              ),
              tabPanel("Breakdown of Music, Collab & ODOA Events by Type",
                       fluidRow(
                           box(
                             width = 12,
                             title = "MUSC Events Analysis", 
                             closable = FALSE, 
                             status = "warning", 
                             solidHeader = TRUE, 
                             collapsible = TRUE,
                             dropdownMenu = boxDropdown(
                               boxDropdownItem("Click me", id = "play6", icon = icon("heart")),
                               tags$div(id = "audio_container6"),
                               dropdownDivider(),
                               ################# TEMP 1 ########################
                               # Wrap the selectInput inside a div and prevent event propagation
                               tags$div(id = "dropdownMenu-container",
                                        selectInput(
                                          inputId = "year_select_plot4",
                                          label = tags$span(style = "color:black;", "Select Year:"),  # Label with black color
                                          choices = year_choices,
                                          selected = "All",  # Default selection
                                          multiple = TRUE    # Allow multiple selections
                                        )
                               ),
                               
                               # Green "Update" button
                               actionBttn(inputId = "update_plot4", label = "Update", 
                                          style = "fill", color = "success"),
                               
                               # Red "Reset" button
                               actionBttn(inputId = "reset_plot4", label = "Reset", 
                                          style = "fill", color = "danger"),
                               
                               dropdownDivider(),
                               # Toggle for music only
                               switchInput(
                                 inputId = "toggle_music",
                                 onLabel = tags$span(style = "color:black;", "Music Only"),
                                 offLabel = tags$span(style = "color:black;", "All Departments"),
                                 onStatus = "success",
                                 offStatus = "danger",
                                 size = "small",
                                 value = TRUE
                               ),
                               
                               # Fix the dropdown menu closing issue using JavaScript
                               tags$script(HTML("
    $(document).on('click', '#dropdownMenu-container', function(event) {
      event.stopPropagation();
    });
  "))
                             ), 
                             div(
                               h1("Event Type", align = "center", style = "font-weight:bold"),
                               br(),
                               shinycssloaders::withSpinner(fullscreen_this(plotlyOutput("Plot4")))
                             )
                           )
                       ),
                       fluidRow(div(br(), 
                                    h3("  maybe something else to talk about this")))
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
      userBox(title = userDescription(
        title = "Kunwu Lyu",
        subtitle = "lead Developer",
        type = 1,
        image = "https://media.licdn.com/dms/image/v2/D5603AQHPh_-Y2pO5MA/profile-displayphoto-shrink_200_200/profile-displayphoto-shrink_200_200/0/1685996205041?e=2147483647&v=beta&t=tLCBes3OQbU0wXwExAH3PGipB-9jZx-EsImiMFrIMtQ",
        backgroundImage = "https://hga.com/wp-content/uploads/2018/04/Carleton-College-Music-and-Performance-Commons-Addition-interior-ballet-rehearsal.jpg"
      ), status = "primary", "lyuk@carleton.edu")
    ),
    br(),
    "or the person who pays me to do this:",
    fluidRow(userBox(title = userDescription(
      title = "Alexi Carlson",
      subtitle = "Performing Activities Coordinator",
      type = 1,
      image = "https://cdn.carleton.edu/uploads/sites/172/2021/12/Alexi-Carlson-Headshot.png?resize=400,400&crop=0,6,100,88",
      backgroundImage = "https://www.pegasusgrp.net/uploads/1/1/7/4/117471487/1826-027-00-carltonpac-av-485-medium_orig.jpg"
    ), status = "primary", "acarlson4@carleton.edu")), 
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
    if (USER$login == TRUE) { 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)] == "advanced") {
        sidebarMenu(
          id = "sidebarID",
          dataMenu,   # Include all menus for advanced users
          resultsMenu,
          contactMenu,
          aboutMenu
        )
      } else {
        sidebarMenu(
          id = "sidebarID",
          resultsMenu,  # Exclude the Data menu for non-advanced users
          contactMenu,
          aboutMenu
        )
      }
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)] == "advanced") {
        tabItems(
          analysisTab,  # Include all tabs for advanced users
          resultsTab,
          contactTab,
          readmeTab
        )
      } else {
        tabItems(
          resultsTab,  # Exclude the Analysis tab for non-advanced users
          contactTab,
          readmeTab
        )
      }
    } else {
      loginpage  # Show the login page when not logged in
    }
  })
  
  ############################## Generate Music Guest Info #####################
  observeEvent(input$MUSC_Guest, {
    # Read and clean the data
    music_guest_only <- read_csv(file = "guest_only.csv")
    print_guest_dat <- music_guest_only %>% 
      select(date, what, genre, sponsor, year, term_category) %>% 
      mutate(what = str_replace_all(what, "GUEST: ", ""))
    
    # Function to generate guest information text
    print_guest_fn <- function(data) {
      years <- unique(data$year)
      output_text <- ""
      
      for (year in years) {
        output_text <- paste0(output_text, "\n---------------------------\n", year, "\n")
        
        terms <- unique(data$term_category[data$year == year])
        for (term in terms) {
          output_text <- paste0(output_text, "\n", term, "\n")
          
          term_data <- data[data$year == year & data$term_category == term, ]
          if (nrow(term_data) == 0) {
            output_text <- paste0(output_text, "No guests\n")
          } else {
            for (i in 1:nrow(term_data)) {
              event <- term_data$what[i]
              genre <- term_data$genre[i]
              sponsor <- term_data$sponsor[i]
              output_text <- paste0(output_text, event, " | ", genre, " | ", sponsor, "\n")
            }
          }
        }
      }
      output_text <- paste0(output_text, "\n---------------------------\n")
      return(output_text)
    }
    
    # Generate the text
    musc_guest_info <- print_guest_fn(print_guest_dat)
    
    # Attempt to send the text as a custom message
    tryCatch({
      session$sendCustomMessage("txt", musc_guest_info)
      
      # Success: Show a success alert using shinyalert
      shinyalert::shinyalert("Success!", "Guest information copied to clipboard.", type = "success"
                             ,closeOnEsc = TRUE,
                             closeOnClickOutside = TRUE
                             )
      
    }, error = function(e) {
      # Failure: Show an error alert
      shinyalert::shinyalert("Error", "Failed to copy guest information.", type = "error"
                             ,closeOnEsc = TRUE,
                             closeOnClickOutside = TRUE
                             )
    })
  })
  
  
  ############################## Plot 1 Overview ###############################
  # Sort the data frame by year and term in the order of F, W, S
  event_summary <- event_summary %>%
    mutate(term = factor(term, levels = c("F14", "W15", "S15", 
                                          "F15", "W16", "S16", "F16", "W17", "S17", 
                                          "F17", "W18", "S18", "F18", "W19", "S19", 
                                          "F19", "W20", "S20", "F20", "W21", "S21", 
                                          "F21", "W22", "S22", "F22", "W23", "S23", 
                                          "F23", "W24", "S24"), ordered = TRUE)) %>%
    arrange(year, term) %>%
    mutate(
      term_category = case_when(
        str_detect(term, "^F") ~ "Fall",
        str_detect(term, "^W") ~ "Winter",
        str_detect(term, "^S") ~ "Spring"
      )
    ) %>%
    mutate(term_category = factor(term_category, levels = c("Fall", "Winter", "Spring"), ordered = TRUE))
  
  # Reactive values to store the filtered dataset and selected years
  reactive_vals <- reactiveValues(
    filtered_data = event_summary,
    selected_years = unique(event_summary$year)
  )
  
  # Render the plot initially with all years selected
  output$Plot1 <- renderPlotly({
    gg_plot <- reactive_vals$filtered_data %>%
      ggplot(aes(x = year, y = term_total, fill = term_category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = term_total), 
                position = position_stack(vjust = 0.5), 
                size = 3, 
                color = "black") +
      scale_fill_manual(values = c("Fall" = "#FF9999", "Winter" = "#99CCFF", "Spring" = "#99FF99")) +
      labs(x = "Year", y = "Total Events", fill = "Term") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(gg_plot)
  })
  
  # Update the plot when the update button is pressed
  observeEvent(input$update_plot1, {
    # Get the selected years from the selectInput
    selected_years <- input$year_select_plot1
    
    # Filter the data based on the selected years
    if ("All" %in% selected_years) {
      reactive_vals$filtered_data <- event_summary
    } else {
      reactive_vals$filtered_data <- event_summary %>%
        filter(year %in% selected_years)
    }
  })
  
  # Reset the plot and year selection when the reset button is pressed
  observeEvent(input$reset_plot1, {
    # Reset the filtered data to all years
    reactive_vals$filtered_data <- event_summary
    
    # Update the year selectInput to select all years
    updateSelectInput(session, "year_select_plot1", selected = "All")
  })
  
  
  ############################### Plot 2 Overview ##############################
  renderPlot2fn1 <- function(data) {
    # Function to summarize and pivot data for each year
    summarize_and_pivot <- function(current_year) {
      data %>%
        filter(year == current_year) %>%
        group_by(term_category, support_level) %>%
        summarize(Support = n(), .groups = 'drop') %>%
        pivot_wider(names_from = support_level, values_from = Support, values_fill = list(Support = 0)) %>%
        mutate(year = current_year)  # Add a year column to identify the table
    }
    
    # List of years to iterate over
    years <- unique(data$year)
    
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
    
    # Create the facet plot
    ggplot2 <- ggplot(support_tables_melted, aes(x = term_category, y = Support, fill = support_level)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(Support, " (", round(percentage, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 2, color = "black") +
      facet_wrap(~ year) +
      labs(x = "Term", y = "Support Count", fill = "Support Level") +
      scale_fill_manual(values = c("H" = "#FF9999", "M" = "#99CCFF", "L" = "#99FF99", "NA" = "black")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(ggplot2)
  }
  
  renderPlot2fn2 <- function(data) {
    # Version with average audience count
    support_summary <- data %>%
      filter(support_level != "NA") %>%
      group_by(year, support_level) %>%
      summarize(
        total_support = n(),
        average_audience_count = mean(as.numeric(audience_count[!is.na(audience_count)]), na.rm = TRUE),
        .groups = 'drop'
      )
    
    ggplot_bubble_chart <- ggplot(support_summary, aes(x = year, y = average_audience_count, size = total_support, color = support_level)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(3, 20)) +
      scale_color_manual(values = c("H" = "#FF9999", "M" = "#99CCFF", "L" = "#99FF99", "NA" = "black")) +
      labs(x = "Year", y = "Average Audience Count", size = "Total Support", color = "Support Level") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(ggplot_bubble_chart)
  }
  
  # Initial Rendering
  output$Plot2 <- renderPlotly({
    renderPlot2fn1(combined_data_filtered)
  })
  
  # Update plot when update button is pressed
  observeEvent(input$update_plot2, {
    # Filter data based on selected years
    filtered_data <- combined_data_filtered %>%
      filter(year %in% input$year_select_plot2 | input$year_select_plot2 == "All")
    
    # Check if audience toggle is on or off
    if (input$toggle_audience) {
      output$Plot2 <- renderPlotly({
        renderPlot2fn2(filtered_data)
      })
    } else {
      output$Plot2 <- renderPlotly({
        renderPlot2fn1(filtered_data)
      })
    }
  })
  
  # Reset plot2 when reset button is pressed
  observeEvent(input$reset_plot2, {
    updateSelectInput(session, "year_select_plot2", selected = "All")
    updatePrettyToggle(session, "toggle_audience", value = FALSE)
    
    # Reset plot to default
    output$Plot2 <- renderPlotly({
      renderPlot2fn1(combined_data_filtered)
    })
  })
  ############################### Plot 3 Overview ##############################
  renderPlot3fn <- function(data) {
    # Function to summarize and pivot data for each year
    summarize_and_pivot_department <- function(current_year) {
      data %>%
        filter(year == current_year) %>%
        group_by(term_category, department_type) %>%
        summarize(DepartmentCount = n(), .groups = 'drop') %>%
        pivot_wider(names_from = department_type, values_from = DepartmentCount, values_fill = list(DepartmentCount = 0)) %>%
        mutate(year = current_year)  # Add a year column to identify the table
    }
    
    # List of years to iterate over
    years <- unique(data$year)
    
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
    ggplotly(ggplot_department_summary)
  }
  
  # Initial rendering of the plot
  output$Plot3 <- renderPlotly({
   renderPlot3fn(combined_data_filtered)
  })
  
  # Update plot when update button is pressed
  observeEvent(input$update_plot3, {
    if (input$year_select_plot3 == "All") {
      filtered_data <- combined_data_filtered
    } else {
      filtered_data <- combined_data_filtered %>%
        filter(year %in% input$year_select_plot3)
    }
    output$Plot3 <- renderPlotly({
      renderPlot3fn(filtered_data)
    })
  })
  
  # Reset plot3 when reset button is pressed
  observeEvent(input$reset_plot3, {
    updateSelectInput(session, "year_select_plot3", selected = "All")
    
    # Reset plot to default
    output$Plot3 <- renderPlotly({
      renderPlot3fn(combined_data_filtered)
    })
  })
  
  ############################### Plot 4 Overview ##############################
  renderPlot4fn1 <- function(data) {
    # Function to summarize and pivot data for each year
    summarize_and_pivot_event <- function(current_year) {
      data %>%
        filter(year == current_year,
               department_type == "MUSC") %>%
        group_by(term_category, event_type) %>%
        summarize(EventCount = n(), .groups = 'drop') %>%
        pivot_wider(names_from = event_type, values_from = EventCount, values_fill = list(EventCount = 0)) %>%
        mutate(year = current_year)  # Add a year column to identify the table
    }
    
    # List of years to iterate over
    years <- unique(data$year)
    
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
    
    ggplotly(ggplot_event_summary)
  }
  
  renderPlot4fn2 <- function(data) {
    # Function to summarize and pivot data for each year
    summarize_and_pivot_event <- function(current_year) {
      data %>%
        filter(year == current_year) %>%
        group_by(term_category, event_type) %>%
        summarize(EventCount = n(), .groups = 'drop') %>%
        pivot_wider(names_from = event_type, values_from = EventCount, values_fill = list(EventCount = 0)) %>%
        mutate(year = current_year)  # Add a year column to identify the table
    }
    
    # List of years to iterate over
    years <- unique(data$year)
    
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
    
    ggplotly(ggplot_event_summary)
  }
  
  # Initial Rendering
  output$Plot4 <- renderPlotly({
    renderPlot4fn1(combined_data_filtered)
  })
  
  # Update plot when update button is pressed
  observeEvent(input$update_plot4, {
    if (input$year_select_plot4 == "All") {
      filtered_data <- combined_data_filtered
      # Check if audience toggle is on or off
      if (input$toggle_music) {
        output$Plot4 <- renderPlotly({
          renderPlot4fn1(filtered_data)
        })
      } else {
        output$Plot4 <- renderPlotly({
          renderPlot4fn2(filtered_data)
        })
      }
    } else {
      filtered_data <- combined_data_filtered %>%
        filter(year %in% input$year_select_plot4)
      # Check if audience toggle is on or off
      if (input$toggle_music) {
        output$Plot4 <- renderPlotly({
          renderPlot4fn1(filtered_data)
        })
      } else {
        output$Plot4 <- renderPlotly({
          renderPlot4fn2(filtered_data)
        })
      }
    }
  })
  
  # Reset plot2 when reset button is pressed
  observeEvent(input$reset_plot4, {
    updateSelectInput(session, "year_select_plot4", selected = "All")
    updatePrettyToggle(session, "toggle_MUSC", value = TRUE)
    
    # Reset plot to default
    output$Plot4 <- renderPlotly({
      renderPlot4fn1(combined_data_filtered)
    })
  })
  
  ##################### Submit New Term Table #############
  
  output$term_table <- renderReactable({
    reactable(term_start_dates, 
              columns = list(
                term = colDef(name = "Term"),
                start_date = colDef(name = "Start Date", format = colFormat(date = TRUE))
              ),
              bordered = TRUE,
              highlight = TRUE,
              searchable = TRUE,
              defaultSorted = list(term = "asc"),
              striped = TRUE,
              pagination = TRUE
    )
  })
  
  ############################ Data Summary ####################################
  # Function to create the table with dynamic column exclusion
  render_table <- function(data, exclude = NULL) {
    # Define column definitions
    column_defs <- list(
      venue = colDef(name = "Venue"),
      year = colDef(name = "Year"),
      term_category = colDef(name = "Term Category"),
      term = colDef(name = "Term"),
      week_of_term = colDef(name = "Week of Term"),
      date = colDef(name = "Date"),
      what = colDef(name = "Event"),
      event_type = colDef(name = "Event Type", 
                          cell = function(value) {
                            color_map <- c("Guest" = "blue", "Faculty Recital" = "purple", "Student Recital" = "orange", "Studio Recital" = "grey", "Ensemble Concert" = "lightgrey", "Student Activity" = "pink", "Masterclass" = "cyan", "Presentation" = "lightblue", "Special Events" = "lightgreen")
                            color <- color_map[as.character(value)]
                            if (is.na(color)) color <- "lightgrey"
                            tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                          }),
      department = colDef(name = "Department"),
      department_type = colDef(name = "Department Type", 
                               cell = function(value) {
                                 color_map <- c("MUSC" = "blue", "ODOA" = "purple", "CSA" = "orange", "Collab" = "maroon", "Others" = "pink")
                                 color <- color_map[as.character(value)]
                                 if (is.na(color)) color <- "lightgrey"
                                 tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                               }),
      days_committed = colDef(name = "Days Committed", 
                              cell = data_bars(data, 
                                               fill_color = viridis::plasma(4), 
                                               background = '#F1F1F1', 
                                               text_position = 'outside-end', 
                                               number_fmt = scales::comma)),
      av_staff = colDef(name = "AV Staff", 
                        cell = data_bars(data, 
                                         fill_color = viridis::inferno(9), 
                                         fill_opacity = 0.8, 
                                         round_edges = TRUE, 
                                         text_position = 'outside-end', 
                                         number_fmt = scales::comma)),
      pac_staff = colDef(name = "PAC Staff", 
                         cell = data_bars(data, 
                                          fill_color = viridis::inferno(9), 
                                          fill_opacity = 0.8, 
                                          round_edges = TRUE, 
                                          text_position = 'outside-end', 
                                          number_fmt = scales::comma)),
      audience_count = colDef(name = "Audience Count",
                              cell = data_bars(data, 
                                               fill_color = viridis::turbo(5), 
                                               background = 'darkgrey', 
                                               border_style = 'solid', 
                                               border_width = '1px', 
                                               border_color = 'forestgreen', 
                                               box_shadow = TRUE, 
                                               text_position = 'inside-base', 
                                               number_fmt = scales::comma)),
      support_level = colDef(name = "Support Level",
                             cell = function(value) {
                               color_map <- c("H" = "lightcoral", "M" = "lightblue", "L" = "lightgreen")
                               color <- color_map[as.character(value)]
                               if (is.na(color)) color <- "lightgrey"
                               tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                             },
                             sortable = TRUE),
      audio_needs = colDef(name = "Audio Needs", 
                           cell = function(value) {
                             color_map <- c("H" = "lightcoral", "M" = "lightblue", "L" = "lightgreen")
                             color <- color_map[as.character(value)]
                             if (is.na(color)) color <- "lightgrey"
                             tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                           }),
      stage_needs = colDef(name = "Stage Needs", 
                           cell = function(value) {
                             color_map <- c("H" = "lightcoral", "M" = "lightblue", "L" = "lightgreen")
                             color <- color_map[as.character(value)]
                             if (is.na(color)) color <- "lightgrey"
                             tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                           }),
      lighting_needs = colDef(name = "Lighting Needs", 
                              cell = function(value) {
                                color_map <- c("H" = "lightcoral", "M" = "lightblue", "L" = "lightgreen")
                                color <- color_map[as.character(value)]
                                if (is.na(color)) color <- "lightgrey"
                                tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                              }),
      projection = colDef(name = "Projection", 
                          cell = function(value) {
                            color_map <- c("Y" = "green", "N" = "red")
                            color <- color_map[as.character(value)]
                            if (is.na(color)) color <- "lightgrey"
                            tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                          }),
      video_recording = colDef(name = "Video Recording", 
                               cell = function(value) {
                                 color_map <- c("Y" = "green", "N" = "red")
                                 color <- color_map[as.character(value)]
                                 if (is.na(color)) color <- "lightgrey"
                                 tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                               }),
      livestream = colDef(name = "Livestream", 
                          cell = function(value) {
                            color_map <- c("Y" = "green", "N" = "red")
                            color <- color_map[as.character(value)]
                            if (is.na(color)) color <- "lightgrey"
                            tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                          }),
      poster = colDef(name = "Poster", 
                      cell = function(value) {
                        color_map <- c("Y" = "green", "N" = "red")
                        color <- color_map[as.character(value)]
                        if (is.na(color)) color <- "lightgrey"
                        tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                      }),
      program = colDef(name = "Program", 
                       cell = function(value) {
                         color_map <- c("Y" = "green", "N" = "red")
                         color <- color_map[as.character(value)]
                         if (is.na(color)) color <- "lightgrey"
                         tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                       }),
      reception = colDef(name = "Reception", 
                         cell = function(value) {
                           color_map <- c("Y" = "green", "N" = "red")
                           color <- color_map[as.character(value)]
                           if (is.na(color)) color <- "lightgrey"
                           tags$span(style = paste0("background-color: ", color, "; padding: 2px 8px; border-radius: 12px; color: white; display: inline-block;"), as.character(value))
                         })
    )
    
    if (!is.null(exclude)) {
      # Hide selected columns
      for (col in exclude) {
        if (col %in% names(column_defs)) {
          column_defs[[col]]$show <- FALSE
        }
      }
    }
    
    # Create reactable
    reactable(
      data,
      theme = nytimes(),
      pagination = TRUE,
      columns = column_defs,
      bordered = TRUE,
      highlight = TRUE,
      striped = TRUE,
      searchable = TRUE,
      defaultSorted = list(audience_count = "desc")
    )
  }
  
  # Initial rendering of the table
  output$combined_table <- renderReactable({
    render_table(combined_data_filtered, exclude = c("days_committed", "poster", "program", "reception", "term", "department_type", "year", "term_category", "week_of_term"))
  })
  
  # Update table based on input$update_tab
  observeEvent(input$update_tab, {
    # Re-render the table with updated exclusions
    output$combined_table <- renderReactable({
      render_table(combined_data_filtered, exclude = input$exclude_tab_vars)
    })
  })
  
  # Reset the table to default
  observeEvent(input$reset_tab, {
    # Reset the exclude_vars input to its default state
    updateSelectInput(session, "exclude_vars", selected = c("days_committed", "poster", "program", "reception", "term", "department_type", "year", "term_category", "week_of_term"))
    
    # Re-render the table with default exclusions
    output$combined_table <- renderReactable({
      render_table(combined_data_filtered, exclude = c("days_committed", "poster", "program", "reception", "term", "department_type", "year", "term_category", "week_of_term"))
    })
  })
  
  ############################## TEMP 2 ##########################################
  observeEvent(input$submitNewTerm, {
    new_term <- input$termID
    new_start_date <- as.Date(input$termDate)
    
    # Use dplyr to append the new row
    term_start_dates$data <- term_start_dates$data %>%
      bind_rows(data.frame(term = new_term, start_date = new_start_date))
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