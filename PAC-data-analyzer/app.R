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
remotes::install_github("timelyportfolio/dataui")

################################ DATA WRANGLING ################################





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
          badgeColor = "lime"
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
          h3('Exploratory Data Analysis'),
          tabsetPanel(id = "tabs",
                      tabPanel("Life Expectancy by Countries", 
                               fluidRow(
                                 column(
                                   width = 8,
                                   shinycssloaders::withSpinner(plotlyOutput("Plot1"))
                                 ),
                                 column(
                                   width = 4,
                                   box(
                                     width = 12,
                                     title = "Data Exploration", 
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
                                       h4("Explore the trends in life expectancy across different countries."),
                                       br(),
                                       h4("Use the gear in the top right corner to select the countries used for the plot, 
                                          and use the slider below the plot to navigate through the years.")
                                     )
                                   )
                                 )
                               )
                      ),
                      tabPanel("Life Expectancy vs. Something",
                               fluidRow(
                                 column(
                                   width = 8,
                                   shinycssloaders::withSpinner(plotlyOutput("Plot2"))
                                 ),
                                 column(
                                   width = 4,
                                   box(
                                     width = 12,
                                     title = "Data Exploration", 
                                     closable = FALSE, 
                                     status = "warning", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     dropdownMenu = boxDropdown(
                                       boxDropdownItem("Click me", id = "play2", icon = icon("heart")),
                                       tags$div(id = "audio_container2"),
                                     ), 
                                     div(
                                       h1("Data Exploration", align = "center", style = "font-weight:bold"),
                                       br(),
                                       h4("Explore the relation between life expectancy and other variables 
                                          across different countries."),
                                       br(),
                                       h4("Use the gear in the top right corner to select the variable used for the plot, 
                                          and use the slider below the plot to navigate through the years")
                                     )
                                   )
                                 )
                               )
                      )
          )
        ),
        tabItem(
          tabName = "results",
          h3('Machine Learning Results'),
          tabsetPanel(id = "tabs2",
                      tabPanel("K-Means Classification", 
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
                                       h1("k-Means Elbow Graph", align = "center", style = "font-weight:bold"),
                                       br(),
                                       h4("To begin, we created a K-means clustering model to classify the countries
                                          into groups with similar life expectancies and features. We started the process by
                                          experimenting with a different number of centers to find the optimal value. This
                                          is the elbow graph produced by this process."),
                                       br(),
                                       h4("The graph shows the elbow to be between 4 and 8 centers. We decided to proceed
                                          with 5 centers since it gives a good tradeoff of model performance and time taken.")
                                     )
                                   )
                                 )
                               )
                      ),
                      tabPanel("Random Forest Variable Importance",
                               fluidRow(
                                 column(
                                   width = 8,
                                   shinycssloaders::withSpinner(imageOutput("importance_plot_init"))
                                 ),
                                 column(
                                   width = 4,
                                   box(
                                     width = 12,
                                     title = "Random Forest Analysis", 
                                     closable = FALSE, 
                                     status = "warning", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     dropdownMenu = boxDropdown(
                                       boxDropdownItem("Click me", id = "play4", icon = icon("heart")),
                                       tags$div(id = "audio_container4"),
                                     ), 
                                     div(
                                       h1("Random Forest Analysis", align = "center", style = "font-weight:bold"),
                                       br(),
                                       h4("The graph shows what the most important variables are in the decisions
                                          made to classify a country within their life expectancy group. Unsurpringly
                                          the most important variable is adult_mortaility. However, the next two variables,
                                          income composition of resources and GDP per capita, and the ninth most important
                                          variable, percentage expenditure, are much more interesting as they are not 
                                          health-related and they provide insight into the correlation between wealth 
                                          and life expectancy.Furthermore, the next vairable is relate to education, 
                                          which is also not healt related, but it suggests that higher levels of education
                                          could lead to a higher life expectancy. The other five variables are related to 
                                          health issues."),
                                       br(),
                                       h4("Given this results, we suggest exploring the association between life expectancy
                                          and wealth, and education.")
                                     )
                                   )
                                 )
                               )
                      ),
                      tabPanel("Decision Tree",
                               fluidRow(
                                 column(
                                   width = 8,
                                   shinycssloaders::withSpinner(imageOutput("tree_plot_init"))
                                 ),
                                 column(
                                   width = 4,
                                   box(
                                     width = 12,
                                     title = "Decision Tree", 
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
                                       h4("The decision tree furhter confirms that the dataset presents a positive correlation
                                          between life expectancy and welath and education. It also further confirms that
                                          countries with lower indeces of public helath problems, such as hiv, and death of
                                          kids under the age of 5, lead to higher life expectancy.")
                                     )
                                   )
                                 )
                               )
                      )
          )
        ),
        tabItem(
          tabName = "contact",
          h2("Don't contact us!"),
          br(),
          box(
            width = 10,
            title = "But if you really want to contact us", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE, 
            collapsed = TRUE,
            "You can try either one or both of our software development team:", 
            br(), 
            br(),
            fluidRow(
              box(title = "Alejandro González", status = "primary", "gonzaleza@carleton.edu"),
              box(title = "Kunwu Lyu", status = "primary", "lyuk@carleton.edu")
            ),
            br(),
            "or the person who taught us how to do this:",
            fluidRow(box(title = "Deepak Bastola", status = "primary", "dbastola@carleton.edu")), 
            br(),
            "and we'll get back to you when we're done with finals."
          )
        ),
        tabItem(
          tabName = "readme",
          div(
            h1("Welcome to Life Expectancy Analysis", align = "center", style = "font-weight:bold"),
            br(),
            h4("Navigate to the Data tab to see the visualizations and the trends we found!"),
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
  
  
  # output$kmeans_plot <- renderPlot({
  #   kmeans_plot <- multi_kmeans %>%
  #     ggplot(aes(k,tot.withinss)) +
  #     geom_point() +
  #     geom_line()+
  #     scale_x_continuous(breaks = 1:20) 
  #   kmeans_plot
  # }) %>% 
  #   bindCache()
  # 
  # rf_results <- reactive({
  #   plot_variable_importance(ml_data, ml_outcome)
  # })
  # 
  # output$importance_plot <- renderPlot({
  #   rf_results()$importance_plot
  # }) %>% 
  #   bindCache()
  # 
  # output$tree_plot <- renderPlot({
  #   important_vars <- c("income_composition_of_resources", "adult_mortality", "gdp_pcap", "schooling")
  #   tree_results <- tune_and_fit_decision_tree(ml_data, important_vars, ml_outcome)
  #   rpart.plot(tree_results$tree_fit$fit, roundint = FALSE)
  # }) %>%
  #   bindCache()
  
  
  
  
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
