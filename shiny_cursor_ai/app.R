#install.packages("remotes")
#remotes::install_github("RinteRface/shinydashboardPlus") 

# Load required packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(plotly)

# Helper: Read and tidy a single CSV
read_and_tidy <- function(file, indicator, age_group, sex) {
  df <- read_csv(file)
  df_long <- df %>%
    pivot_longer(-country, names_to = "year", values_to = "value") %>%
    mutate(
      year = as.integer(year),
      indicator = indicator,
      age_group = age_group,
      sex = sex
    )
  return(df_long)
}

# File metadata for all 8 indicators
file_info <- tibble(
  file = c(
    "data/data_1_literacy_rate_adult_female_percent_of_females_ages_15_above.csv",
    "data/data_2_literacy_rate_adult_male_percent_of_males_ages_15_and_above.csv",
    "data/data_3_literacy_rate_adult_total_percent_of_people_ages_15_and_above.csv",
    "data/data_4_literacy_rate_adult.csv",
    "data/data_5_literacy_rate_youth.csv",
    "data/data_6_literacy_rate_youth_female_percent_of_females_ages_15_24.csv",
    "data/data_7_literacy_rate_youth_male_percent_of_males_ages_15_24.csv",
    "data/data_8_literacy_rate_youth_total_percent_of_people_ages_15_24.csv"
  ),
  indicator = c(
    "Adult Literacy Rate (Female)",
    "Adult Literacy Rate (Male)",
    "Adult Literacy Rate (Total)",
    "Adult Literacy Rate (All)",
    "Youth Literacy Rate (All)",
    "Youth Literacy Rate (Female)",
    "Youth Literacy Rate (Male)",
    "Youth Literacy Rate (Total)"
  ),
  age_group = c(
    "Adult", "Adult", "Adult", "Adult", "Youth", "Youth", "Youth", "Youth"
  ),
  sex = c(
    "Female", "Male", "Total", "All", "All", "Female", "Male", "Total"
  )
)

# Read and combine all data
data_list <- pmap(file_info, read_and_tidy)
all_data <- bind_rows(data_list) %>%
  filter(!is.na(value))

# Custom CSS for modern look
custom_css <- "
body { background-color: #f6f8fa; font-family: 'Montserrat', 'Roboto', sans-serif; }
.logo { font-family: 'Montserrat', 'Roboto', sans-serif; font-size: 2rem; font-weight: bold; color: #2e7d67; letter-spacing: 1px; }
.box, .value-box, .box-solid, .small-box { border-radius: 12px !important; box-shadow: 0 2px 8px rgba(44,62,80,0.07); }
.value-box, .small-box { font-size: 1.5rem; }
.sidebar { background: #2e7d67 !important; }
.main-header .logo { background: #2e7d67 !important; color: #fff !important; }
.main-header .navbar { background: #fff !important; }
.controlbar { background: #fff !important; }
"  

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      span(class = "logo", "Global Literacy ", span(style = "color:#2e7d67;", "Literacy Dashboard")),
      tags$head(
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Montserrat:400,700|Roboto:400,700&display=swap"),
        tags$style(HTML(custom_css))
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
        fluidRow(
          column(3, selectInput("year", "Year",
            choices = seq(min(all_data$year, na.rm=TRUE), max(all_data$year, na.rm=TRUE), by=1),
            selected = seq(min(all_data$year, na.rm=TRUE), max(all_data$year, na.rm=TRUE), by=1),
            multiple = TRUE)),
          column(3, selectInput("age_group", "Age Group", choices = unique(all_data$age_group))),
          column(3, selectInput("sex", "Sex", choices = unique(all_data$sex))),
          column(3, uiOutput("country_ui"))
        ),
        br(),
        fluidRow(
          valueBoxOutput("meanBox"),
          valueBoxOutput("minBox"),
          valueBoxOutput("maxBox")
        ),
        fluidRow(
          box(width = 6, plotlyOutput("main_plot", height = 400)),
          box(width = 6, plotlyOutput("world_map", height = 400))
        )
      ),
      tabItem(tabName = "about",
        box(width = 12, title = "About This Dashboard", status = "success", solidHeader = TRUE,
          p("This dashboard allows interactive exploration of global literacy indicators by country, year, age group, and sex. Data is sourced from Gapminder and the World Bank."),
          p("For more information and to access the original data, visit ",
            a("Gapminder Data", href = "https://www.gapminder.org/data/", target = "_blank"), ".")
        )
      ),
      tabItem(tabName = "data",
        box(width = 12, title = "Download and Explore Data", status = "success", solidHeader = TRUE,
          downloadButton("download_data", "Download Filtered Data"),
          br(), br(),
          DTOutput("data_table")
        )
      )
    )
  ),
  title = "Shiny Literacy Dashboard"
)

# Server
server <- function(input, output, session) {
  # Dynamic country choices
  output$country_ui <- renderUI({
    filtered <- all_data %>%
      filter(
        age_group == input$age_group,
        sex == input$sex
      )
    countries <- sort(unique(filtered$country))
    selectInput("country", "Country", choices = countries, selected = "Afghanistan", multiple = TRUE)
  })

  # Reactive filtered data
  filtered_data <- reactive({
    req(input$age_group, input$sex, input$country, input$year)
    all_data %>%
      filter(
        age_group == input$age_group,
        sex == input$sex,
        country %in% input$country,
        year %in% as.integer(input$year)
      )
  })

  # Main bar plot
  output$main_plot <- renderPlotly({
    df <- filtered_data()
    df$year <- as.factor(df$year)
    p <- ggplot(df, aes(x = year, y = value, fill = country, group = country, text = paste('Country:', country, '<br>Year:', year, '<br>Literacy Rate:', value))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Year", y = "Literacy Rate (%)", fill = "Country") +
      theme_minimal()
    ggplotly(p, tooltip = c("text"))
  })

  # World map
  output$world_map <- renderPlotly({
    df <- filtered_data() %>%
      group_by(country) %>%
      summarise(value = mean(value, na.rm = TRUE))
    # Only highlight selected countries, others are light gray
    all_countries <- unique(all_data$country)
    map_df <- tibble(country = all_countries) %>%
      left_join(df, by = "country")
    map_df$highlight <- ifelse(!is.na(map_df$value), 1, 0)
    map_df$value[is.na(map_df$value)] <- 0
    plot_geo(map_df) %>%
      add_trace(
        z = ~highlight,
        text = ~country,
        locations = ~country,
        locationmode = 'country names',
        colors = c('#e5ecf6', '#39536b'), # light gray for unselected, dark blue for selected
        marker = list(line = list(color = 'rgb(40,40,40)', width = 0.5)),
        showscale = FALSE
      ) %>%
      layout(
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = 'natural earth'),
          bgcolor = 'rgba(0,0,0,0)'
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )
  })

  # Summary statistics
  output$meanBox <- renderValueBox({
    df <- filtered_data()
    valueBox(round(mean(df$value, na.rm=TRUE), 1), "Mean Literacy Rate", icon = icon("chart-line"), color = "aqua")
  })
  output$minBox <- renderValueBox({
    df <- filtered_data()
    valueBox(round(min(df$value, na.rm=TRUE), 1), "Min Literacy Rate", icon = icon("arrow-down"), color = "yellow")
  })
  output$maxBox <- renderValueBox({
    df <- filtered_data()
    valueBox(round(max(df$value, na.rm=TRUE), 1), "Max Literacy Rate", icon = icon("arrow-up"), color = "green")
  })

  # Data table
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 20, scrollX = TRUE))
  })

  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("literacy_data_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server) 