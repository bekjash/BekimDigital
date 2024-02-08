# Load required libraries
pacman::p_load(
  rio,          # for importing data
  here,         # for file paths
  janitor,      # for data cleaning
  lubridate,    # for working with dates
  tidyverse,    # for data management
  shiny,
  dplyr,        # for modifying column names
  ggplot2,      # for plotting and making graphs
  plotly        # adding pie chart and other plot functions 
)

# Read XLSX file and store it in linelist_raw
linelist_raw <- readxl::read_xlsx(here("dataset", "dataset_olderpersons.xlsx"))

# Remove columns 2 and 3 (ISO Code and Data source category) as they are not needed in this project
linelist <- linelist_raw %>% select(-2, -3)

# Fix column names
col_names <- linelist[1, ]
names(linelist) <- col_names
linelist <- linelist[-1, ]
colnames(linelist) <- str_to_title(gsub(" ", "_", colnames(linelist)))

# Extract initial 4 columns (country, date, age, sex) that are constant for each row of values
initial_columns <- linelist[, 1:4]

# Define additional columns for each separate data section (ds) to be organized separately on each tab
ds1 <- bind_cols(initial_columns, linelist[, 5:8]) # Columns pertaining to Older persons by household size (percentage)
selected_columns1 <- 5:8
ds1_subset <- ds1[, c(1, selected_columns1)]
ds1_subset[, 2:5] <- lapply(ds1_subset[, 2:5], as.numeric)

ds2 <- bind_cols(initial_columns, linelist[, 10:13]) # Columns pertaining to Older persons by age of the head of household (percentage)
selected_columns2 <- 5:8
ds2_subset <- ds2[, c(1, selected_columns2)]
ds2_subset[, 2:5] <- lapply(ds2_subset[, 2:5], as.numeric)

ds3 <- bind_cols(initial_columns, linelist[, 14:20]) # Columns pertaining to Older persons by basic household type (percentage)
selected_columns3 <- 5:11
ds3_subset <- ds3[, c(1, selected_columns3)]
ds3_subset[, 2:8] <- lapply(ds3_subset[, 2:8], as.numeric)

ds4 <- bind_cols(initial_columns, linelist[, 21:24]) # Columns pertaining to Intergenerational household types (percentage of older persons)
selected_columns4 <- 5:8
ds4_subset <- ds4[, c(1, selected_columns4)]
ds4_subset[, 2:5] <- lapply(ds4_subset[, 2:5], as.numeric)

# Define colors for graphs
piegraph_colors <- c("steelblue", "seagreen", "darkorange", "firebrick", "darkorchid")
bargraph_colors_1 <- c("steelblue", "seagreen", "darkorange", "firebrick")
bargraph_colors_2 <- c("steelblue", "seagreen", "darkorange", "firebrick", "darkorchid", "powderblue", "darkgoldenrod")
bargraph_colors_3 <- c("steelblue", "seagreen", "darkorange", "firebrick")

# Define UI
ui <- fluidPage(
  titlePanel("Understanding the Living Arrangements and Socioeconomic Status of Older Adults in the 8 countries in the Balkan Region"),
  mainPanel(
    navbarPage(
      "Navigation Bar:",  # Navbar title
      tabPanel("Homepage", 
               fluidPage(tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                         textOutput("maintext"))
      ),
      tabPanel("Household Size", 
               fluidRow(
                 column(6, selectInput("country1", "Select Balkan Country", choices = unique(ds1_subset$Country))),
                 column(8,  # Use the full width of the column
                        plotlyOutput("pie_chart", height = "500px", width = "800px"))  # Adjust height and width as needed
               ),
               column(12,  # Use the full width of the column
                      fluidRow(
                        tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                        textOutput("bodytext1")
                      ))
      ),
      tabPanel("Age of Head of Household", 
               fluidRow(
                 column(6, selectInput("country2", "Select Balkan Country", choices = unique(ds2_subset$Country))),
                 column(8,  # Use the full width of the column
                        plotOutput("bar_plot_1", height = "500px", width = "800px")),
                 column(12,  # Use the full width of the column
                        fluidRow(
                          tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                          textOutput("bodytext2")
                        ))
               )),
      tabPanel("Basic Household Type", 
               fluidRow(
                 column(6, selectInput("country3", "Select Balkan Country", choices = unique(ds3_subset$Country))),
                 column(8,  # Use the full width of the column
                        plotOutput("bar_plot_2", height = "500px", width = "800px")),
                 
                 column(12,  # Use the full width of the column
                        fluidRow(
                          tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                          textOutput("bodytext3")
                        ))
               )),
      tabPanel("Intergenerational Household Type", 
               fluidRow(
                 column(6, selectInput("country4", "Select Balkan Country", choices = unique(ds4_subset$Country))),
                 column(8,  # Use the full width of the column
                        plotOutput("bar_plot_3", height = "500px", width = "800px")),
                 column(12,  # Use the full width of the column
                        fluidRow(
                          tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                          textOutput("bodytext4")
                        ))
               ))
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Display main page text
  output$maintext <- renderPrint({
    cat(paste0("This visualization data project aims to shed light on understanding the Living Arrangements and their relation to the Socioeconomic Status of older adults in the eight countries in the Balkan Region. Data in this report is publicly available and provided by the United Nations (UN) Population Division's Department for the Living Arrangements of Older Persons. The insights gained from these visualizations can be used to inform policies and interventions aimed at improving the quality of life for older adults in the Balkan region.")
    )})
  
  # Server code for Household size tab
  selected_country_ds1 <- reactive({
    filter(ds1_subset, Country == input$country1)
  })
  
  # Create a summary data frame with counts
  summary_ds1 <- reactive({
    selected_country_ds1() %>%
      gather(variable, value, -Country) %>%
      group_by(variable, value) %>%
      summarize(count = n(), .groups = 'drop') %>%
      mutate(percentage = value / sum(value) * 100)
  })
  
  # Display the pie chart
  output$pie_chart <- renderPlotly({
    req(nrow(selected_country_ds1()) > 0)  # Check if selected_country_data is not empty
    
    pie_chart <- plot_ly(
      labels = ~variable,
      values = ~percentage,  
      data = summary_ds1(),
      type = "pie",
      marker = list(colors = piegraph_colors)
    ) %>%
      layout(title = paste("Older persons by household size in", input$country1)) %>%
      config(displayModeBar = FALSE)  # Remove the plotly modebar
  })
  
  # Display Household Size tab text
  output$bodytext1 <- renderPrint({
    cat(paste0("Number of people in the household of older adults.")
    )})
  
  # Server code for Age of Head of Household tab
  selected_country_ds2 <- reactive({
    filter(ds2_subset, Country == input$country2)
  })
  
  # Create a summary data frame with counts
  summary_ds2 <- reactive({
    selected_country_ds2() %>%
      gather(variable, value, -Country) %>%
      group_by(variable, value) %>%
      summarize(count = n(), .groups = 'drop')
  })
  
  # Display the bar plot
  output$bar_plot_1 <- renderPlot({
    ggplot(summary_ds2(), aes(x = variable, y = value, fill = as.factor(value))) +
      geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.5) +
      geom_col(position = "dodge", fill = bargraph_colors_1) +
      labs(title = paste("Older persons by age of head of household in", input$country2),
           x = "Age of Head of Household",
           y = "Percentage") +
      theme_classic() +
      guides(fill = guide_legend(title = "Percentage Values")) +
      ylim(0, 100)  
  })
  
  # Display Age of Head of Household tab text
  output$bodytext2 <- renderPrint({
    cat(paste0("Age distribution of household heads.")
    )})
  
  # Server code for Basic Household Type tab
  selected_country_ds3 <- reactive({
    filter(ds3_subset, Country == input$country3)
  })
  
  # Create a summary data frame with counts
  summary_ds3 <- reactive({
    selected_country_ds3() %>%
      gather(variable, value, -Country) %>%
      group_by(variable, value) %>%
      summarize(count = n(), .groups = 'drop')
  })
  
  # Display the bar plot
  output$bar_plot_2 <- renderPlot({
    ggplot(summary_ds3(), aes(x = variable, y = value, fill = as.factor(value))) +
      geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.5) +
      geom_col(position = "dodge", fill = bargraph_colors_2) +
      labs(title = paste("Older persons by basic household type in", input$country3),
           x = "Household Type",
           y = "Percentage") +
      theme_classic() +
      guides(fill = guide_legend(title = "Percentage Values")) +
      ylim(0, 100)  
  })
  
  # Display Basic Household Type tab text
  output$bodytext3 <- renderPrint({
    cat(paste0("Characteristics of basic household types.")
    )})
  
  # Server code for Intergenerational Household Type tab
  selected_country_ds4 <- reactive({
    filter(ds4_subset, Country == input$country4)
  })
  
  # Create a summary data frame with counts
  summary_ds4 <- reactive({
    selected_country_ds4() %>%
      gather(variable, value, -Country) %>%
      group_by(variable, value) %>%
      summarize(count = n(), .groups = 'drop')
  })
  
  # Display the bar plot
  output$bar_plot_3 <- renderPlot({
    ggplot(summary_ds4(), aes(x = variable, y = value, fill = as.factor(value))) +
      geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.5) +
      geom_col(position = "dodge", fill = bargraph_colors_3) +
      labs(title = paste("Older persons by intergenerational household type in", input$country4),
           x = "Household Type",
           y = "Percentage") +
      theme_classic() +
      guides(fill = guide_legend(title = "Percentage Values")) +
      ylim(0, 100)
  })
  
  # Display Intergenerational Household Type tab text
  output$bodytext4 <- renderPrint({
    cat(paste0("Household composition by intergenerational type.")
    )})
}

# Run the application

shinyApp(ui, server)

