library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)

# Load data
ckd <- read_csv("IHME-GBD_CKD_20260130.csv")

# Clean data
ckd <- ckd %>%
  mutate(
    year = as.numeric(year),
    val = as.numeric(val),
    measure_name = as.character(measure_name),
    sex_name = as.character(sex_name),
    age_name = as.character(age_name),
    location_name = as.character(location_name),
    metric_name = as.character(metric_name),
    cause_name = as.character(cause_name)
  ) %>%
  filter(
    !is.na(year),
    !is.na(val),
    !is.na(measure_name),
    !is.na(sex_name),
    !is.na(age_name)
  )

# Optional filters to keep the app cleaner
# Adjust these only if needed
ckd <- ckd %>%
  filter(location_name == "China")

ui <- fluidPage(
  titlePanel("CKD Burden Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select parameters"),
      
      selectInput(
        "measure",
        "Measure:",
        choices = sort(unique(ckd$measure_name)),
        selected = sort(unique(ckd$measure_name))[1]
      ),
      
      selectInput(
        "sex",
        "Sex:",
        choices = sort(unique(ckd$sex_name)),
        selected = sort(unique(ckd$sex_name))[1]
      ),
      
      selectInput(
        "age_group",
        "Age group:",
        choices = sort(unique(ckd$age_name)),
        selected = sort(unique(ckd$age_name))[1]
      ),
      
      sliderInput(
        "year_range",
        "Year range:",
        min = min(ckd$year, na.rm = TRUE),
        max = max(ckd$year, na.rm = TRUE),
        value = c(min(ckd$year, na.rm = TRUE), max(ckd$year, na.rm = TRUE)),
        sep = ""
      ),
      
      radioButtons(
        "plot_type",
        "Plot type:",
        choices = c("Line plot", "Bar plot"),
        selected = "Line plot"
      ),
      
      actionButton("update", "Update View")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Trend Plot",
          br(),
          plotOutput("trend_plot", height = "450px"),
          br(),
          verbatimTextOutput("summary_text")
        ),
        
        tabPanel(
          "Summary Table",
          br(),
          DTOutput("summary_table")
        ),
        
        tabPanel(
          "About & References",
          br(),
          h4("App Goal"),
          p("This app was developed to explore temporal trends in chronic kidney disease (CKD) burden in China by measure, sex, age group, and year."),
          
          h4("Broad Context"),
          p("Chronic kidney disease (CKD) is an important public health problem in China. Examining variation across sex, age group, and time can help users better understand patterns in disease burden."),
          
          h4("Data Source"),
          p("This project uses data from the Global Burden of Disease(GBD) 2023 databases. The complete data set and accompanying information are available at [vizhub.healthdata.org/gbd-results](https://vizhub.healthdata.org/gbd-results/)"),
          
          h4("Methods"),
          p("The app provides interactive filtering and descriptive visualization of CKD burden estimates. Line plots display temporal patterns, and the shaded band represents the uncertainty interval when available."),
          
          h4("Repository"),
          p("Code repository: https://github.com/JingxinYuan/VTPEH6270_Jingxin-Yuan.git"),
          
          h4("AI Disclosure"),
          p("AI tools were used to support coding troubleshooting and interface refinement. All final decisions, code checking, and interpretation were reviewed by the author.")
        )
      )
    )
  )
)

# Serve
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    ckd %>%
      filter(
        measure_name == input$measure,
        sex_name == input$sex,
        age_name == input$age_group,
        year >= input$year_range[1],
        year <= input$year_range[2]
      ) %>%
      arrange(year)
  })
  
  output$trend_plot <- renderPlot({
    dat <- filtered_data()
    req(nrow(dat) > 0)
    
    if (input$plot_type == "Line plot") {
      ggplot(dat, aes(x = year, y = val)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_x_continuous(
          breaks = seq(min(dat$year),max(dat$year),by = 1)
        ) +
        labs(
          title = paste("CKD Trend:", input$measure),
          subtitle = paste("Sex:", input$sex, "| Age group:", input$age_group),
          x = "Year",
          y = input$measure
        ) +
        theme_minimal(base_size = 14)
    } else {
      ggplot(dat, aes(x = factor(year), y = val)) +
        geom_col() +
        labs(
          title = paste("CKD Trend:", input$measure),
          subtitle = paste("Sex:", input$sex, "| Age group:", input$age_group),
          x = "Year",
          y = input$measure
        ) +
        theme_minimal(base_size = 14)
    }
  })
  
  output$summary_table <- renderDT({
    dat <- filtered_data()
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$summary_text <- renderText({
    dat <- filtered_data()
    req(nrow(dat) > 0)
    
    paste0(
      "Rows returned: ", nrow(dat), "\n",
      "Selected measure: ", input$measure, "\n",
      "Selected sex: ", input$sex, "\n",
      "Selected age group: ", input$age_group, "\n",
      "Year range: ", input$year_range[1], " to ", input$year_range[2], "\n",
      "Years in filtered data: ", paste(range(dat$year), collapse = " to "), "\n",
      "Average value: ", round(mean(dat$val, na.rm = TRUE), 2)
    )
  })
}

shinyApp(ui = ui, server = server)