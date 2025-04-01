# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)

# Read the movies data from a CSV file
movies_data <- read.csv("netflix.csv")

# Convert 'premiere' column to Date format and extract 'year'
movies_data$premiere <- as.Date(movies_data$premiere, format = "%m/%d/%Y")
movies_data$year <- format(as.Date(movies_data$premiere, format = "%m/%d/%Y"), "%Y")

# Define the UI layout
ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Netflix Data-Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Movie Analysis", tabName = "Home"),
      menuItem("Genre wise Analysis 1", tabName = "Genre1"),
      menuItem("Genre wise Analysis 2", tabName = "Genre2"),
      menuItem("Language wise Analysis 1", tabName = "Language1"),
      menuItem("Language wise Analysis 2", tabName = "Language2"),
      menuItem("Date wise Analysis", tabName = "date1"),
      menuItem("Year wise Analysis", tabName = "year"),
      menuItem("Project Report", tabName = "project")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Home", # Home tab
              fluidPage(
                titlePanel("Top 5 Movies by IMDb Score"),
                dataTableOutput("topMovies"),
                titlePanel("Lowest 5 Movies by IMDb Score"),
                dataTableOutput("lowestMovies"),
                downloadButton("downTopMovies", "Download Top Movies"),
                downloadButton("downLowestMovies", "Download Lowest Movies")
              )),
      tabItem("Genre1", # Genre wise Analysis 1 tab
              fluidPage(
                selectInput("genreInput1", "Choose a genre:", choices = unique(movies_data$genre)),
                tableOutput("filteredTable1"),
                downloadButton("downFilteredTable1", "Download Table")
              )
      ),
      tabItem("Genre2", # Genre wise Analysis 2 tab
              fluidPage(
                selectInput("genreInput2", "Choose a genre:", choices = unique(movies_data$genre)),
                radioButtons("colorInput", "Choose a color:", choices = c("Red" = "red", "Light Blue" = "lightblue", "Green" = "green", "Yellow" = "yellow")),
                plotOutput("languageHistogram"),
                downloadButton("downLanguageHistogram", "Download Language Histogram")
              )
      ),
      tabItem("Language1", # Language wise Analysis 1 tab
              fluidPage(
                titlePanel("Language Distribution Pie Chart"),
                plotOutput("pieChart"),
                downloadButton("downPieChart", "Download Pie Chart")
              )),
      tabItem("Language2", # Language wise Analysis 2 tab
              fluidPage(
                selectInput("languageInput", "Select Language:", choices = unique(movies_data$language)),
                tableOutput("dataDisplay"),
                downloadButton("downDataDisplay", "Download Data")
              )),
      tabItem("date1", # Date wise Analysis tab
              fluidPage(
                titlePanel("Date-wise Analysis"),
                dateRangeInput("dateRange", "Select Date Range:",
                               start = min(movies_data$premiere), end = max(movies_data$premiere)),
                tableOutput("dateAnalysis"),
                downloadButton("downDateAnalysis", "Download Table")
              )),
      tabItem("year", # Year wise Analysis tab
              fluidPage(
                titlePanel("Year-wise Movie Releases"),
                plotOutput("yearlyGraph"),
                downloadButton("downYearlyGraph", "Download Yearly Graph")
              )),
      tabItem("project", # Project Report tab
              fluidPage(
                titlePanel("Project Report"),
                tags$iframe(style="height:600px; width:100%; scrolling=yes",
                            src="https://online.publuu.com/475682/1074379")
              ))
    )
  )
)

# Define the server logic
server <- function(input, output){
  
  # Function to create download handlers
  createDownloadHandler <- function(dataToDownload, filename, extension) {
    downloadHandler(
      filename = function() { paste(filename, Sys.Date(), ".", extension, sep = "") },
      content = function(file) {
        switch(extension,
               "csv" = write.csv(dataToDownload(), file, row.names = FALSE),
               "png" = ggsave(file, plot = dataToDownload(), device = "png"),
               stop("Invalid file extension")
        )
      }
    )
  }
  
  # Render top 5 movies table
  output$topMovies <- renderDataTable({
    movies_data %>%
      arrange(desc(imdb_score)) %>%
      head(5)
  })
  
  # Render lowest 5 movies table
  output$lowestMovies <- renderDataTable({
    movies_data %>%
      arrange(imdb_score) %>%
      head(5)
  })
  
  # Download handler for top movies
  output$downTopMovies <- createDownloadHandler(
    dataToDownload = function() {
      movies_data %>%
        arrange(desc(imdb_score)) %>%
        head(5)
    },
    filename = "Top_Movies",
    extension = "csv"
  )
  
  # Download handler for lowest movies
  output$downLowestMovies <- createDownloadHandler(
    dataToDownload = function() {
      movies_data %>%
        arrange(imdb_score) %>%
        head(5)
    },
    filename = "Lowest_Movies",
    extension = "csv"
  )
  
  # Render table based on selected genre for Genre wise Analysis 1 tab
  output$filteredTable1 <- renderTable({
    filtered_movies <- movies_data[movies_data$genre == input$genreInput1, ]
    filtered_movies
  })
  
  # Render language histogram based on selected genre for Genre wise Analysis 2 tab
  output$languageHistogram <- renderPlot({
    filtered_movies <- movies_data[movies_data$genre == input$genreInput2, ]
    ggplot(filtered_movies, aes(x = language, fill = input$colorInput)) +
      geom_histogram(stat = "count", color = "black") +
      theme_minimal() +
      labs(title = "", x = "Language", y = "No of Movies") +
      scale_fill_identity()
  })
  
  # Render pie chart for language distribution for Language wise Analysis 1 tab
  output$pieChart <- renderPlot({
    language_counts <- table(movies_data$language)
    language_data <- as.data.frame(language_counts)
    top_languages <- head(language_data[order(-language_data$Freq), ], 6)
    ggplot(top_languages, aes(x = "", y = Freq, fill = Var1)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "Language", y = "Number of Movies", x = "") +
      scale_fill_brewer(palette = "Set3")
  })
  
  # Render table based on selected language for Language wise Analysis 2 tab
  output$dataDisplay <- renderTable({
    filtered_data <- movies_data[movies_data$language == input$languageInput, ]
    filtered_data
  })
  
  # Render table based on selected date range for Date wise Analysis tab
  output$dateAnalysis <- renderTable({
    filtered_data <- movies_data %>%
      filter(premiere >= input$dateRange[1] & premiere <= input$dateRange[2]) %>%
      mutate(premiere = format(premiere, "%m/%d/%Y"))
    filtered_data
  })
  
  # Render yearly graph for Year wise Analysis tab
  output$yearlyGraph <- renderPlot({
    yearly_data <- movies_data %>%
      group_by(year) %>%
      summarise(count = n()) %>%
      arrange(year)
    ggplot(yearly_data, aes(x = as.numeric(year), y = count)) +
      geom_line() +
      labs(title = "Movies Released Per Year",
           x = "Year",
           y = "Number of Movies") +
      theme_minimal()
  })
  
  # Download handler for filtered table based on genre
  output$downFilteredTable1 <- createDownloadHandler(
    dataToDownload = function() {
      filtered_movies <- movies_data[movies_data$genre == input$genreInput1, ]
      filtered_movies
    },
    filename = "Filtered_Table_Genre1",
    extension = "csv"
  )
  
  # Download handler for language histogram
  output$downLanguageHistogram <- createDownloadHandler(
    dataToDownload = function() {
      filtered_movies <- movies_data[movies_data$genre == input$genreInput2, ]
      p <- ggplot(filtered_movies, aes(x = language, fill = input$colorInput)) +
        geom_histogram(stat = "count", color = "black") +
        theme_minimal() +
        labs(title = "", x = "Language", y = "No of Movies") +
        scale_fill_identity()
      p
    },
    filename = "Language_Histogram_Genre2",
    extension = "png" # Change extension according to the file type
  )
  
  # Download handler for pie chart
  output$downPieChart <- createDownloadHandler(
    dataToDownload = function() {
      language_counts <- table(movies_data$language)
      language_data <- as.data.frame(language_counts)
      top_languages <- head(language_data[order(-language_data$Freq), ], 6)
      p <- ggplot(top_languages, aes(x = "", y = Freq, fill = Var1)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(fill = "Language", y = "Number of Movies", x = "") +
        scale_fill_brewer(palette = "Set3")
      p
    },
    filename = "Language_Pie_Chart",
    extension = "png"
  )
  
  # Download handler for filtered data display based on language
  output$downDataDisplay <- createDownloadHandler(
    dataToDownload = function() {
      filtered_data <- movies_data[movies_data$language == input$languageInput, ]
      filtered_data
    },
    filename = "Filtered_Data_Display_Language2",
    extension = "csv"
  )
  
  # Download handler for date analysis
  output$downDateAnalysis <- createDownloadHandler(
    dataToDownload = function() {
      filtered_data <- movies_data %>%
        filter(premiere >= input$dateRange[1] & premiere <= input$dateRange[2]) %>%
        mutate(premiere = format(premiere, "%m/%d/%Y"))
      filtered_data
    },
    filename = "Date_Analysis",
    extension = "csv"
  )
  
  # Download handler for yearly graph
  output$downYearlyGraph <- createDownloadHandler(
    dataToDownload = function() {
      yearly_data <- movies_data %>%
        group_by(year) %>%
        summarise(count = n()) %>%
        arrange(year)
      p <- ggplot(yearly_data, aes(x = as.numeric(year), y = count)) +
        geom_line() +
        labs(title = "Movies Released Per Year",
             x = "Year",
             y = "Number of Movies") +
        theme_minimal()
      p
    },
    filename = "Yearly_Graph",
    extension = "png"
  )
}

# Run the Shiny app
shinyApp(ui, server)
