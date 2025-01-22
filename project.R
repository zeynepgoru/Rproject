library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(DT)
library(ggrepel)

library(leaflet)  # For map visualization


ui <- fluidPage(
  titlePanel("Music Genre Revenue by Country"),
  
  # Create tabs 
  navbarPage(
    "Music Data Analysis",
    
    # First Page: Genre Revenue Analysis
    tabPanel(
      "Genre Revenue",
      sidebarLayout(
        sidebarPanel(
          # Widgets
          selectInput("genre_filter", "Choose Genre:", 
                      choices = NULL), 
          sliderInput("revenue_filter", "Filter by Minimum Revenue:",
                      min = 0, max = 300, value = 50, step = 10),

          h4("Top Selling Tracks by Revenue"),
          DTOutput("topTracksTable")  
        ),
        
        mainPanel(
         
          plotOutput("genrePlot"),
          
          
          leafletOutput("genreMap", height = 500)
        )
      )
    ),
    
    # Second Page: Pie Chart of Most Listened Genres by Country
    tabPanel(
      "Most Listened Genres",
      sidebarLayout(
        sidebarPanel(
          # Checkbox 
          checkboxGroupInput("country_filter", "Select Countries:", 
                             choices = NULL,  
                             selected = NULL)  
        ),
        
        mainPanel(
          
          uiOutput("genrePieCharts")  
        )
      )
    ),
    
    # Third Page: K-Means Clustering
    tabPanel(
      "Cluster Analysis",
      sidebarLayout(
        sidebarPanel(
          numericInput("num_clusters", "Number of Clusters:", 
                       value = 3, min = 2, max = 10, step = 1)
        ),
        
        mainPanel(
          plotOutput("clusterPlot")  #
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  
  path_to_db <- "C:/DB/chinook.db"
  
  
  con <- dbConnect(RSQLite::SQLite(), path_to_db)
  
  
  regional_genre <- dbGetQuery(con, "
    SELECT c.Country, g.Name AS Genre, SUM(il.UnitPrice * il.Quantity) AS Revenue
    FROM invoice_items il
    JOIN Invoices i ON il.InvoiceId = i.InvoiceId
    JOIN customers c ON i.CustomerId = c.CustomerId
    JOIN tracks t ON il.TrackId = t.TrackId
    JOIN genres g ON t.GenreId = g.GenreId
    GROUP BY c.Country, g.Name
    ORDER BY c.Country, Revenue DESC
  ")
  
  # Filter 
  top_countries <- regional_genre %>%
    group_by(Country) %>%
    summarize(TotalRevenue = sum(Revenue)) %>%
    arrange(desc(TotalRevenue)) %>%
    slice_head(n = 10) %>%
    pull(Country)
  
  filtered_data <- regional_genre %>%
    filter(Country %in% top_countries)
  
  # Update genre filter choices 
  observe({
    updateSelectInput(session, "genre_filter", 
                      choices = unique(filtered_data$Genre),
                      selected = unique(filtered_data$Genre)[1])
  })
  
  # Update country filter choices 
  observe({
    updateCheckboxGroupInput(session, "country_filter", 
                             choices = unique(filtered_data$Country),
                             selected = unique(filtered_data$Country)[1])  
  })
  
  
  filtered_data_reactive <- reactive({
    data <- filtered_data
    
    
    if (!is.null(input$genre_filter) && input$genre_filter != "") {
      data <- data %>%
        filter(Genre == input$genre_filter)
    }
    
    
    data <- data %>%
      filter(Revenue >= input$revenue_filter)
    
    return(data)
  })
  
 
  track_revenue_reactive <- reactive({
    req(input$genre_filter)  
    
    query <- paste0("
      SELECT t.Name AS Track, SUM(ii.UnitPrice * ii.Quantity) AS Revenue
      FROM invoice_items ii
      JOIN tracks t ON ii.TrackId = t.TrackId
      JOIN genres g ON t.GenreId = g.GenreId
      WHERE g.Name = '", input$genre_filter, "' 
      GROUP BY t.Name
      ORDER BY Revenue DESC
      LIMIT 10
    ")
    
    
    dbGetQuery(con, query)
  })
  
  # genre preference plot 
  output$genrePlot <- renderPlot({
    data_to_plot <- filtered_data_reactive()
    
    ggplot(data_to_plot, aes(x = Country, y = Revenue, fill = Genre)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Genre Preferences by Country",
           x = "Country",
           y = "Revenue",
           fill = "Genre") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #  top-selling tracks table
  output$topTracksTable <- renderDT({
    track_revenue_reactive()
  }, 
  options = list(
    pageLength = 3,  
    lengthMenu = list(c(3, 7, 10), c("3", "7", "10"))  
  ))
  
  # Generate the map 
  output$genreMap <- renderLeaflet({
    data_to_map <- filtered_data_reactive()
    
    
    country_coords <- data.frame(
      Country = c("USA", "Germany", "United Kingdom", "Canada", "France", "Italy", "Australia", 
                  "Nederland", "Brazil", "Spain", "Chile", "India", "Portugal", 
                  "Czech Republic"),
      Latitude = c(37.0902, 51.1657, 51.5074, 56.1304, 46.6034, 41.8719, -25.2744, 
                   52.1326, -14.2350, 40.4637, -35.6751, 20.5937, 39.3999, 49.8175),
      Longitude = c(-95.7129, 10.4515, -0.1278, -106.3468, 1.8883, 12.5674, 133.7751, 
                    5.2913, -51.9253, -3.7492, -71.5429, 78.9629, -8.2245, 15.4730)
    )
    
    
    data_map <- data_to_map %>%
      left_join(country_coords, by = "Country")
    
    #  color palette 
    color_pal <- colorNumeric(palette = "RdYlBu", domain = data_map$Revenue)
    
    leaflet(data_map) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,  
        color = ~color_pal(Revenue),
        radius = 8,
        popup = ~paste0("<strong>", Country, "</strong><br>",
                        "Genre: ", Genre, "<br>",
                        "Revenue: $", round(Revenue, 2))
      ) %>%
      addLegend(
        pal = color_pal,
        values = data_map$Revenue,
        title = "Revenue",
        opacity = 0.7
      )
  })
  
  #  pie chart 
  output$genrePieCharts <- renderUI({
    
    selected_countries <- input$country_filter
    
    
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      return(NULL)
    }
    
   
    lapply(selected_countries, function(country) {
      plotOutput(outputId = paste0("genrePieChart_", country))
    })
  })
  
  
  observe({
    selected_countries <- input$country_filter
    
    # For each selected country
    lapply(selected_countries, function(country) {
      output[[paste0("genrePieChart_", country)]] <- renderPlot({
        
        country_data <- regional_genre %>%
          filter(Country == country)
        
       
        ggplot(country_data, aes(x = "", y = Revenue, fill = Genre)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar(theta = "y") +
          labs(title = paste("Most Listened Genres in", country)) +
          theme_void() +
          theme(legend.title = element_blank())
      })
    })
  })
  
  # K-Means Clustering 
  output$clusterPlot <- renderPlot({
    req(input$num_clusters)
    
    # Data for clustering
    clustering_data <- regional_genre %>%
      group_by(Country) %>%
      summarize(TotalRevenue = sum(Revenue), .groups = "drop") %>%
      filter(!is.na(TotalRevenue))
    
    #  k-means clustering
    set.seed(42)
    kmeans_result <- kmeans(clustering_data$TotalRevenue, centers = input$num_clusters)
    clustering_data$Cluster <- as.factor(kmeans_result$cluster)
    
    # Create the clustering plot
    ggplot(clustering_data, aes(x = Country, y = TotalRevenue, color = Cluster, label = Country)) +
      geom_point(size = 4, alpha = 0.8) +
      geom_text_repel(size = 3) +
      labs(title = "K-Means Clustering of Revenue by Country",
           x = "Country", y = "Total Revenue", color = "Cluster") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}


shinyApp(ui = ui, server = server)



