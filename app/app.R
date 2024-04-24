library(shiny)
library(httr)
library(jsonlite)

ui <- fluidPage(
  titlePanel("Weather Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("location", "Location:"),
      textInput("start_date", "Start Date (yyyy-mm-dd):"),
      textInput("end_date", "End Date (yyyy-mm-dd, optional):"),
      actionButton("btn_run", "Run")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Temperature", plotOutput("temp_plot")),
        tabPanel("Humidity", plotOutput("humidity_plot")),
        tabPanel("Precipitation", plotOutput("precipitation_plot")),
        tabPanel("Wind Speed", plotOutput("wind_speed_plot"))
      )
    )
  )
)


server <- function(input, output) {
  base_url <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/"
  API_KEY <- "3KAJKHWT3UEMRQWF2ABKVVVZE"
  
  fetchWeatherData <- function(location, start_date, end_date) {
    tryCatch({
      if (!is.null(end_date)) {
        url <- paste0(base_url, location, "/", start_date, "/", end_date, "?key=", API_KEY)
      } else if (!is.null(start_date)) {
        url <- paste0(base_url, location, "/", start_date, "?key=", API_KEY)
      } else {
        url <- paste0(base_url, location, "?key=", API_KEY)
      }
      
      weather_data <- httr::content(httr::GET(url), as = "parsed")
      return(weather_data)
    }, error = function(e) {
      stop("Error fetching data from the API: ", e$message)
    })
  }
  
  observeEvent(input$btn_run, {
    location <- input$location
    start_date <- input$start_date
    end_date <- input$end_date
    
    if (is.null(location)) {
      return()
    }
    
    weather_data <- fetchWeatherData(location, start_date, end_date)
    
    if (!is.null(weather_data)) {
      days <- weather_data$days
      dates <- as.Date(sapply(days, `[[`, "datetime"), format = "%Y-%m-%d")
      temperatures <- sapply(days, `[[`, "temp")
      humidities <- sapply(days, `[[`, "humidity")
      precipitations <- sapply(days, `[[`, "precip")
      wind_speeds <- sapply(days, `[[`, "windspeed")
      
      max_temp <- max(temperatures, na.rm = TRUE)
      min_temp <- min(temperatures, na.rm = TRUE)
      avg_temp <- mean(temperatures, na.rm = TRUE)
      
      max_humidity <- max(humidities, na.rm = TRUE)
      min_humidity <- min(humidities, na.rm = TRUE)
      avg_humidity <- mean(humidities, na.rm = TRUE)
      
      max_precip <- max(precipitations, na.rm = TRUE)
      min_precip <- min(precipitations, na.rm = TRUE)
      avg_precip <- mean(precipitations, na.rm = TRUE)
      
      max_wind <- max(wind_speeds, na.rm = TRUE)
      min_wind <- min(wind_speeds, na.rm = TRUE)
      avg_wind <- mean(wind_speeds, na.rm = TRUE)
      
      output$temp_plot <- renderPlot({
        plot(dates, temperatures, type = "l", col = "red", xlab = "Date", ylab = "Temperature (°C)", main = "Daily Temperatures")
        text(x = dates, y = temperatures, labels = round(temperatures, 2), pos = 3, col = "red", cex = 0.7)
        text(x = max_temp, y = temperatures[1], labels = round(temperatures, 2), pos = 3, col = "red", cex = 0.7)
        abline(h = max_temp, col = "red", lty = 2, lwd = 2)
        abline(h = min_temp, col = "blue", lty = 2, lwd = 2)
        abline(h = avg_temp, col = "green", lty = 2, lwd = 2)
      })
      
      output$humidity_plot <- renderPlot({
        plot(dates, humidities, type = "l", col = "blue", xlab = "Date", ylab = "Humidity (%)", main = "Daily Humidity")
        text(x = dates, y = humidities, labels = round(humidities, 2), pos = 3, col = "blue", cex = 0.7)
        abline(h = max_humidity, col = "red", lty = 2, lwd = 2)
        abline(h = min_humidity, col = "blue", lty = 2, lwd = 2)
        abline(h = avg_humidity, col = "green", lty = 2, lwd = 2)
      })
      
      output$precipitation_plot <- renderPlot({
        plot(dates, precipitations, type = "l", col = "green", xlab = "Date", ylab = "Precipitation (mm)", main = "Daily Precipitation")
        text(x = dates, y = precipitations, labels = round(precipitations, 2), pos = 3, col = "green", cex = 0.7)
        abline(h = max_precip, col = "red", lty = 2, lwd = 2)
        abline(h = min_precip, col = "blue", lty = 2, lwd = 2)
        abline(h = avg_precip, col = "green", lty = 2, lwd = 2)
      })
      
      output$wind_speed_plot <- renderPlot({
        plot(dates, wind_speeds, type = "l", col = "magenta", xlab = "Date", ylab = "Wind Speed (km/h)", main = "Daily Wind Speed")
        text(x = dates, y = wind_speeds, labels = round(wind_speeds, 2), pos = 3, col = "magenta", cex = 0.7)
        abline(h = max_wind, col = "red", lty = 2, lwd = 2)
        abline(h = min_wind, col = "blue", lty = 2, lwd = 2)
        abline(h = avg_wind, col = "green", lty = 2, lwd = 2)
      })
    }
  })
}

shinyApp(ui, server)
