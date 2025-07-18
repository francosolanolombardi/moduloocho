#install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "leaflet", 
#                   "DT", "lubridate", "sf", "rnaturalearth", "rnaturalearthdata"))

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Cargar datos
df <- read.csv("base_final_trabajo.csv")
df$date <- as.Date(df$date)

# Cargar mapa mundial
world <- ne_countries(scale = "medium", returnclass = "sf")

# Variables numéricas para elegir
vars_numericas <- names(df)[sapply(df, is.numeric)]
vars_numericas <- setdiff(vars_numericas, c("Unnamed..0", "population"))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard COVID-19"),
  dashboardSidebar(
    sidebarMenu(
      id = "tab",  # Necesario para usar en conditionalPanel
      menuItem("Línea", tabName = "linea", icon = icon("chart-line")),
      menuItem("Barras", tabName = "barras", icon = icon("chart-bar")),
      menuItem("Mapa", tabName = "mapa", icon = icon("globe")),
      menuItem("Dispersión", tabName = "scatter", icon = icon("braille"))
    ),
    dateRangeInput("fecha", "Rango de fechas:",
                   start = min(df$date), end = max(df$date)),
    selectInput("continente", "Continente:", choices = c("Todos", unique(df$continent))),
    selectInput("pais", "País:", choices = c("Todos", unique(df$location))),
    
    # Variable principal (para línea, barras y mapa)
    selectInput("variable", "Variable principal:", choices = vars_numericas, selected = "total_cases"),
    
    # Solo para scatterplot: variable X e Y
    conditionalPanel(
      condition = "input.tab === 'scatter'",
      selectInput("scatter_x", "Variable eje X (scatter):", choices = vars_numericas, selected = "total_cases"),
      selectInput("scatter_y", "Variable eje Y (scatter):", choices = vars_numericas, selected = "total_deaths")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("linea",
              fluidRow(
                box(title = "Gráfico de líneas", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("plot_line"))
              )),
      tabItem("barras",
              fluidRow(
                box(title = "Gráfico de barras por país", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("plot_bar"))
              )),
      tabItem("mapa",
              fluidRow(
                box(title = "Mapa coroplético", width = 12, status = "success", solidHeader = TRUE,
                    leafletOutput("plot_map"))
              )),
      tabItem("scatter",
              fluidRow(
                box(title = "Gráfico de dispersión", width = 12, status = "warning", solidHeader = TRUE,
                    plotOutput("plot_scatter"))
              ))
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  data_filtrada <- reactive({
    df %>%
      filter(date >= input$fecha[1] & date <= input$fecha[2]) %>%
      filter(if (input$continente != "Todos") continent == input$continente else TRUE) %>%
      filter(if (input$pais != "Todos") location == input$pais else TRUE)
  })
  
  output$plot_line <- renderPlot({
    ggplot(data_filtrada(), aes(x = date, y = .data[[input$variable]], color = location)) +
      geom_line() +
      labs(title = paste("Evolución de", input$variable), y = input$variable, x = "Fecha") +
      theme_minimal()
  })
  
  output$plot_bar <- renderPlot({
    df_bar <- data_filtrada() %>%
      group_by(location) %>%
      summarise(valor = max(.data[[input$variable]], na.rm = TRUE)) %>%
      arrange(desc(valor)) %>%
      slice_head(n = 10)
    
    ggplot(df_bar, aes(x = reorder(location, valor), y = valor)) +
      geom_bar(stat = "identity", fill = "#2c7fb8") +
      coord_flip() +
      labs(title = paste("Top 10 países por", input$variable), y = input$variable, x = "País") +
      theme_minimal()
  })
  
  output$plot_map <- renderLeaflet({
    df_latest <- data_filtrada() %>%
      group_by(location) %>%
      filter(date == max(date)) %>%
      summarise(valor = max(.data[[input$variable]], na.rm = TRUE), .groups = "drop")
    
    mapa <- left_join(world, df_latest, by = c("name" = "location"))
    
    pal <- colorNumeric("YlOrRd", domain = mapa$valor, na.color = "transparent")
    
    leaflet(mapa) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(valor),
                  weight = 1, color = "white",
                  fillOpacity = 0.8,
                  label = ~paste(name, "<br>", input$variable, ":", round(valor, 2))) %>%
      addLegend(pal = pal, values = ~valor, title = input$variable, position = "bottomright")
  })
  
  output$plot_scatter <- renderPlot({
    df_scatter <- data_filtrada() %>%
      group_by(location) %>%
      filter(date == max(date)) %>%
      summarise(x = max(.data[[input$scatter_x]], na.rm = TRUE),
                y = max(.data[[input$scatter_y]], na.rm = TRUE),
                .groups = "drop")
    
    ggplot(df_scatter, aes(x = x, y = y, label = location)) +
      geom_point(color = "steelblue", size = 3) +
      geom_text(hjust = 1.1, vjust = 1.1, size = 3) +
      labs(x = input$scatter_x, y = input$scatter_y, title = "Relación entre variables") +
      theme_minimal()
  })
}

shinyApp(ui, server)
