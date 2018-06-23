library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(leaflet)
# library(rgdal)
library(reshape2)
library(scales)
library(dplyr)

####################################################################################################################
# Importar Data

## Tablas
infopozos <- read.csv("./Data/infopozos.csv")
names(infopozos) <- c("Pozo","lng","lat","x84","y84","x56","y56","Elev","SMA","SG","ModC")
pozos_qca <- c("CB-2", "CB-3", "CB-4", "CB-5", "CB-6", "CB-7", "CB-8", "CB-9", "CB-10", "CB-12","CON-10",
           "CON-15", "CON-16", "CON-21", "KP-DH10-40", "KP-DH10-33", "QSCSG6-237", "QSG08-402", "QSG08-431")

niveles <- read.csv("./Data/niveles.csv", fileEncoding="latin1", colClasses = c("factor", "factor", "NULL", "NULL", "numeric"))
niveles$Fecha <- as.Date(niveles$Fecha, format = "%m/%d/%Y")

qca <- sample_n(read.csv("./Data/qca.csv", fileEncoding="latin1"), 50)

qca$Fecha <- as.Date(as.character(qca$Fecha, format = "%m/%d/%Y"))
qca_t <- melt(qca, id.vars = c("Pozo","Fecha","SampleID","UH"), variable.name = "parametro")

niveles_qca <- subset(niveles, Pozo %in% pozos_qca)
niveles_qca$Nivel <- (-1*niveles_qca$Nivel) 

max_date <- max(qca_t$Fecha)
min_date <- min(qca_t$Fecha)

## Shapes
# tailings <- readOGR("./Data/tranque.dbf",layer="tranque")
# rajo <- readOGR("./Data/rajo.dbf", layer="rajo")

# new dataset con niveles y qca
mergedDB <- merge(niveles_qca, qca, all = TRUE)
mergedDB_t <- melt(mergedDB, id.vars = c("Pozo","Fecha","SampleID","UH", "Observacion"), variable.name = "parametro")
#####################################################################################################################

# Aplicacion
ui <- dashboardPage(
  dashboardHeader(title = ("Monitoreo de pozos")),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("home"), selected = TRUE),
    menuItem("Datos", tabName = "data", icon = icon("database"),
                menuSubItem("Tablas", tabName = "tabla", icon= icon("table")))
    # 
    )#Cierra sidebarmenu
  ), #Cierra dashboard sidebard
  
  
  dashboardBody(
    tabItems(
    tabItem(tabName="dashboard",
    fluidRow(
      box(title = ("Mapa interactivo"), status = "info", width = 12,
        leafletOutput("map")
        ), #Cierra box de mapa
      
      tabBox( width = 6,
        tabPanel("Niveles", 
        selectInput(width = 200,
          inputId = "pozo", label = NULL,
          choices = as.character(infopozos$Pozo),
          selected = "CB-1"),
        plotOutput("plot_niveles")
        ), #cierra tab panel
        tabPanel("Hidroquimica",
                 column(4, selectInput(width = 200,
                             inputId = "pozo2", label = NULL,
                             choices = sort(as.character(qca_t$Pozo)), ""
                             )),
                 column(4, selectInput(width = 200,
                             inputId = "par", label = NULL,
                             choices = as.character(qca_t$parametro), ""
                             )),
                 br(),
                 plotlyOutput("plot_qca")
        ) #cierra tab panel
      ) # Cierra tabbox
    ), #Cierra fROw 1
fluidRow(
      box(width = 6, 
        title = ("Grafico interactivo"), status = "success", collapsible = TRUE,
          plotOutput("plot_comp")
      ),
      box(width = 2, 
        title = "Seleccione pozos a graficar", status = "success", 
          checkboxGroupInput(inputId = "pozos", "", pozos_qca
                        )
      )
                        ),#Cierra fROw 2
      fluidRow(
      infoBox(width = 3, "Pozo monitoreados", 55 , icon = icon("play")),
      infoBox(width = 3, "Pozo reportados SMA", 22 , icon = icon("battery-full"))
      )#Cierra fROw 2
         ), #Cierra tabitem dashboard
    tabItem(tabName = "tabla",
      fluidRow(
        box(title = "Base de datos", status = "primary", width = 3,
            selectInput(inputId = "down_data",
                        "Seleccione datos a descargar", 
                        c("Informacion de pozos", "Niveles", "Hidroquimica")
            ),
            downloadButton('downloadData', 'Download')
        ),
        box(title = "Base de datos", status = "primary", width = 12,
       dataTableOutput("database")  
            )# cierra box de base de datos
      ) # Cierra fluidrow de base de datos
      ) # cierra tabitem de datos
    
    
        ) #Cierra tabItems  
  ) #cierra Dashboard body
  
) #cierra el ui

server <- function(input, output) {
  
  output$map <- renderLeaflet({ # Agrega mapa de leaflet
    leaflet(data = infopozos) %>%
      addProviderTiles("Esri.WorldImagery") %>%  # Add Provider Map Tiles
      # addTiles()%>%
      setView(-69.35, -22.85, zoom = 12) %>%
      # setMaxBounds(-70, -23, -68, -21)%>%
      # addPolylines(data=tailings, popup="Tranque de Relaves", color = "yellow")%>%
      # addPolylines(data=rajo, popup="Rajo Catabela", color = "yellow")%>%
      addCircleMarkers(~lng, ~lat, popup=~Pozo, radius=5, group = "1")

  })
  
  newdata_nivel <- reactive({
    validate(
      need(input$pozo != "CB-1" & input$pozo != "CB-11", "Pozo Seco, seleccione otro pozo")
    )
    
    subset(niveles, Pozo == input$pozo)
  })

  newdata_qca <- reactive({
    validate(
      need(input$pozo2 != "CB-1" & input$pozo2 != "CB-11", "Pozo Seco, seleccione otro pozo")
    )
    
    subset(qca_t, Pozo == input$pozo2 & parametro == input$par)
  })
  
  comparedata <- reactive({
    validate(
      need(input$pozos != "", "Seleccione un pozo para visualizar graficos")
    )
    # 
    (subset(mergedDB_t, Pozo %in% input$pozos &(parametro == "Nivel" | parametro == "TDS" | parametro == "Cond")))
  })
  exportdata <- reactive({
    # validate(
    #   need(input$pozos != "", "Seleccione un pozo para visualizar graficos")
    # )
    # 
      if(input$down_data == "Informacion de pozos") infopozos else 
        (if(input$down_data == "Niveles") niveles_qca
              else qca) 
      })
  
  output$plot_niveles <- renderPlot({
    p1 <- ggplot(newdata_nivel(), aes(Fecha, Nivel, group = Pozo, label = Pozo, color = "blue")) + geom_point() + geom_line() +
      scale_y_reverse() + scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")+
      theme(axis.text.x  = element_text(angle=90))
    print(p1)
  })
  
  output$plot_qca <- renderPlotly({
    q1 <- ggplot(newdata_qca(), aes(x = Fecha, y = value, label= SampleID, group = parametro))+ geom_line(data=newdata_qca()[!is.na(newdata_qca()$value),]) + geom_point()+
      scale_x_date(date_breaks = "3 month", date_labels = "%m/%y", limits = c(min_date, max_date)) +
      theme(axis.text.x  = element_text(angle=90))
    ggplotly(q1)
    
  })
  
  output$plot_comp <- renderPlot({
    p2 <- ggplot(comparedata(), aes(x = Fecha, y = value, group = Pozo, color = Pozo)) + geom_point() + 
      geom_line(data=comparedata()[!is.na(comparedata()$value),]) +
      facet_grid(parametro~., scales = "free_y")+
      scale_x_date(date_breaks = "3 month", date_labels = "%m/%y", limits = c(min_date, max_date))+
      # scale_y_continuous(breaks = c(seq(min(comparedata()$value),2,max(comparedata()$value))))+
      theme(axis.text.x  = element_text(angle=90))
  #   # ggp <- as.widget(ggplotly(p2, height = 700)%>% layout(autosize=TRUE))
  p2
  # ggp
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Datos_',input$down_data ,'_',Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(exportdata(), con)
    }
  )
  output$database <- renderDataTable({
    exportdata()
  })
  
} #cierra el server

shinyApp(ui, server)