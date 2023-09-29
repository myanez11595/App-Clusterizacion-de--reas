library(shiny)
library(shinythemes)
library(DT)
library(sf)
library(sp)
library(rgdal)
library(dplyr)
library(leaflet.extras)
library(leaflet)
library(tidyverse)
library(adagio)

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                titlePanel(h1("INSTITUTO NACIONAL DE ESTADÍSTICA Y CENSOS", align="center")),
                navbarPage("Áreas de empadronamiento",
                           
                           tabPanel(icon("home"), 
                                    fluidRow(
                                      
                                      column(
                                        br(),
                                        
                                        p("En la etapa de Actualización Cartográfica y Pre censo de Viviendas, se recogió la información
                                        de las viviendas con los jefes de hogar, número de habitantes y los establecimientos existentes.
                                        En este censo y en anteriores, la información básica es la que sirve para la elaboración de áreas
                                        de empadronamiento que hasta el año 2001 se la realizaba manualmente. Con las experiencias
                                        censales y con el objeto de automatizar la generación de las áreas de empadronamiento se
                                        desarrolló un algoritmo el mismo que, partiendo de la base precensal: genera las áreas, corrige
                                        errores, mejora tiempos y sobre todo estandariza procesos.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                        width=10),
                                      column(
                                        br(),
                                        tags$a(href='https://www.ecuadorencifras.gob.ec/institucional/home/',tags$img(src="inec.png",width="200px",height="130px")),
                                        width=2)
                                      
                                    ),
                                    sidebarPanel(width=4,tabsetPanel(type = "tabs",
                                                                  tabPanel("Primer proceso", 
                                                                           br(),
                                      fileInput("shp1", "Manzana sector (.zip)",
                                                multiple = F,
                                                ".zip",width = '100%'),
                                      br(),
                                      dataTableOutput("files"),
                                      br(),
                                      fileInput("shp2", "Sede (.zip)",
                                                multiple = F,
                                                ".zip",width = '100%'),
                                      br(),
                                      dataTableOutput("files2"),
                                      br(),
                                      fileInput("shp4", "Edificios Censales (CA04) (.zip)",
                                                multiple = F,
                                                ".zip",width = '100%'),
                                      br(),
                                      dataTableOutput("files4"),
                                      br(),
                                      fileInput("shp5", "Ríos (.zip)",
                                                multiple = F,
                                                ".zip",width = '100%'),
                                      br(),
                                      dataTableOutput("files5")),
                                      tabPanel("Segundo proceso",
                                      br(),
                                      fileInput("shp6", "Adicionales_l  (.zip)",
                                                multiple = F,
                                                ".zip",width = '100%'),
                                      br(),
                                      dataTableOutput("files6"),
                                      br(),
                                      fileInput("shp3", "Ejes viales (.zip)",
                                                multiple = F,
                                                ".zip",width = '100%'),
                                      br(),
                                      dataTableOutput("files3"),
                                      br(),
                                      fileInput("shp7", "AEs depurada  (.zip)",
                                                multiple = F,
                                                ".zip",width = '100%'),
                                      br(),
                                      dataTableOutput("files7"),
                                      br(),
                                      fileInput("shp8", "Sede (.zip)",
                                                multiple = F,
                                                ".zip",width = '100%'),
                                      br(),
                                      dataTableOutput("files8")
                                      
                                    ),
                                    tabPanel("Tercer proceso",
                                             br(),
                                             fileInput("shp9", "AEs Finales  (.zip)",
                                                       multiple = F,
                                                       ".zip",width = '100%'),
                                             br(),
                                             dataTableOutput("files9"),
                                             br(),
                                             fileInput("csv10", "Base de campo (.csv)",
                                                       multiple = F,
                                                       ".zip",width = '100%'),
                                             br(),
                                             dataTableOutput("files10")),
                                    tabPanel("Cuarto proceso",
                                             br(),
                                             fileInput("shp11", "Manzanas  (.zip)",
                                                       multiple = F,
                                                       ".zip",width = '100%'),
                                             br(),
                                             dataTableOutput("files11"),
                                             br(),
                                             fileInput("shp12", "CA04  (.zip)",
                                                       multiple = F,
                                                       ".zip",width = '100%'),
                                             br(),
                                             dataTableOutput("files12")))),

                                    mainPanel(width=8,tabsetPanel(type = "tabs",
                                                                  tabPanel("Resultado",
                                                                          column( br(), sidebarPanel("Porcentaje",selectInput("psf","Porcentaje Superior",choices = c(1, 1.015,1.035, 1.05,1.07,1.085,1.1,1.15,1.2,1.25), selected = "1.035"),
                                                                                        selectInput("pif","Porcentaje Inferior",choices = c(0.8,0.85,0.9,0.95,1,1.05,1.1,1.15,1.2), selected = "0.85"),width = 5),
                                                                           sidebarPanel( actionButton("go", "Resultados"),htmlOutput("text"), width = 7),
                                                                            width = 12),
                                                                           br(),
                                                                           h3("Áreas de Empadronamiento Resultantes",align="center"),
                                                                           h1(downloadButton("downloadData", "Descarga Primer Resultado"),"",downloadButton("downloadData2", "Descarga Segundo Resultado"),"",downloadButton("downloadData3", "Descarga Tercer Resultado"),"",downloadButton("downloadData4", "Descarga Cuarto Resultado"),align="center"), 
                                                                                            column(h3("Mapa de Ubicación de Sedes",align="center"),leafletOutput("mapa", height = 700), width = 12),align="center")
                                                                           
                                    )),    
                                    br(),
                                    hr(),
                                    fluidRow(column(width = 12, p(em("Developed by"),br("Instituto Nacional de Estadística y Censos"),style="text-align:center; font-family: times") ))
                           )
                           
                           
                )
)

options(shiny.maxRequestSize=10000*1024^2)

server<-function(input, output){
#Cargamos funciones creadas
  source("funciones/Rutinas/00_insumos.R")
  source("funciones/Rutinas/000_preparacion.R")
  source("funciones/extpol/caja.R")
  source("funciones/Preparacion/st_centroid_within_poly.R")
  source("funciones/Rutinas/0_modelo.R")


#Cargamos files que necesitamos 
  SECTOR_MANZANA_A= reactive(
    {
      temp <- tempfile()
      unzip(zipfile = input$shp1$datapath, exdir = temp)
      
      #dsn <- input$shp1$datapath
      
      shp1 <- read_sf(temp)
      
      SECTOR_MANZANA_A <- shp1

      return(SECTOR_MANZANA_A)  
    }
  )
  
  
  sede_a= 
    reactive(
      {
        temp2 <- tempfile()
        unzip(zipfile = input$shp2$datapath, exdir = temp2)
        
        #dsn <- input$shp1$datapath
        
        shp2 <- read_sf(temp2)
        
        sede_a<- shp2
        
        return(sede_a)  
      }
    )
  
  sede_a2= 
    reactive(
      {
        temp8 <- tempfile()
        unzip(zipfile = input$shp8$datapath, exdir = temp8)
        
        #dsn <- input$shp1$datapath
        
        shp8 <- read_sf(temp8)
        
        sede_a2<- shp8
        
        return(sede_a2)  
      }
    )
  
  
  Ejes= reactive({  
    temp3 <- tempfile()
    unzip(zipfile = input$shp3$datapath, exdir = temp3)
    
    #dsn <- input$shp1$datapath
    
    shp3 <- read_sf(temp3)
    
    Ejes <- shp3 %>% 
      st_as_sf() %>%
      st_cast("LINESTRING") %>% 
      group_by() %>% 
      summarise() %>% 
      st_intersection(sede_a2())

    return(Ejes)
  })
  ca04= reactive(
    {
      temp4 <- tempfile()
      unzip(zipfile = input$shp4$datapath, exdir = temp4)
      
      #dsn <- input$shp1$datapath
      
      shp4 <- read_sf(temp4)
      
      ca04<- shp4

      return(ca04)  
    }
  )
  rios= reactive(
    {
      temp5 <- tempfile()
      unzip(zipfile = input$shp5$datapath, exdir = temp5)
      
      #dsn <- input$shp1$datapath
      
      shp5 <- read_sf(temp5)
      
      rios<- shp5

      return(rios)  
    }
  )
  
  adicionales= reactive({  
    temp6 <- tempfile()
    unzip(zipfile = input$shp6$datapath, exdir = temp6)
    
    #dsn <- input$shp1$datapath
    
    shp6 <- read_sf(temp6)
    
    adicionales <- shp6 %>%
      st_as_sf() %>% 
      st_cast("LINESTRING") %>% 
      group_by() %>% 
      summarise() %>% 
      st_intersection(sede_a2())
    
    return(adicionales)
  })
  
  AES= reactive(
    {
      temp7 <- tempfile()
      unzip(zipfile = input$shp7$datapath, exdir = temp7)
      
      #dsn <- input$shp1$datapath
      
      shp7 <- read_sf(temp7)
      
      AES<- shp7
      
      return(AES)  
    }
  )
  AES_finales= reactive(
    {
      temp9 <- tempfile()
      unzip(zipfile = input$shp9$datapath, exdir = temp9)
      
      #dsn <- input$shp1$datapath
      
      shp9 <- read_sf(temp9)
      
      AES_finales<- shp9
      
      return(AES_finales)  
    }
  )
  manzana_p= reactive(
    {
      temp11 <- tempfile()
      unzip(zipfile = input$shp11$datapath, exdir = temp11)
      
      #dsn <- input$shp1$datapath
      
      shp11 <- st_read(temp11)
      
      manzana_p<- shp11
      
      return(manzana_p)  
    }
  )
  ca04_p= reactive(
    {
      temp12 <- tempfile()
      unzip(zipfile = input$shp12$datapath, exdir = temp12)
      
      #dsn <- input$shp1$datapath
      
      shp12 <- st_read(temp12)
      
      ca04_p<- shp12
      
      return(ca04_p)  
    }
  )
  rutina1= reactive({
    psf_1 = as.numeric(input$psf) 
    pif_1 = as.numeric(input$pif)  
    
    rutina1=insumos(SECTOR_MANZANA_A(),ca04(),rios(),sede_a(),psf_1,pif_1)
    
    return(rutina1)
  })
  
  new_man= reactive({
    
    new_man=preparacion(AES(),Ejes(),adicionales())
    
    return(new_man)
  })
  
  activador=eventReactive(input$go,{
    withProgress(message = "Procesando....", {
      activador=HTML(rutina1()$text) 
      return(activador)
      
    })
  })
  output$text =renderUI({ 
    
    activador()  
    #}
  })
  
  prio= reactive({
    
    prio=priorizados(manzana_p(),ca04_p())
    
    return(prio)
  })
  
  output$mapa = renderLeaflet({  
    if(is.null(input$shp8)&is.null(input$shp2)){
      leaflet() %>% addProviderTiles(providers$Esri.WorldImagery,group = "Satelite")%>%  setView(lng = -78, lat = 0,  zoom = 4)
    }else if(is.null(input$shp8)){ 
      mapa= leaflet() %>% addProviderTiles(providers$Esri.WorldImagery,group = "Satelite") %>% 

        addLayersControl( overlayGroups = c("Satelite", paste("Sede","",unique(sede_a()$N_SO))), options=layersControlOptions(collapsed = F,labels="Layers",title = "Layers",color="blue"))%>%
        
        addScaleBar()%>% 
        addMeasurePathToolbar(options = measurePathOptions(imperial = T,
                                                           minPixelDistance = 100,
                                                           showDistances = F))%>%
        addDrawToolbar(editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
      addPolygons(mapa,data=st_as_sf(spTransform(as_Spatial(sede_a()), CRS("+proj=longlat +ellps=WGS84"))),weight=5,col = 'red',group = paste("Sede","",unique(sede_a()$N_SO))) 
    }else{
      mapa= leaflet() %>% addProviderTiles(providers$Esri.WorldImagery,group = "Satelite") %>% 
        
        addLayersControl( overlayGroups = c("Satelite", paste("Sede","",unique(sede_a2()$N_SO))), options=layersControlOptions(collapsed = F,labels="Layers",title = "Layers",color="blue"))%>%
        
        addScaleBar()%>% 
        addMeasurePathToolbar(options = measurePathOptions(imperial = T,
                                                           minPixelDistance = 100,
                                                           showDistances = F))%>%
        addDrawToolbar(editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
      addPolygons(mapa,data=st_as_sf(spTransform(as_Spatial(sede_a2()), CRS("+proj=longlat +ellps=WGS84"))),weight=5,col = 'red',group = paste("Sede","",unique(sede_a2()$N_SO))) 
    }
  })
  
  
  output$files <- renderDataTable(datatable(head(input$shp1), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files2 <- renderDataTable(datatable(head(input$shp2), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files3 <- renderDataTable(datatable(head(input$shp3), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files4 <- renderDataTable(datatable(head(input$shp4), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files5 <- renderDataTable(datatable(head(input$shp5), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files6 <- renderDataTable(datatable(head(input$shp6), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files7 <- renderDataTable(datatable(head(input$shp7), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files8 <- renderDataTable(datatable(head(input$shp8), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files9 <- renderDataTable(datatable(head(input$shp9), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files10 <- renderDataTable(datatable(head(input$csv10), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files11 <- renderDataTable(datatable(head(input$shp11), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$files12 <- renderDataTable(datatable(head(input$shp12), options = list(dom = 't',initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),scrollX = TRUE)))
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Shape Comprimido.zip")
    },
    content = function(file) {
     
      withProgress(message = "Procesando....", {
        
        incProgress(0.5)
        tmp.path <- dirname(file)
        
        name.base <- file.path(tmp.path, paste("AEs Sede","",unique(sede_a()$N_SO)) )
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        
        
        #########################segundo shape##############################
        tmp.path2 <- dirname(file)
        
        name.base2 <- file.path(tmp.path2, paste("Poligono Sede","",unique(sede_a()$N_SO)) )
        name.glob2 <- paste0(name.base2, ".*")
        name.shp2  <- paste0(name.base2, ".shp")
        

        
        if (length(Sys.glob(name.glob)) > 0 & length(Sys.glob(name.glob2)) > 0)  file.remove(Sys.glob(name.glob),Sys.glob(name.glob2))
        
        sf::st_write(rutina1()$prueba_01, dsn = name.shp2, ## layer = "shpExport",
                     driver = "ESRI Shapefile", quiet = TRUE, append=TRUE)
        
        zip::zipr(zipfile = name.zip, files = c(Sys.glob(name.glob2)))
        
        req(file.copy(name.zip, file))
        
        if (length(Sys.glob(name.glob)) > 0 & length(Sys.glob(name.glob2)) > 0) file.remove(Sys.glob(name.glob),Sys.glob(name.glob))
        
        incProgress(0.5)
      })
      
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("Shape Comprimido.zip")
    },
    content = function(file2) {
      
      withProgress(message = "Procesando....", {
        
        incProgress(0.5)
        tmp.path <- dirname(file2)
        
        name.base <- file.path(tmp.path, paste("AEs Sede","",unique(sede_a2()$N_SO)) )
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        
        
        #########################segundo shape##############################
        tmp.path2 <- dirname(file2)
        
        name.base2 <- file.path(tmp.path2, paste("Poligono Sede","",unique(sede_a2()$N_SO)) )
        name.glob2 <- paste0(name.base2, ".*")
        name.shp2  <- paste0(name.base2, ".shp")
        
        
        
        if (length(Sys.glob(name.glob)) > 0 & length(Sys.glob(name.glob2)) > 0)  file.remove(Sys.glob(name.glob),Sys.glob(name.glob2))
        
        sf::st_write(new_man(), dsn = name.shp, ## layer = "shpExport",
                     driver = "ESRI Shapefile", quiet = TRUE, append=TRUE)
        
        zip::zipr(zipfile = name.zip, files = c(Sys.glob(name.glob)))
        req(file.copy(name.zip, file2))
        
        if (length(Sys.glob(name.glob)) > 0 & length(Sys.glob(name.glob2)) > 0) file.remove(Sys.glob(name.glob),Sys.glob(name.glob))
        
        incProgress(0.5)
      })
      
    }
  )
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste("Shape Comprimido.zip")
    },
    content = function(file3) {
      
      withProgress(message = "Procesando....", {
        
        incProgress(0.5)
        tmp.path <- dirname(file3)
        
        name.base <- file.path(tmp.path, paste("AEs Sede Priorizadas") )
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        
        
        #########################segundo shape##############################
        tmp.path2 <- dirname(file3)
        
        name.base2 <- file.path(tmp.path2, paste("AEs Sede Priorizadas") )
        name.glob2 <- paste0(name.base2, ".*")
        name.shp2  <- paste0(name.base2, ".shp")
        
        
        
        if (length(Sys.glob(name.glob)) > 0 & length(Sys.glob(name.glob2)) > 0)  file.remove(Sys.glob(name.glob),Sys.glob(name.glob2))
        
        sf::st_write(prio(), dsn = name.shp, ## layer = "shpExport",
                     driver = "ESRI Shapefile", quiet = TRUE, append=TRUE)
        
        zip::zipr(zipfile = name.zip, files = c(Sys.glob(name.glob)))
        req(file.copy(name.zip, file3))
        
        if (length(Sys.glob(name.glob)) > 0 & length(Sys.glob(name.glob2)) > 0) file.remove(Sys.glob(name.glob),Sys.glob(name.glob))
        
        incProgress(0.5)
      })
      
    }
  )
}
shinyApp(ui = ui, server = server)

