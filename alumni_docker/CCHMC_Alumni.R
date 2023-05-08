##====UI====##

library(shiny)
library(flexdashboard)
library(tidyverse)
library(leaflet)
library(DT)
library(sf)
library(shinyDataFilter)##Dynamic data filter
library(mapboxapi)
##Ref: https://stackoverflow.com/questions/50111566/applying-leaflet-map-bounds-to-filter-data-within-shiny
##     https://demo.appsilon.com/apps/ports_analytics/#!/
# setwd('C:/Users/LIUC3K/Desktop/Rshiny project for Alumni/alumni_pangyu/CCHMC_alumni/alumni_docker')

d_docs <- read_rds("alumni_geo.rds") 
z_centroid <- read_rds("zcta_centroid.rds")

# Define UI 
ui <- fluidPage(
  
  # App title ----
  # <img src="https://www.seekpng.com/png/detail/336-3364375_cincinnati-childrens-hospital-logo.png" alt="Cincinnati Children's Hospital Logo@seekpng.com">
  # navbarPage(
     title ="",
                
  sidebarLayout(
    
    sidebarPanel(width =3,

     selectInput("filter_options", 
                    label = h4('Select a provider by name, specialty:'),
                    choices = c("Provider Name","Specialty","Metropolitan Area","Search by Zipcode"), 
                    multiple = F),
     
     # filter BASED ON "Provider Name"
      conditionalPanel(
        condition = "input.filter_options == 'Provider Name'",
       
        selectizeInput("select_phys", label = "Provider Name:",
                      choices = unique(sort(d_docs$phys_name)), selected = NULL, multiple = TRUE)
      ), 
     # filter BASED ON specialty
     conditionalPanel(
       condition = "input.filter_options == 'Specialty'",
       
       selectizeInput("select_specialty", label = "Specialty:",
                      choices = unique(sort(d_docs$specialty1)), selected = NULL, multiple = TRUE),
       selectizeInput("select_metro_specialty", label = "Metropolitan Area:",
                      choices = unique(sort(d_docs$metro_name)), selected = NULL, multiple = TRUE)
     ), 
     # filter BASED ON Metropolitan Area
     conditionalPanel(
       condition = "input.filter_options == 'Metropolitan Area'",
       
       selectizeInput("select_metro", label = "Metropolitan Area:",
                      choices = unique(sort(d_docs$metro_name)), selected = NULL, multiple = TRUE),
       selectizeInput("select_specialty_metro", label = "Specialty:",
                      choices = unique(sort(d_docs$specialty1)), selected = NULL, multiple = TRUE)
     ), 
     # filter BASED ON Zipcode
     conditionalPanel(
       condition = "input.filter_options == 'Search by Zipcode'",
       textInput("select_zip", label = "Zip code:", value = '',
                 placeholder = "Enter zip..."),
       sliderInput("buffer_slide", label = "Radius (miles)", min = 5,
                   max = 100, value = 25, step  = 5),
       selectizeInput("select_specialty_zip", label = "Specialty:",
                      choices = unique(sort(d_docs$specialty1)), selected = NULL, multiple = TRUE)
     ), 
     tags$h6(textOutput("docs_in_table"))#,
     # downloadButton("dt", label = "Download")

    ),
    
    
    
    
    mainPanel(width =9,
              # splitLayout(
              #             leafletOutput("map", height = 850),
              #             DTOutput("alumni_tb")
              #             )
              verticalLayout(leafletOutput("map", height =650),DTOutput("alumni_tb"))
      
      
      
    )
  )
# )
)



server <- function(input, output, session) {
  
  ##Filter changed based on "Phsyical Name"
  # observe({
  # x <- d_docs %>% filter(phys_name %in% c(input$select_phys)) %>% st_drop_geometry()
  # updateSelectizeInput(session, "select_specialty_phys", "Specialty:", choices = sort(unique(x$specialty1)))
  # })
  observe({
    x <- d_docs %>% filter(phys_name %in% c(input$select_phys)) %>% st_drop_geometry()
    updateSelectizeInput(session, "select_metro_phys", "Metropolitan Area:", choices = sort(unique(x$metro_name)))
  })
  ##Filter changed based on "Specialty"
  observe({
    x <- d_docs %>% filter(specialty1 %in% c(input$select_specialty)) %>% st_drop_geometry()
    updateSelectizeInput(session, "select_metro_specialty", "Metropolitan Area:", choices = sort(unique(x$metro_name)))
  })
  ##Filter changed based on "Metropolitan Area"
  observe({
    x <- d_docs %>% filter(metro_name %in% c(input$select_metro)) %>% st_drop_geometry()
    updateSelectizeInput(session, "select_specialty_metro", "Specialty:", choices = sort(unique(x$specialty1)))
  })
  ##Filter changed based on "Zipcode"
  observe({
    x <- d_zcta() %>% filter(zip5 %in% c(input$select_zip))
    updateSelectizeInput(session, "select_specialty_zip", "Specialty:", choices = sort(unique(x$specialty1)))
  })
### Processing Data
  zcta_filter_f <- function(zips, dist) {
    z_buff <- z_centroid %>%
      filter(zip5 %in% zips) %>%
      st_buffer(dist = dist*1609.34)
    d_buff <- st_intersection(z_buff, d_docs) %>% 
      st_drop_geometry() %>% 
      select(ID, specialty1, zip5)

    return(d_buff)
  }
  
  d_zcta <- reactive({
    zcta_filter_f(zips = input$select_zip, dist = input$buffer_slide)
  })
  d_subset<-reactive({
    d_nogeo <- d_docs %>% st_drop_geometry()
    doc_id <- d_nogeo %>% select(ID)
    if(input$filter_options=="Provider Name"){
      if(is.null(input$select_phys)){d_nogeo}
      else{ d_nogeo%>%filter(phys_name%in%c(input$select_phys))}
     
                       
    }
    else if (input$filter_options=="Specialty"){
      if(is.null(input$select_metro_specialty)){
        d_nogeo%>%filter(
        specialty1%in%c(input$select_specialty))}
      else{d_nogeo%>%filter(
        specialty1%in%c(input$select_specialty),
        metro_name%in%c(input$select_metro_specialty))}
      
      
    }
    else if (input$filter_options=="Metropolitan Area"){
      if(is.null(input$select_specialty_metro)){d_nogeo%>%filter(metro_name%in%c(input$select_metro))}
      else {d_nogeo%>%filter(metro_name%in%c(input$select_metro),
                             specialty1%in%c(input$select_specialty_metro))}
      
                       
      
    }
    else if (input$filter_options=="Search by Zipcode"){
      if(is.null(input$select_specialty_zip)){
        d_zip<-d_zcta()%>% select(ID)
        d_zip_special<-inner_join(d_zip, d_nogeo) 
        d_zip_special
      }
      else { d_zip<-d_zcta()%>% select(ID)
      d_zip_special<-inner_join(d_zip, d_nogeo) %>% filter(specialty1 %in% c(input$select_specialty_zip))
      d_zip_special}
     
    }
  })
  

  # d_spatial <- reactive({
  #   
  #   d_sf <- select(d_docs, ID)
  #   
  #   validate(need(nrow(d_subset()) > 0,
  #                 'Please extend the search radius.  No alumni are located within the specified zipcode and surrounding area.'))
  #   if (nrow(d_subset()) > 0) {
  #     d_subset() %>% 
  #       inner_join(d_sf) %>% 
  #       st_as_sf() %>% 
  #       st_transform(4326) 
  #   }
  # })
  d_spatial <- reactive({
    
    d_sf <- select(d_docs, ID)
    if(input$filter_options=="Specialty"){
      validate(need(nrow(d_subset()) > 0,
                    'Please select Speciality!'))
      if (nrow(d_subset()) > 0) {
        d_subset() %>% 
          inner_join(d_sf) %>% 
          st_as_sf() %>% 
          st_transform(4326) 
      }
      
    }
    else if (input$filter_options=="Metropolitan Area"){
      validate(need(nrow(d_subset()) > 0,
                    'Please select Metropolitan Area!'))
      if (nrow(d_subset()) > 0) {
        d_subset() %>% 
          inner_join(d_sf) %>% 
          st_as_sf() %>% 
          st_transform(4326) 
      }
    }
    else if (input$filter_options=="Search by Zipcode"){
      validate(need(nrow(d_subset()) > 0,
                    'Please fill Zipcode!'))
      if (nrow(d_subset()) > 0) {
        d_subset() %>% 
          inner_join(d_sf) %>% 
          st_as_sf() %>% 
          st_transform(4326) 
      }
    }
    else if (input$filter_options=="Provider Name"){
      
      if (nrow(d_subset()) > 0) {
        d_subset() %>% 
          inner_join(d_sf) %>% 
          st_as_sf() %>% 
          st_transform(4326) 
      }
    }
    # validate(need(nrow(d_subset()) > 0,
    #               'Please extend the search radius.  No alumni are located within the specified zipcode and surrounding area.'))
    # if (nrow(d_subset()) > 0) {
    #   d_subset() %>% 
    #     inner_join(d_sf) %>% 
    #     st_as_sf() %>% 
    #     st_transform(4326) 
    # }
  })

  
 
### Map
###Dynamic leafletmap w/ table
  in_bounding_box <- function(data, latitude, longitude, bounds) {
      data %>%
          dplyr::filter(
              latitude > bounds$south &
                  latitude < bounds$north &
                  longitude < bounds$east & 
                  longitude > bounds$west
          )
  }

  
  output$map <- renderLeaflet({
    
    pop_link <- d_spatial() %>% 
      mutate(phys_link = paste0("<a href='", "https://google.com/search?q=", phys_search,
                                " 'target='_blank'>", "Google this physician", "</a>")) %>% 
      st_drop_geometry()
    
    map_labs <- paste(sep = "<br/>", d_spatial()$phys_name, d_spatial()$institution, pop_link$phys_link)
    
    # leaflet(d_spatial()
    #         ) %>% 
    #   addTiles() %>%
    #   addMarkers(
    #     clusterOptions = markerClusterOptions(),
    #     popup = ~map_labs) %>% 
    #   addProviderTiles(providers$OpenStreetMap, group = 'Open Street Map') %>%
    #   addProviderTiles(providers$CartoDB.Positron, group = 'Carto Positron') %>%
    #   addLayersControl(position = "topright",
    #                    baseGroups = c('Open Street Map', 'Carto Positron'))
    leaflet() %>%
        # addTiles() %>%
        ##Creat unique map style, setting only English via mapbox
        # addMapboxTiles(style_id = "cl0jlkbg1000114mnblpew8er",
        #                username = "liuc3k",
        #                access_token='pk.eyJ1IjoibGl1YzNrIiwiYSI6ImNsMGlubDEyeTA0YzczanA3Ym12eXVobmMifQ.n2HsHJpyvy5RNxzetsxoWA')%>%
        # addMarkers(
        #     clusterOptions = markerClusterOptions(),
        #     popup = ~map_labs) %>% 
        # addProviderTiles(providers$OpenStreetMap, group = 'Open Street Map') %>%
        # addProviderTiles(providers$OpenStreetMap.CH, group = 'Open Street Map') %>%
        
        # addProviderTiles(providers$CartoDB.Positron, group = 'Carto Positron') %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # addLayersControl(position = "topright",
        #                  baseGroups = c('Open Street Map', 'Carto Positron'))%>%

        addMarkers(
                data=d_spatial(),
                ~longitude,
                ~latitude,
                clusterOptions = markerClusterOptions(),
                popup = ~map_labs,
                popupOptions = popupOptions(closeButton = FALSE)
                  
    )%>%setView(-45.236775,14.211139,zoom = 2)
      # fitBounds(lng1=min(d_docs$longitude), lat1=min(d_docs$latitude), lng2=max(d_docs$longitude), lat2=max(d_docs$latitude))
      
  })
  data_map <- reactive({
      if (is.null(input$map_bounds)) {
          d_spatial()
      } else {
          bounds <- input$map_bounds
          in_bounding_box(data=d_spatial(), latitude, longitude, bounds)
      }
  })
  ### Result Number
  n_docs <- reactive({
      # if(nrow(d_subset()) > 1) {
      #     paste0("\nThere are ", nrow(d_subset()), " physicians that fit your criteria.")
      # } else {
      #     paste0("\nThere is ", nrow(d_subset()), " physician that fits your criteria.")
      # }
      if(nrow(data_map()) > 1) {
          paste0("\nThere are ", nrow(data_map()), " physicians that fit your criteria.")
      } else {
          paste0("\nThere is ", nrow(data_map()), " physician that fits your criteria.")
      }
  }) 
  output$docs_in_table <- renderText(n_docs()) 
### Table 
  d_table <- reactive({

    # d_spatial() %>%
    #   arrange(phys_name) %>%
    #   st_drop_geometry() %>%
    #   select(Provider = phys_link,
    #          Institution = institution,
    #          Specialty = specialty1,
    #          City,
    #          State)
      data_map() %>%
          arrange(phys_name) %>%
          st_drop_geometry() %>%
          select(Provider = phys_link,
                 Institution = institution,
                 Specialty = specialty1,
                 City,
                 State)
  })
  
  
  output$alumni_tb<-renderDT(
    
    d_table(),
    rownames = FALSE,
    filter='top',
    class = 'cell-border stripe',
    escape = FALSE,
    options = list(
      pageLength = 5,
    #   # paging = FALSE,
    #   searchHighlight = TRUE,
    #   autoWidth = FALSE,
    #   # scroller = TRUE,
    #   scrollX = TRUE,
      # scrollY = "700px"#,
    #   fixedColumns = FALSE,
       dom = 'tp'
       )
  )
  # output$dt <- downloadHandler(
  #   filename = function() {
  #     paste('data-', Sys.Date(), '.csv', sep='')
  #   },
  #   content = function(con) {
  #     write.csv(d_table(), con)
  #   }
  # )
  }
shinyApp(ui = ui, server = server)
