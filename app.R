library(shiny)
library(dplyr)
library(shinydashboard)
library(shinyalert)
library(shinyjs)
library(reticulate)
use_python("/Users/antongaev/anaconda/bin/python", required = T)
library(DT)
library(tidyr)
library(stringr)
library(jsonlite)
library(scales)

#allow >5mb files for uploading
options(shiny.maxRequestSize=30*1024^2) 

source("car_types.R")
source("addresses.R")
source("duration.R")
source("two_loadings_detector.R")
source("weight.R")
source("distances.R")
source("agent_filters.R")

ui <- dashboardPage(
    dashboardHeader(title = "ИнфоКафе",
                    #set the help
                    dropdownMenu(
                        type = "notifications", 
                        icon = icon("question-circle"),
                        badgeStatus = NULL,
                        headerText = textOutput("res")
                    )),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Загрузка новых данных", tabName = "data_load", icon = icon("download")),
            menuItem("Обработка типов груза", tabName = "no_cargo", icon = icon("suitcase")),
            menuItem("Обработка типов ТС", tabName = "no_car", icon = icon("truck")),
            menuItem("Фильтры контрагентов", tabName = "agent_filters", icon = icon("window-close")),
            menuItem("Обработка адресов", tabName = "wrong_departure", icon = icon("flag")),
            menuItem("Обработанная таблица", tabName = "final_table", icon = icon("columns"))
        )
    ),
    dashboardBody(
        tabItems(
            # the First tab content
            tabItem(tabName = "data_load",
                    fluidRow(
                        column(1),
                        column(
                            9, 
                            textOutput("help_message"),
                            br(),
                            fileInput("file1", "Загрузите файл в формате CSV",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            actionButton(inputId = "clicks", label = "Поехали!", icon("rocket"), 
                                         style="color: #fff; background-color: #EB70E5; border-color: #2e6da4")
                        ),
                        column(2))
            ),
            # First tab content
            tabItem(tabName = "final_table",
                    fluidRow(
                        actionButton(inputId = "form_the_table", label = "сформировать таблицу"),
                        textInput("dt_name", "Введите наименование для таблицы"),
                        actionButton(inputId = "save_dt", label = "сохранить"),
                        DT::dataTableOutput("thedataframe")
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "wrong_departure",
                    fluidRow(
                        column(4,
                               div(class="hidden",
                                   regions <- read.csv2("~/Downloads/regions.csv"),
                                   regions <- select(regions, region)
                               ),
                               actionButton(inputId = "addresses_starter", label = "Начать обработку адресов", 
                                            style="color: #fff; background-color: #EB70E5; border-color: #2e6da4"),
                               br(),
                               br(),
                               selectizeInput("regions", "Выберите субъект федерации",
                                           choices = (regions$region),
                                           options = list(create = TRUE)),
                               textInput("newcity", "Введите наименование населенного пункта для записи в базе данных"),
                               textInput("newstreet", "Введите наименование улицы для записи в базе данных"),
                               numericInput("city_latitude", "Введите широту координат населенного пункта", value = 59.57, min = 40, max = 70, step = 0.01),
                               numericInput("city_longitude", "Введите долготу координат населенного пункта", value = 30.19, min = 19, max = 161, step = 0.01),
                               actionButton(inputId = "addcity", label = "Добавить в базу данных"),
                               actionButton(inputId = "change_address", label = "Изменить адрес в базе данных")),
                        column(8,                      
                               DT::dataTableOutput('nodeparture'))
                    ),
                    br(),
                    fluidRow(
                      actionButton("show_stp", "Показать адреса в Санкт-Петербурге"),
                      DT::dataTableOutput('stp_streets')
                    ),
                    fluidRow(
                      actionButton("show_msc", "Показать адреса в Москве"),
                      DT::dataTableOutput('msc_streets')
                    ),
                    fluidRow(
                      actionButton("show_rf", "Показать адреса в городах"),
                      DT::dataTableOutput('city_streets')
                    )
            ),
            
            # Fourth tab content
            tabItem(tabName = "no_cargo", theme = "sezar.css",
                    div(class="hidden",
                        cargo_profiles <- read.csv2("cargo_profiles.csv"),
                        cargo_types <- read.csv2(paste0(tail(cargo_profiles$system_cargo_profile_name, n=1), ".csv")),
                        cargo_types <- select(cargo_types, cargo_type, original_value),
                        cargo_type_table <- data.frame(sort(table(cargo_types$cargo_type), decreasing = TRUE))
                    ),
                    fluidPage(
                        fluidRow(
                            column(1),
                            column(
                                5,
                                selectInput("select_cargo_type_profile", "Выберите профиль типов груза",
                                            choices = (cargo_profiles$user_cargo_profile_name))
                            ),
                            column(
                                5, br(),
                                actionButton("save_cargo_profile", "Создать профиль", icon("plus"), 
                                             style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                            ),
                            column(1
                            )
                        ),
                        fluidRow(
                            column(1),
                            column(
                                5,
                                actionButton("show_unhandled", "Выбрать профиль", icon("check-circle"), 
                                             style="color: #fff; background-color: #00FFD9; border-color: #2e6da4")
                            ),
                            column(
                                5,
                                useShinyalert(),
                                actionButton("delete_cargo_profile", "Удалить выбранный профиль", icon("times"), 
                                             style="color: #fff; background-color: #F20303; border-color: #2e6da4")
                            ),
                            column(1
                            )
                        ),
                        fluidRow(
                            column(1),
                            column(
                                10, useShinyjs(),
                                DT::dataTableOutput('x14'),
                                textOutput('z14'),
                                verbatimTextOutput('y14')
                            ),
                            column(1
                            )
                        ),
                        
                        fluidRow(
                            column(1),
                            column(5,
                                   box(width = 12,
                                       selectInput("select_cargo_type", "Выберите группу",
                                                   choices = (cargo_type_table[1]))
                                   )
                            ),
                            column(5, br(), br(),
                                   actionButton("add_elements_to_selected_groups", "Добавить выделенные элементы в выбранную группу", icon("plus"), 
                                                style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                                   
                            ),
                            column(1)
                        ),
                        fluidRow(
                            column(3,
                                   actionButton("create_new_group", "Создать новую группу", icon("plus"), 
                                                style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                            ),
                            column(3,
                                   actionButton("delete_group", "Удалить выделенную группу", icon("times"), 
                                                style="color: #fff; background-color: #F20303; border-color: #2e6da4")
                            ),
                            column(3,
                                   actionButton("create_new_rule", "Создать новое правило", icon("plus"), 
                                                style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                            ),
                            column(3,
                                   actionButton("delete_rule", "Удалить выделенное правило", icon("times"), 
                                                style="color: #fff; background-color: #F20303; border-color: #2e6da4")
                            )
                        )
                    )
            ),
            
            # Fourth tab content
            tabItem(tabName = "no_car", theme = "sezar.css",
                    div(class="hidden",
                        car_profiles <- read.csv2("car_profiles.csv"),
                        car_types <- read.csv2(paste0(tail(car_profiles$system_car_profile_name, n=1), ".csv")),
                        car_types <- select(car_types, car_type, original_value),
                        car_type_table <- data.frame(sort(table(car_types$car_type), decreasing = TRUE))
                    ),
                    fluidPage(
                      fluidRow(
                        column(1),
                        column(
                          5,
                          selectInput("select_car_type_profile", "Выберите профиль типов груза",
                                      choices = (car_profiles$user_car_profile_name))
                        ),
                        column(
                          5, br(),
                          actionButton("save_car_profile", "Создать профиль", icon("plus"), 
                                       style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                        ),
                        column(1
                        )
                      ),
                      fluidRow(
                        column(1),
                        column(
                          5,
                          actionButton("show_uncared", "Выбрать профиль", icon("check-circle"), 
                                       style="color: #fff; background-color: #00FFD9; border-color: #2e6da4")
                        ),
                        column(
                          5,
                          useShinyalert(),
                          actionButton("delete_car_profile", "Удалить выбранный профиль", icon("times"), 
                                       style="color: #fff; background-color: #F20303; border-color: #2e6da4")
                        ),
                        column(1
                        )
                      ),
                      fluidRow(
                        column(1),
                        column(
                          10, useShinyjs(),
                          DT::dataTableOutput('car14'),
                          textOutput('carz14'),
                          verbatimTextOutput('cary14')
                        ),
                        column(1
                        )
                      ),
                      
                      fluidRow(
                        column(1),
                        column(5,
                               box(width = 12,
                                   selectInput("select_car_type", "Выберите группу",
                                               choices = (cargo_type_table[1]))
                               )
                        ),
                        column(5, br(), br(),
                               actionButton("add_elements_to_cared_groups", "Добавить выделенные элементы в выбранную группу", icon("plus"), 
                                            style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                               
                        ),
                        column(1)
                      ),
                      fluidRow(
                        column(3,
                               actionButton("create_car_group", "Создать новую группу", icon("plus"), 
                                            style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                        ),
                        column(3,
                               actionButton("delete_car_group", "Удалить выделенную группу", icon("times"), 
                                            style="color: #fff; background-color: #F20303; border-color: #2e6da4")
                        ),
                        column(3,
                               actionButton("create_new_car_rule", "Создать новое правило", icon("plus"), 
                                            style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                        ),
                        column(3,
                               actionButton("delete_car_rule", "Удалить выделенное правило", icon("times"), 
                                            style="color: #fff; background-color: #F20303; border-color: #2e6da4")
                        )
                      )
                    )
            ),
            tabItem(tabName = "agent_filters",
                    fluidRow(
                        actionButton(inputId = "show_all_agents", label = "Показать всех контрагентов"),
                        DT::dataTableOutput('all_agents')
                    ),
                    fluidRow(
                        column(4,
                               actionButton(inputId = "add_agent_filter", label = "Добавить в группу отфильтрованных", icon = icon("arrow-down"), 
                                            style="color: #fff; background-color: #F20303; border-color: #2e6da4")),
                        column(1),
                        column(7, actionButton(inputId = "delete_agent_filter", label = "Убрать из фильтра", icon = icon("arrow-up"), 
                                               style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                        )
                    ),
                    fluidRow(
                        DT::dataTableOutput('filtered_agents'),
                        actionButton(inputId = "show_filtered_agents", label = "Показать неучитываемых контрагентов")
                    )
            )
        )
    )
)

server <- function(input, output, session) {
  
  #state reactive values
  withcartype <- NULL
  makeReactiveBinding("withcartype")
  departure_final <- NULL
  makeReactiveBinding("departure_final")
  destination_final <- NULL
  makeReactiveBinding("destination_final")
    
  #show cities dts
  observeEvent(input$show_stp, {stpetersburgstreets <- read.csv2("stpetersburgstreets.csv")
    stpetersburgstreets <- dplyr::select(stpetersburgstreets, -c(X))
    output$stp_streets <- DT::renderDataTable(stpetersburgstreets)
  })
  
  observeEvent(input$show_msc, {moscowstreets <- read.csv2("moscowstreets.csv")
    moscowstreets <- dplyr::select(moscowstreets, -c(X))
    output$msc_streets <- DT::renderDataTable(moscowstreets)
  })
  
  observeEvent(input$show_rf, {citiesnew <- read.csv2("citiesnew.csv")
    citiesnew <- dplyr::select(citiesnew, -c(X))
    output$city_streets <- DT::renderDataTable(citiesnew)
  })
  
  observeEvent(input$stp_streets_rows_selected, {
    selected_filters <- isolate(input$stp_streets_rows_selected)
    
    stpetersburgstreets <- read.csv2("stpetersburgstreets.csv")
    stpetersburgstreets <- filter(stpetersburgstreets, X == selected_filters[1])

    updateSelectizeInput(session, "regions", "Выберите субъект федерации", choices = (regions$region),
                         options = list(create = TRUE),
                         selected = "Санкт-Петербург")
    
    updateTextInput(session, "newcity", "Введите наименование населенного пункта для записи в базе данных", stpetersburgstreets$city)
    updateTextInput(session, "newstreet", "Введите наименование улицы для записи в базе данных", stpetersburgstreets$street)
    
    updateNumericInput(session, "city_latitude", "Введите широту координат населенного пункта", value = as.double(as.character(stpetersburgstreets$latitude)), min = 40, max = 70, step = 0.01)
    updateNumericInput(session, "city_longitude", "Введите долготу координат населенного пункта", value = as.double(as.character(stpetersburgstreets$longitude)), min = 19, max = 161, step = 0.01)
  })
  
  observeEvent(input$msc_streets_rows_selected, {
    selected_filters <- isolate(input$msc_streets_rows_selected)
    
    moscowstreets <- read.csv2("moscowstreets.csv")
    moscowstreets <- filter(moscowstreets, X == selected_filters[1])
    
    updateSelectizeInput(session, "regions", "Выберите субъект федерации", choices = (regions$region),
                         options = list(create = TRUE),
                         selected = "Москва")
    
    updateTextInput(session, "newcity", "Введите наименование населенного пункта для записи в базе данных", moscowstreets$city)
    updateTextInput(session, "newstreet", "Введите наименование улицы для записи в базе данных", moscowstreets$street)
    
    updateNumericInput(session, "city_latitude", "Введите широту координат населенного пункта", value = as.double(as.character(moscowstreets$latitude)), min = 40, max = 70, step = 0.01)
    updateNumericInput(session, "city_longitude", "Введите долготу координат населенного пункта", value = as.double(as.character(moscowstreets$longitude)), min = 19, max = 161, step = 0.01)
  })
  
  observeEvent(input$city_streets_rows_selected, {
    selected_filters <- isolate(input$city_streets_rows_selected)
    
    citiesnew <- read.csv2("citiesnew.csv")
    citiesnew <- filter(citiesnew, X == selected_filters[1])
    
    updateSelectizeInput(session, "regions", "Выберите субъект федерации", choices = (regions$region),
                         options = list(create = TRUE),
                         selected = as.character(citiesnew$region))
    
    updateTextInput(session, "newcity", "Введите наименование населенного пункта для записи в базе данных", citiesnew$city)
    updateTextInput(session, "newstreet", "Введите наименование улицы для записи в базе данных", citiesnew$street)
    
    updateNumericInput(session, "city_latitude", "Введите широту координат населенного пункта", value = as.double(as.character(citiesnew$latitude)), min = 40, max = 70, step = 0.01)
    updateNumericInput(session, "city_longitude", "Введите долготу координат населенного пункта", value = as.double(as.character(citiesnew$longitude)), min = 19, max = 161, step = 0.01)
  })
  
  observeEvent(input$change_address, {
    if (input$regions == "Санкт-Петербург") {
      selected_filters <- isolate(input$stp_streets_rows_selected)
      stpetersburgstreets <- read.csv2("stpetersburgstreets.csv")
      stpetersburgstreets <- filter(stpetersburgstreets, X != selected_filters[1])
      stpetersburgstreets <- dplyr::select(stpetersburgstreets, region, city, street, latitude, longitude)
      
      region <- "Санкт-Петербург"
      city <- "Санкт-Петербург"
      street <- input$newstreet
      latitude <- as.character(input$city_latitude)
      longitude <- as.character(input$city_longitude)
      addition <- data.frame(region, city, street, latitude, longitude)
      stpetersburgstreets <- rbind(stpetersburgstreets, addition)
      write.csv2(stpetersburgstreets, file = "stpetersburgstreets.csv")
      output$stp_streets <- DT::renderDataTable(stpetersburgstreets)
      
    } else if (input$regions == "Москва") {
      selected_filters <- isolate(input$msc_streets_rows_selected)
      moscowstreets <- read.csv2("moscowstreets.csv")
      moscowstreets <- filter(moscowstreets, X != selected_filters[1])
      moscowstreets <- dplyr::select(moscowstreets, region, city, street, latitude, longitude)
      
      region <- "Москва"
      city <- "Москва"
      street <- input$newstreet
      latitude <- as.character(input$city_latitude)
      longitude <- as.character(input$city_longitude)
      addition <- data.frame(region, city, street, latitude, longitude)
      moscowstreets <- rbind(moscowstreets, addition)
      write.csv2(moscowstreets, file = "moscowstreets.csv")
      output$msc_streets <- DT::renderDataTable(moscowstreets)
      
    } else {
      selected_filters <- isolate(input$city_streets_rows_selected)
      citiestest <- read.csv2("citiesnew.csv")
      citiestest <- filter(citiestest, X != selected_filters[1])
      citiestest <- dplyr::select(citiestest, region, city, street, latitude, longitude)
      
      regions <- read.csv2("~/Downloads/regions.csv")
      regions_plus <- regions
      regions <- select(regions, region)
      regions_plus$X <- grepl(input$regions, regions_plus$region)
      regions_plus <- filter(regions_plus, X == TRUE)
      if (nrow(regions_plus) == 0) {
        region <- input$regions
        addition <- data.frame(region)
        regions <- rbind(regions, addition)
        write.csv2(regions, "~/Downloads/regions.csv")
      }
      region <- input$regions
      city <- input$newcity
      street <- input$newstreet
      latitude <- as.character(input$city_latitude)
      longitude <- as.character(input$city_longitude)
      addition <- data.frame(region, city, street, latitude, longitude)
      citiestest <- rbind(citiestest, addition)
      write.csv2(citiestest, file = "citiesnew.csv")
      output$city_streets <- DT::renderDataTable(citiestest)
      
    }
  })
  
  #add unparsed address func
  observeEvent(input$addcity, {
    
    if (input$regions == "Санкт-Петербург") {
      
      stpetersburgstreets <- read.csv2("stpetersburgstreets.csv")
      stpetersburgstreets <- dplyr::select(stpetersburgstreets, region, city, street, latitude, longitude)
      region <- "Санкт-Петербург"
      city <- "Санкт-Петербург"
      street <- input$newstreet
      latitude <- as.character(input$city_latitude)
      longitude <- as.character(input$city_longitude)
      addition <- data.frame(region, city, street, latitude, longitude)
      stpetersburgstreets <- rbind(stpetersburgstreets, addition)
      write.csv2(stpetersburgstreets, file = "stpetersburgstreets.csv")
      unparsable <- read.csv2("unparsable.csv")
      unparsable$X <- grepl(input$newstreet, unparsable$to_maps..duplicated.to_maps....)
      unparsable <- filter(unparsable, X == FALSE)
      unparsable <- select(unparsable, to_maps..duplicated.to_maps....)
      write.csv2(unparsable, "unparsable.csv")
      output$nodeparture <- DT::renderDataTable(unparsable, colnames = c('Необработанные значения'))
      
    } else if (input$regions == "Москва") {
      
      moscowstreets <- read.csv2("moscowstreets.csv")
      moscowstreets <- dplyr::select(moscowstreets, region, city, street, latitude, longitude)
      region <- "Москва"
      city <- "Москва"
      street <- input$newstreet
      latitude <- input$city_latitude
      longitude <- input$city_longitude
      addition <- data.frame(region, city, street, latitude, longitude)
      moscowstreets <- rbind(moscowstreets, addition)
      write.csv2(moscowstreets, file = "moscowstreets.csv")
      unparsable <- read.csv2("unparsable.csv")
      unparsable$X <- grepl(input$newstreet, unparsable$to_maps..duplicated.to_maps....)
      unparsable <- filter(unparsable, X == FALSE)
      unparsable <- select(unparsable, to_maps..duplicated.to_maps....)
      write.csv2(unparsable, "unparsable.csv")
      output$nodeparture <- DT::renderDataTable(unparsable, colnames = c('Необработанные значения'))
      
    } else {
      
      citiestest <- read.csv2("citiesnew.csv")
      citiestest <- dplyr::select(citiestest, region, city, street, latitude, longitude)
      regions <- read.csv2("~/Downloads/regions.csv")
      regions_plus <- regions
      regions <- select(regions, region)
      regions_plus$X <- grepl(input$regions, regions_plus$region)
      regions_plus <- filter(regions_plus, X == TRUE)
      if (nrow(regions_plus) == 0) {
        region <- input$regions
        addition <- data.frame(region)
        regions <- rbind(regions, addition)
        write.csv2(regions, "~/Downloads/regions.csv")
      }
      region <- input$regions
      city <- input$newcity
      street <- input$newstreet
      latitude <- input$city_latitude
      longitude <- input$city_longitude
      addition <- data.frame(region, city, street, latitude, longitude)
      citiestest <- rbind(citiestest, addition)
      write.csv2(citiestest, file = "citiesnew.csv")
      unparsable <- read.csv2("unparsable.csv")
      unparsable$X <- grepl(input$newcity, unparsable$to_maps..duplicated.to_maps....)
      unparsable <- filter(unparsable, X == FALSE)
      unparsable <- select(unparsable, to_maps..duplicated.to_maps....)
      write.csv2(unparsable, "unparsable.csv")
      output$nodeparture <- DT::renderDataTable(unparsable, colnames = c('Необработанные значения'))
      
    }
  })
  
  #filters fuctions
  filtered_agents_foo <- eventReactive({input$add_agent_filter
    input$delete_agent_filter
    input$show_filtered_agents}, {
      agent_filters <- read.csv2("agent_filters.csv")
      agent_filters <- select(agent_filters, agent_number, agent_filter)
    })
  output$filtered_agents <- DT::renderDataTable(filtered_agents_foo(), filter = 'top', colnames = c('номер', 'Неучитываемые контрагенты'),
                                                extensions = 'Buttons', options = list(
                                                  columnDefs = list(
                                                    list(targets = 1, visible = FALSE)
                                                  )
                                                )
  )
  observeEvent(input$add_agent_filter, {
    allagents <- read.csv2("allagents.csv")
    allagents <- select(allagents, Var1, Freq)
    selected_filters <- isolate(input$all_agents_rows_selected)
    allagents$id <- c(1:nrow(allagents))
    changed_values <- filter(allagents, id == selected_filters[1])
    allagents <- filter(allagents, id != selected_filters[1])
    for (i in 2:length(selected_filters)){
      changed_values_add <- filter(allagents, id == selected_filters[i])
      changed_values <- rbind(changed_values, changed_values_add)
    }
    for (i in 1:length(selected_filters)){
      allagents <- filter(allagents, id != selected_filters[i])
    }
    write.csv2(allagents, "allagents.csv")
    agent_filters <- read.csv2("agent_filters.csv")
    agent_filters <- select(agent_filters, agent_number, agent_filter)
    agent_number <- 1:length(selected_filters)
    addition <- data.frame(agent_number)
    addition$agent_filter <- changed_values$Var1
    agent_filters <- rbind(agent_filters, addition)
    write.csv2(agent_filters, "agent_filters.csv")
    
  })
  
  observeEvent(input$delete_agent_filter, {
    agent_filters <- read.csv2("agent_filters.csv")
    agent_filters <- select(agent_filters, agent_number, agent_filter)
    selected_filters <- isolate(input$filtered_agents_rows_selected)
    agent_filters$id <- c(1:nrow(agent_filters))
    changed_values <- filter(agent_filters, id == selected_filters[1])
    agent_filters <- filter(agent_filters, id != selected_filters[1])
    for (i in 2:length(selected_filters)){
      changed_values_add <- filter(agent_filters, id == selected_filters[i])
      changed_values <- rbind(changed_values, changed_values_add)
    }
    for (i in 1:length(selected_filters)){
      agent_filters <- filter(agent_filters, id != selected_filters[i])
    }
    write.csv2(agent_filters, "agent_filters.csv")
    allagents <- read.csv2("allagents.csv")
    allagents <- select(allagents, Var1, Freq)
    Freq <- 1:length(selected_filters)
    addition <- data.frame(Freq)
    addition$Var1 <- changed_values$agent_filter
    allagents <- rbind(allagents, addition)
    write.csv2(allagents, "allagents.csv")
    
  })
  
  #cargo type functions
  observeEvent(input$select_cargo_type_profile, {
    cargo_profiles <- read.csv2("cargo_profiles.csv")
    cargo_profiles <- filter(cargo_profiles, user_cargo_profile_name == input$select_cargo_type_profile)
    cargo_types <- read.csv2(paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
    cargo_types <- select(cargo_types, cargo_type, original_value)
    cargo_type_table <- data.frame(sort(table(cargo_types$cargo_type), decreasing = TRUE))
    updateSelectInput(session, "select_cargo_type", choices = (cargo_type_table[1]))
  })
  
  observeEvent(input$create_new_group, {
    
    shinyalert(
      title = "Создать новую группу",
      text = "Введите названия для новой группы",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "input",
      inputType = "text",
      inputValue = "",
      inputPlaceholder = "",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#00FFD9",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x != FALSE) {#load profiles
        cargo_profiles <- read.csv2("cargo_profiles.csv")
        cargo_profiles <- filter(cargo_profiles, user_cargo_profile_name == input$select_cargo_type_profile)
        cargo_types <- read.csv2(paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
        cargo_types <- select(cargo_types, cargo_type, original_value)
        cargo_type_table <- data.frame(sort(table(cargo_types$cargo_type), decreasing = TRUE))
        Var1 <- input$shinyalert
        Freq <- 1
        addition <- data.frame(Var1, Freq)
        cargo_type_table <- rbind(cargo_type_table, addition)
        updateSelectInput(session, "select_cargo_type", choices = (cargo_type_table[1]))
      }}
    )
  })
  
  observeEvent(input$delete_group, {
    
    shinyalert(
      title = "Вы точно хотите удалить эту группу?",
      text = "Результат будет необратим",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Удалить",
      confirmButtonCol = "#ED4242",
      cancelButtonText = "Нет, оставить",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x == TRUE) {#to delete a group i need to upload current profile
        #delete selected group also means to delete all the elements of this group, that is why confirmation is needed
        daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
        dfsix = as.matrix(daf[12])
        original_cargos <- data.frame(dfsix)
        
        cargo_profiles <- read.csv2("cargo_profiles.csv")
        cargo_profiles <- filter(cargo_profiles, user_cargo_profile_name == input$select_cargo_type_profile)
        cargo_types <- read.csv2(paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
        cargo_types <- select(cargo_types, cargo_type, original_value, Freq)
        
        #check the selected group
        changed_values <- filter(cargo_types, cargo_type == input$select_cargo_type)
        cargo_types <- filter(cargo_types, cargo_type != input$select_cargo_type)
        
        
        for (i in 1:nrow(changed_values)) {
          original_cargos$nas <- grepl(changed_values$original_value[i], dfsix)
          original_cargos_filters <- filter(original_cargos, nas == TRUE)
          if (nrow(original_cargos_filters) > 0) {
            original_cargos_filters <- data.frame(original_cargos_filters[!duplicated(original_cargos_filters), ])
            original_cargos_filters$cargo_type <- "необработанные"
            original_cargos_filters$original_value = c(as.matrix(original_cargos_filters[1]))
            original_cargos_filters$Freq <- nrow(original_cargos_filters)
          }
        }
        
        if (nrow(original_cargos_filters) > 0) {
          original_cargos_filters <- select(original_cargos_filters, cargo_type, original_value, Freq)
          #rbind the original values to the working prototype
          cargo_types <- select(cargo_types, cargo_type, original_value, Freq)
          cargo_types <- rbind(cargo_types, original_cargos_filters)
        }
        #save
        write.csv2(cargo_types, file = paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
        #update select input without this group inside confirmation
        cargo_type_table <- data.frame(sort(table(cargo_types$cargo_type), decreasing = TRUE))
        cargo_type_table <- filter(cargo_type_table, Freq > 0)
        updateSelectInput(session, "select_cargo_type", choices = (cargo_type_table[1]))
        click("show_unhandled")
      }}
    )
  })
  
  observeEvent(input$create_new_rule, {
    
    shinyalert(
      title = "Введите новое правило",
      text = "Все наименования грузов, содержащие данный набор символов, попадут в указанную группу",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "input",
      inputType = "text",
      inputValue = "",
      inputPlaceholder = "",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#00FFD9",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x != FALSE) {#load profiles
        cargo_profiles <- read.csv2("cargo_profiles.csv")
        cargo_profiles <- filter(cargo_profiles, user_cargo_profile_name == input$select_cargo_type_profile)
        cargo_types <- read.csv2(paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
        cargo_types <- select(cargo_types, cargo_type, original_value, Freq)
        cargo_type <- input$select_cargo_type
        original_value <- input$shinyalert
        cargo_types$id <- grepl(input$shinyalert, cargo_types$original_value)
        addition <- filter(cargo_types, id == TRUE)
        Freq <- nrow(addition)
        cargo_types <- filter(cargo_types, id == FALSE)
        cargo_types$id <- c(1:nrow(cargo_types))
        id <- nrow(cargo_types)+1
        addition <- data.frame(cargo_type, original_value, Freq, id)
        cargo_types <- rbind(cargo_types, addition)
        write.csv2(cargo_types, file = paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
        click("show_unhandled")
      }}
    )
    
  })
  
  observeEvent(input$add_elements_to_selected_groups, {
    cargo_profiles <- read.csv2("cargo_profiles.csv")
    cargo_profiles <- filter(cargo_profiles, user_cargo_profile_name == input$select_cargo_type_profile)
    cargo_types <- read.csv2(paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
    cargo_types <- select(cargo_types, cargo_type, original_value, Freq)
    selected_popkas <- isolate(input$x14_rows_selected)
    cargo_types$id <- c(1:nrow(cargo_types))
    changed_values <- filter(cargo_types, id == selected_popkas[1])
    if (length(selected_popkas) > 1){
      for (i in 2:length(selected_popkas)){
        changed_values_add <- filter(cargo_types, id == selected_popkas[i])
        changed_values <- rbind(changed_values, changed_values_add)
      }
    }
    changed_values$cargo_type <- input$select_cargo_type
    for (i in 1:length(selected_popkas)){
      cargo_types <- filter(cargo_types, id != selected_popkas[i])
    }
    #output$y14 <- DT::renderDataTable(cargo_types, filter = 'top')
    #output$y14 <- renderPrint( popkies )
    #output$z14 <- renderPrint(popkies[1])
    #output$z14 = DT::renderDataTable(changed_values, filter = 'top')
    cargo_types <- rbind(cargo_types, changed_values)
    cargo_types <- cargo_types[order(cargo_types$id), ]
    write.csv2(cargo_types, file = paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
  })
  
  
  observeEvent(input$save_cargo_profile, {
    
    shinyalert(
      title = "Создать профиль",
      text = "Введите названия для профиля",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "input",
      inputType = "text",
      inputValue = "",
      inputPlaceholder = "",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#00FFD9",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x != FALSE) {#load profiles
        #load profiles
        cargo_profiles <- read.csv2("cargo_profiles.csv")
        cargo_profiles <- select(cargo_profiles, user_cargo_profile_name, system_cargo_profile_name)
        
        #check the last number, then plus one
        system_cargo_profile_name <- paste0("cargo_types", as.numeric(gsub('\\D+','', tail(cargo_profiles, n=1)$system_cargo_profile_name))+1)
        
        #load input name
        user_cargo_profile_name <- input$shinyalert
        
        cargo_type <- c("необработанные", "другое")
        original_value <- c("Любые значения группы необработанные не будут учитываться в итоговой таблице", "Создавайте новые группы и добавляйте туда значения")
        Freq <- c(0, 0)
        cargo_types <- data.frame(cargo_type, original_value, Freq)
        
        write.csv2(cargo_types, file = paste0("cargo_types", as.numeric(gsub('\\D+','', tail(cargo_profiles, n=1)$system_cargo_profile_name))+1, ".csv"))
        
        
        #data frame it in profiles
        addition <- data.frame(user_cargo_profile_name, system_cargo_profile_name)
        cargo_profiles <- rbind(cargo_profiles, addition)
        write.csv2(cargo_profiles, file = "cargo_profiles.csv")
        
        updateSelectInput(session, "select_cargo_type_profile", choices = cargo_profiles$user_cargo_profile_name)
      }}
    )
    
  })
  
  observeEvent(input$delete_cargo_profile, {
    
    shinyalert(
      title = "Вы точно хотите удалить этот профиль?",
      text = "Результат будет необратим",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Удалить",
      confirmButtonCol = "#ED4242",
      cancelButtonText = "Нет, оставить",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x == TRUE) {#load profiles
        cargo_profiles <- read.csv2("cargo_profiles.csv")
        cargo_profiles <- select(cargo_profiles, user_cargo_profile_name, system_cargo_profile_name)
        #filter the input value
        cargo_profiles <- filter(cargo_profiles, user_cargo_profile_name != input$select_cargo_type_profile)
        #save
        write.csv2(cargo_profiles, file = "cargo_profiles.csv")
        updateSelectInput(session, "select_cargo_type_profile", choices = cargo_profiles$user_cargo_profile_name)
      }}
    )
  })
  
  #decomposite the rule function
  observeEvent(input$delete_rule, {
    
    daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
    dfsix = as.matrix(daf[12])
    original_cargos <- data.frame(dfsix)
    
    cargo_profiles <- read.csv2("cargo_profiles.csv")
    cargo_profiles <- filter(cargo_profiles, user_cargo_profile_name == input$select_cargo_type_profile)
    cargo_types <- read.csv2(paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
    cargo_types <- select(cargo_types, cargo_type, original_value, Freq)
    
    #recognise selected rule
    #grepl the rule from the original dataset
    
    selected_values <- isolate(input$x14_rows_selected)
    #popkies <- as.matrix(selected_popkas)
    cargo_types$id <- c(1:nrow(cargo_types))
    changed_values <- filter(cargo_types, id == selected_values[1])
    if (length(selected_values) > 1){
      for (i in 2:length(selected_values)){
        changed_values_add <- filter(cargo_types, id == selected_values[i])
        changed_values <- rbind(changed_values, changed_values_add)
      }
    }
    
    #changed_values$cargo_type <- input$select_cargo_type
    
    for (i in 1:length(selected_values)){
      cargo_types <- filter(cargo_types, id != selected_values[i])
    }
    for (i in 1:nrow(changed_values)) {
      original_cargos$nas <- grepl(changed_values$original_value[i], dfsix)
      original_cargos_filters <- filter(original_cargos, nas == TRUE)
      if (nrow(original_cargos_filters) > 0) {
        original_cargos_filters <- data.frame(original_cargos_filters[!duplicated(original_cargos_filters), ])
        original_cargos_filters$cargo_type <- "необработанные"
        original_cargos_filters$original_value = c(as.matrix(original_cargos_filters[1]))
        original_cargos_filters$Freq <- nrow(original_cargos_filters)
      }
    }
    if (nrow(original_cargos_filters) > 0) {
      original_cargos_filters <- select(original_cargos_filters, cargo_type, original_value, Freq)
      #rbind the original values to the working prototype
      cargo_types <- select(cargo_types, cargo_type, original_value, Freq)
      cargo_types <- rbind(cargo_types, original_cargos_filters)
    }
    #save
    write.csv2(cargo_types, file = paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #car type functions
  observeEvent(input$select_car_type_profile, {
    car_profiles <- read.csv2("car_profiles.csv")
    car_profiles <- filter(car_profiles, user_car_profile_name == input$select_car_type_profile)
    car_types <- read.csv2(paste0(car_profiles$system_car_profile_name, ".csv"))
    car_types <- select(car_types, car_type, original_value)
    car_type_table <- data.frame(sort(table(car_types$car_type), decreasing = TRUE))
    updateSelectInput(session, "select_car_type", choices = (car_type_table[1]))
  })
  
  observeEvent(input$create_car_group, {
    
    shinyalert(
      title = "Создать новую группу",
      text = "Введите названия для новой группы",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "input",
      inputType = "text",
      inputValue = "",
      inputPlaceholder = "",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#00FFD9",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x != FALSE) {#load profiles
        car_profiles <- read.csv2("car_profiles.csv")
        car_profiles <- filter(car_profiles, user_car_profile_name == input$select_car_type_profile)
        car_types <- read.csv2(paste0(car_profiles$system_car_profile_name, ".csv"))
        car_types <- select(car_types, car_type, original_value)
        car_type_table <- data.frame(sort(table(car_types$car_type), decreasing = TRUE))
        Var1 <- input$shinyalert
        Freq <- 1
        addition <- data.frame(Var1, Freq)
        car_type_table <- rbind(car_type_table, addition)
        updateSelectInput(session, "select_car_type", choices = (car_type_table[1]))
      }}
    )
  })
  
  observeEvent(input$delete_car_group, {
    
    shinyalert(
      title = "Вы точно хотите удалить эту группу?",
      text = "Результат будет необратим",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Удалить",
      confirmButtonCol = "#ED4242",
      cancelButtonText = "Нет, оставить",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x == TRUE) {#to delete a group i need to upload current profile
        #delete selected group also means to delete all the elements of this group, that is why confirmation is needed
        daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
        dfsix = as.matrix(daf[28])
        original_cargos <- data.frame(dfsix)
        
        car_profiles <- read.csv2("car_profiles.csv")
        car_profiles <- filter(car_profiles, user_car_profile_name == input$select_car_type_profile)
        car_types <- read.csv2(paste0(car_profiles$system_car_profile_name, ".csv"))
        car_types <- select(car_types, car_type, original_value, Freq)
        
        #check the selected group
        changed_values <- filter(car_types, car_type == input$select_car_type)
        car_types <- filter(car_types, car_type != input$select_car_type)
        
        
        for (i in 1:nrow(changed_values)) {
          original_cargos$nas <- grepl(changed_values$original_value[i], dfsix)
          original_cargos_filters <- filter(original_cargos, nas == TRUE)
          if (nrow(original_cargos_filters) > 0) {
            original_cargos_filters <- data.frame(original_cargos_filters[!duplicated(original_cargos_filters), ])
            original_cargos_filters$cargo_type <- "необработанные"
            original_cargos_filters$original_value = c(as.matrix(original_cargos_filters[1]))
            original_cargos_filters$Freq <- nrow(original_cargos_filters)
          }
        }
        
        if (nrow(original_cargos_filters) > 0) {
          original_cargos_filters <- select(original_cargos_filters, car_type, original_value, Freq)
          #rbind the original values to the working prototype
          car_types <- select(car_types, car_type, original_value, Freq)
          car_types <- rbind(car_types, original_cargos_filters)
        }
        #save
        write.csv2(car_types, file = paste0(car_profiles$system_car_profile_name, ".csv"))
        #update select input without this group inside confirmation
        car_type_table <- data.frame(sort(table(car_types$car_type), decreasing = TRUE))
        car_type_table <- filter(car_type_table, Freq > 0)
        updateSelectInput(session, "select_cargo_type", choices = (car_type_table[1]))
        click("show_uncared")
      }}
    )
  })
  
  observeEvent(input$create_new_car_rule, {
    
    shinyalert(
      title = "Введите новое правило",
      text = "Все наименования грузов, содержащие данный набор символов, попадут в указанную группу",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "input",
      inputType = "text",
      inputValue = "",
      inputPlaceholder = "",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#00FFD9",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x != FALSE) {#load profiles
        car_profiles <- read.csv2("car_profiles.csv")
        car_profiles <- filter(car_profiles, user_car_profile_name == input$select_car_type_profile)
        car_types <- read.csv2(paste0(car_profiles$system_car_profile_name, ".csv"))
        car_types <- select(car_types, car_type, original_value, Freq)
        car_type <- input$select_car_type
        original_value <- input$shinyalert
        car_types$id <- grepl(input$shinyalert, car_types$original_value)
        addition <- filter(car_types, id == TRUE)
        Freq <- nrow(addition)
        car_types <- filter(car_types, id == FALSE)
        car_types$id <- c(1:nrow(car_types))
        id <- nrow(car_types)+1
        addition <- data.frame(car_type, original_value, Freq, id)
        car_types <- rbind(car_types, addition)
        write.csv2(car_types, file = paste0(car_profiles$system_car_profile_name, ".csv"))
        click("show_uncared")
      }}
    )
    
  })
  
  observeEvent(input$add_elements_to_cared_groups, {
    car_profiles <- read.csv2("car_profiles.csv")
    car_profiles <- filter(car_profiles, user_car_profile_name == input$select_car_type_profile)
    car_types <- read.csv2(paste0(car_profiles$system_car_profile_name, ".csv"))
    car_types <- select(car_types, car_type, original_value, Freq)
    selected_popkas <- isolate(input$car14_rows_selected)
    car_types$id <- c(1:nrow(car_types))
    changed_values <- filter(car_types, id == selected_popkas[1])
    if (length(selected_popkas) > 1){
      for (i in 2:length(selected_popkas)){
        changed_values_add <- filter(car_types, id == selected_popkas[i])
        changed_values <- rbind(changed_values, changed_values_add)
      }
    }
    changed_values$car_type <- input$select_car_type
    for (i in 1:length(selected_popkas)){
      car_types <- filter(car_types, id != selected_popkas[i])
    }
    #output$y14 <- DT::renderDataTable(cargo_types, filter = 'top')
    #output$y14 <- renderPrint( popkies )
    #output$z14 <- renderPrint(popkies[1])
    #output$z14 = DT::renderDataTable(changed_values, filter = 'top')
    car_types <- rbind(car_types, changed_values)
    car_types <- car_types[order(car_types$id), ]
    write.csv2(car_types, file = paste0(car_profiles$system_car_profile_name, ".csv"))
  })
  
  
  observeEvent(input$save_car_profile, {
    
    shinyalert(
      title = "Создать профиль",
      text = "Введите названия для профиля",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "input",
      inputType = "text",
      inputValue = "",
      inputPlaceholder = "",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#00FFD9",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x != FALSE) {#load profiles
        #load profiles
        car_profiles <- read.csv2("car_profiles.csv")
        car_profiles <- select(car_profiles, user_car_profile_name, system_car_profile_name)
        
        #check the last number, then plus one
        system_car_profile_name <- paste0("car_types", as.numeric(gsub('\\D+','', tail(car_profiles, n=1)$system_car_profile_name))+1)
        
        #load input name
        user_car_profile_name <- input$shinyalert
        
        car_type <- c("необработанные", "другое")
        original_value <- c("Любые значения группы необработанные не будут учитываться в итоговой таблице", "Создавайте новые группы и добавляйте туда значения")
        Freq <- c(0, 0)
        car_types <- data.frame(car_type, original_value, Freq)
        
        write.csv2(car_types, file = paste0("car_types", as.numeric(gsub('\\D+','', tail(car_profiles, n=1)$system_car_profile_name))+1, ".csv"))
        
        
        #data frame it in profiles
        addition <- data.frame(user_car_profile_name, system_car_profile_name)
        car_profiles <- rbind(car_profiles, addition)
        write.csv2(car_profiles, file = "car_profiles.csv")
        
        updateSelectInput(session, "select_car_type_profile", choices = car_profiles$user_car_profile_name)
      }}
    )
    
  })
  
  observeEvent(input$delete_car_profile, {
    
    shinyalert(
      title = "Вы точно хотите удалить этот профиль?",
      text = "Результат будет необратим",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Удалить",
      confirmButtonCol = "#ED4242",
      cancelButtonText = "Нет, оставить",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { if(x == TRUE) {#load profiles
        car_profiles <- read.csv2("car_profiles.csv")
        car_profiles <- select(car_profiles, user_car_profile_name, system_car_profile_name)
        #filter the input value
        car_profiles <- filter(car_profiles, user_car_profile_name != input$select_car_type_profile)
        #save
        write.csv2(car_profiles, file = "car_profiles.csv")
        updateSelectInput(session, "select_car_type_profile", choices = car_profiles$user_car_profile_name)
      }}
    )
  })
  
  #decomposite the rule function
  observeEvent(input$delete_car_rule, {
    
    daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
    dfsix = as.matrix(daf[28])
    original_cargos <- data.frame(dfsix)
    
    car_profiles <- read.csv2("car_profiles.csv")
    car_profiles <- filter(car_profiles, user_car_profile_name == input$select_car_type_profile)
    car_types <- read.csv2(paste0(car_profiles$system_car_profile_name, ".csv"))
    car_types <- select(car_types, car_type, original_value, Freq)
    
    #recognise selected rule
    #grepl the rule from the original dataset
    
    selected_values <- isolate(input$car14_rows_selected)
    #popkies <- as.matrix(selected_popkas)
    car_types$id <- c(1:nrow(car_types))
    changed_values <- filter(car_types, id == selected_values[1])
    if (length(selected_values) > 1){
      for (i in 2:length(selected_values)){
        changed_values_add <- filter(car_types, id == selected_values[i])
        changed_values <- rbind(changed_values, changed_values_add)
      }
    }
    
    #changed_values$cargo_type <- input$select_cargo_type
    
    for (i in 1:length(selected_values)){
      car_types <- filter(car_types, id != selected_values[i])
    }
    for (i in 1:nrow(changed_values)) {
      original_cargos$nas <- grepl(changed_values$original_value[i], dfsix)
      original_cargos_filters <- filter(original_cargos, nas == TRUE)
      if (nrow(original_cargos_filters) > 0) {
        original_cargos_filters <- data.frame(original_cargos_filters[!duplicated(original_cargos_filters), ])
        original_cargos_filters$car_type <- "необработанные"
        original_cargos_filters$original_value = c(as.matrix(original_cargos_filters[1]))
        original_cargos_filters$Freq <- nrow(original_cargos_filters)
      }
    }
    if (nrow(original_cargos_filters) > 0) {
      original_cargos_filters <- select(original_cargos_filters, car_type, original_value, Freq)
      #rbind the original values to the working prototype
      car_types <- select(car_types, car_type, original_value, Freq)
      car_types <- rbind(car_types, original_cargos_filters)
    }
    #save
    write.csv2(car_types, file = paste0(car_profiles$system_car_profile_name, ".csv"))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
    #input initial data
    observeEvent(input$clicks, {df <- read.csv2(input$file1$datapath)
        write.csv2(df, file = "inputclicks.csv")
        daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
        daf$nas <- c(is.na(daf[13]))
        daf <- filter(daf, nas == FALSE)
        id = dplyr::select(daf, X)
        
        #now that nothing is happening after hitting Go! button i should add some "success" message
        shinyalert(
          title = "Файл загруен",
          text = "Используйте боковую панель для обработки данных",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "success",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 1850,
          imageUrl = "",
          animation = TRUE
        )
        
        #third column (cargo_type)
        output$x14 = DT::renderDataTable(df(), filter = 'top')
        df <- eventReactive({input$create_new_rule
          input$add_elements_to_selected_groups
          input$delete_rule
          input$show_unhandled}, {
            
            cargo_profiles <- read.csv2("cargo_profiles.csv")
            cargo_profiles <- filter(cargo_profiles, user_cargo_profile_name == input$select_cargo_type_profile)
            cargo_types <- read.csv2(paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
            cargo_types <- select(cargo_types, cargo_type, original_value, Freq)
            cargo_types <- filter(cargo_types, cargo_type != "необработанные")
            
            dfsix = as.matrix(daf[12])
            todelete = grepl("Это фиктивная переменная, она не попадет в итоговую выборку", dfsix)
            df = data.frame(daf, todelete)
            moscow = filter(df, todelete == TRUE)
            df = filter(df, todelete == FALSE)
            df = dplyr::select(df, -todelete)
            dfsix = as.matrix(df[12])
            
            cargo_type <- "необработанные"
            todelete = TRUE
            Freq <- nrow(moscow)
            cargo_type <- data.frame(cargo_type, todelete, Freq)
            moscow = left_join(moscow, cargo_type, by = "todelete")
            adresestojoin = moscow
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            progress$set(message = "Обработка типов груза", value = 0)
            
            for (i in 2:nrow(cargo_types)) {
              # Increment the progress bar, and update the detail text.
              progress$inc(1/nrow(cargo_types), detail = percent(i/nrow(cargo_types)))
              todelete = grepl(cargo_types[i, 2], dfsix)
              df = data.frame(df, todelete)
              moscow = filter(df, todelete == TRUE)
              df = filter(df, todelete == FALSE)
              df = dplyr::select(df, -todelete)
              dfsix = as.matrix(df[12])
              cargo_type <- cargo_types[i, 1]
              todelete = TRUE
              cargo_type <- data.frame(cargo_type, todelete, Freq)
              moscow = left_join(moscow, cargo_type, by = "todelete")
              adresestojoin = rbind(adresestojoin, moscow)
            }
            
            original_value <- left_join(id, adresestojoin)
            original_value$nas <- c(is.na(original_value[2]))
            
            withcargotype <- filter(original_value, nas == FALSE)
            positions <- c(1,34)
            withcargotype <- dplyr::select(withcargotype, positions)
            
            original_value <- filter(original_value, nas == TRUE)
            original_value = dplyr::select(original_value, X)
            original_value <- left_join(original_value, daf)
            positions <- c(12)
            original_value <- dplyr::select(original_value, positions)
            original_value <- data.frame(sort(table(original_value), decreasing = TRUE))
            original_value$cargo_type <- "необработанные"
            cargo_types <- rbind(cargo_types, original_value)
            
            
            
            write.csv2(cargo_types, paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
            cargo_types
          })
        
        
        
        
        
        
        
        
        #car_type
        output$car14 = DT::renderDataTable(vehicle(), filter = 'top')
        vehicle <- eventReactive({input$create_new_car_rule
          input$add_elements_to_cared_groups
          input$delete_car_rule
          input$show_uncared}, {
            
            car_profiles <- read.csv2("car_profiles.csv")
            car_profiles <- filter(car_profiles, user_car_profile_name == input$select_car_type_profile)
            car_types <- read.csv2(paste0(car_profiles$system_car_profile_name, ".csv"))
            car_types <- select(car_types, car_type, original_value, Freq)
            car_types <- filter(car_types, car_type != "необработанные")
            
            dfsix = as.matrix(daf[28])
            todelete = grepl("Это фиктивная переменная, она не попадет в итоговую выборку", dfsix)
            df = data.frame(daf, todelete)
            moscow = filter(df, todelete == TRUE)
            df = filter(df, todelete == FALSE)
            df = dplyr::select(df, -todelete)
            dfsix = as.matrix(df[28])
            
            car_type <- "необработанные"
            todelete = TRUE
            Freq <- nrow(moscow)
            car_type <- data.frame(car_type, todelete, Freq)
            moscow = left_join(moscow, car_type, by = "todelete")
            adresestojoin = moscow
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            progress$set(message = "Обработка типов TC", value = 0)
            
            for (i in 2:nrow(car_types)) {
              # Increment the progress bar, and update the detail text.
              progress$inc(1/nrow(car_types), detail = percent(i/nrow(car_types)))
              todelete = grepl(car_types[i, 2], dfsix)
              df = data.frame(df, todelete)
              moscow = filter(df, todelete == TRUE)
              df = filter(df, todelete == FALSE)
              df = dplyr::select(df, -todelete)
              dfsix = as.matrix(df[28])
              car_type <- car_types[i, 1]
              todelete = TRUE
              car_type <- data.frame(car_type, todelete, Freq)
              moscow = left_join(moscow, car_type, by = "todelete")
              adresestojoin = rbind(adresestojoin, moscow)
            }
            
            original_value <- left_join(id, adresestojoin)
            original_value$nas <- c(is.na(original_value[2]))
            
            withcartype <- filter(original_value, nas == FALSE)
            positions <- c(1,34)
            withcartype <- dplyr::select(withcartype, positions)
            original_value <- filter(original_value, nas == TRUE)
            original_value = dplyr::select(original_value, X)
            original_value <- left_join(original_value, daf)
            
            positions <- c(28)
            original_value <- dplyr::select(original_value, positions)
            original_value <- data.frame(sort(table(original_value), decreasing = TRUE))
            original_value$car_type <- "необработанные"
            car_types <- rbind(car_types, original_value)
            
            
            
            write.csv2(car_types, paste0(car_profiles$system_car_profile_name, ".csv"))
            car_types
          })
        
        
        
        
        
        
        
        
        
        
        
        #car_type module
        #observeEvent(input$car_type_starter, {
          #nocartype <- callModule(car_type_preparation, "car_type")
          #withcartype <<- callModule(withcartype_func, "car_type", reactive(nocartype))
          #nocartype <- callModule(nocartype_func, "car_type",  reactive(nocartype))
          #output$nocar <- DT::renderDataTable(nocartype, colnames = c('Номер перевозки', 'Необработанный тип ТС'))
        #})
        
        #addresses module
        observeEvent(input$addresses_starter, {
          nodeparture_one <- callModule(nodeparture_original, "addresses")
          withdeparture_one <- callModule(withdeparture_original, "addresses", reactive(nodeparture_one))
          nodeparture_second <- callModule(nodeparture_success, "addresses", reactive(nodeparture_one))
          withdeparture_and_destination <- callModule(withdeparture, "addresses", reactive(nodeparture_second), reactive(withdeparture_one))
          departure_final <<- callModule(departure, "addresses", reactive(withdeparture_and_destination))
          destination_final <<- callModule(destination, "addresses", reactive(withdeparture_and_destination))
          unparsable <- read.csv2("unparsable.csv")
          unparsable <- select(unparsable, to_maps..duplicated.to_maps....)
          output$nodeparture <- DT::renderDataTable(unparsable, colnames = c('Необработанные значения'))
          #output$thedataframe = DT::renderDataTable(withdeparture, filter = 'top', options = list(
            #scrollX = TRUE
          #))
        })
        
        #agents module
        observeEvent(input$show_all_agents, {
          callModule(agent_filter, "agent_filter")
          all_agents_foo <- eventReactive({input$add_agent_filter
            input$delete_agent_filter
            input$show_all_agents}, {
              all_agents_in_foo <- read.csv2("allagents.csv")
              all_agents_in_foo <- select(all_agents_in_foo, Var1, Freq)
            })
          output$all_agents <- DT::renderDataTable(all_agents_foo(), filter = 'top', colnames = c('Наименование контрагента', 'Количество перевозок'))
        })
        
        #final table maker
        observeEvent(input$form_the_table, {
          
          # Create a Progress object
          progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          
          #set the progress bar
          progress$set(message = "Формирование итоговой таблицы", value = 0)
          
          trip_duration <- callModule(duration, "duration")
          two_loadings <- callModule(two_loadings_detector, "two_loadings")
          weight <- callModule(weight, "weight")
          
          #add price
          id$price <- pull(daf[11])
          id$price <- as.character(id$price)
          id$price <- gsub("(.*),.*", "\\1", id$price)
          id$price <- gsub("[[:space:]]", "", id$price)
          id$price <- as.numeric(id$price)
          
          #add money_transfer_form
          id$money_transfer_form <- pull(daf[30])
          id$money_transfer_form <- as.character(id$money_transfer_form)
          
          #add payment
          id$payment <- pull(daf[31])
          id$payment <- as.character(id$payment)
          
          #add volume
          id$volume <- pull(daf[26])
          id$volume <- as.character(id$volume)
          id$volume <- gsub("[[:space:]]", "", id$volume)
          id$volume <- as.numeric(id$volume)
          
          #add cargo_price
          id$cargo_price <- pull(daf[19])
          id$cargo_price <- as.character(id$cargo_price)
          id$cargo_price <- gsub("[[:space:]]", "", id$cargo_price)
          id$cargo_price <- as.numeric(id$cargo_price)
          #withcartype$X <- as.character(withcartype$X)
          
          #add car_type
          car_profiles <- read.csv2("car_profiles.csv")
          car_profiles <- filter(car_profiles, user_car_profile_name == input$select_car_type_profile)
          car_types <- read.csv2(paste0(car_profiles$system_car_profile_name, ".csv"))
          car_types <- select(car_types, car_type, original_value, Freq)
          car_types <- filter(car_types, car_type != "необработанные")
          positions <- c(1,28)
          dfsix <- select(daf, positions)
          dfsix$todelete = grepl(c(as.matrix(car_types$original_value[1])), c(as.matrix(dfsix[2])))
          dfsix$car_type <- car_types$car_type[1]
          moscow = select(dfsix, X, car_type)
          dfsix <- select(dfsix, -c(todelete, car_type))
          moscowbasis <- head(moscow, n=0)
          
          # Create a Progress object
          progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          
          progress$set(message = "Подключение модулей", value = 0)
          for (i in 1:nrow(car_types)) {
            
            # Increment the progress bar, and update the detail text.
            progress$inc(1/nrow(car_types), detail = percent(i/nrow(car_types)))
            dfsix$todelete = grepl(c(as.matrix(car_types$original_value[i])), c(as.matrix(dfsix[2])))
            dfsix$car_type <- car_types$car_type[i]
            moscow = filter(dfsix, todelete == TRUE)
            moscow <- select(moscow, X, car_type)
            dfsix = filter(dfsix, todelete == FALSE)
            dfsix = dplyr::select(dfsix, -c(todelete, car_type))
            moscowbasis <- rbind(moscowbasis, moscow)
          }
          
          moscowbasis$X <- as.character(moscowbasis$X)
          id$X <- as.character(id$X)
          final_table <- inner_join(id, moscowbasis, by = "X")
          
          #id, car_type
          #final_table <- inner_join(id, withcartype)
          final_table$X <- as.character(final_table$X)
          
          trip_duration$X <- as.character(trip_duration$X)
          
          #id, car_type, duration
          final_table <- inner_join(final_table, trip_duration)
          two_loadings$X <- as.character(two_loadings$X)
          
          #id, car_type, duration, two_loadings
          final_table <- inner_join(final_table, two_loadings)
          weight$X <- as.character(weight$X)
          
          #id, car_type, duration, two_loadings, weight
          final_table <- inner_join(final_table, weight)
          
          # Create a Progress object
          progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          
          #delete strings that cannot be defined through Yandex parser
          progress$set(message = "Загрузка исторического курса доллара и цены на топливо", value = 0)
          
          #deploy the parser
          py_run_file("~/Downloads/usd_diesel.py")
          jnastr240 = readLines("~/Downloads/usd_dollar.json") %>% 
            str_c(collapse = ",") %>%  
            (function(str) str_c("[", str, "]")) %>% 
            fromJSON(simplifyDataFrame = T)
          jnastr240<-jnastr240[[1]]
          write.csv(jnastr240, "~/Downloads/usd_diesel.csv")
          jnastr240$calendar <- as.Date(jnastr240$calendar)
          final_table <- left_join(final_table, jnastr240, by = "calendar")
          
          #add cargo_type
          cargo_profiles <- read.csv2("cargo_profiles.csv")
          cargo_profiles <- filter(cargo_profiles, user_cargo_profile_name == input$select_cargo_type_profile)
          cargo_types <- read.csv2(paste0(cargo_profiles$system_cargo_profile_name, ".csv"))
          cargo_types <- select(cargo_types, cargo_type, original_value, Freq)
          cargo_types <- filter(cargo_types, cargo_type != "необработанные")
          #dfsix = as.matrix(daf[12])
          positions <- c(1,12)
          dfsix <- select(daf, positions)
          dfsix$todelete = grepl(c(as.matrix(cargo_types$original_value[1])), c(as.matrix(dfsix[2])))
          dfsix$cargo_type <- cargo_types$cargo_type[1]
          moscow = select(dfsix, X, cargo_type)
          dfsix <- select(dfsix, -c(todelete, cargo_type))
          moscowbasis <- head(moscow, n=0)
          
          # Create a Progress object
          progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          
          progress$set(message = "Создание итоговой таблицы", value = 0)
          for (i in 1:nrow(cargo_types)) {
            
            # Increment the progress bar, and update the detail text.
            progress$inc(1/nrow(cargo_types), detail = percent(i/nrow(cargo_types)))
            dfsix$todelete = grepl(c(as.matrix(cargo_types$original_value[i])), c(as.matrix(dfsix[2])))
            dfsix$cargo_type <- cargo_types$cargo_type[i]
            moscow = filter(dfsix, todelete == TRUE)
            moscow <- select(moscow, X, cargo_type)
            dfsix = filter(dfsix, todelete == FALSE)
            dfsix = dplyr::select(dfsix, -c(todelete, cargo_type))
            moscowbasis <- rbind(moscowbasis, moscow)
          }
          
          moscowbasis$X <- as.character(moscowbasis$X)
          
          final_table <- inner_join(final_table, moscowbasis, by = "X")
          
          agent_filters <- read.csv2("agent_filters.csv")
          agent_filters <- select(agent_filters, agent_number, agent_filter)
          dfsix <- as.matrix(agent_filters)
          allagents <- data.frame(sort(table(daf[5]), decreasing = TRUE))
          for (i in 1:nrow(agent_filters)) {
            daf = filter(daf, daf[2] != dfsix[i, 2])
          }
          
          positions <- c(1)
          withfilters <- dplyr::select(daf, positions)
          withfilters$X <- as.character(withfilters$X)
          departure_final <- select(departure_final, -c(nas, todelete, addresses, street))
          #id, car_type, duration, two_loadings, weight
          final_table <- inner_join(final_table, withfilters)
          departure_final$X <- as.character(departure_final$X)
          
          #id, car_type, duration, two_loadings, weight, region, city, latidude, longitude
          final_table <- inner_join(final_table, departure_final)
          destination_final$X <- destination_final$X/0.000001
          destination_final$X <- as.character(destination_final$X)
          destination_final <- select(destination_final, -c(nas, todelete, addresses, street))
          #id, car_type, duration, two_loadings, weight, region, city, latidude, longitude, region.y, city.y, 
          #latidude.y, longitude.y
          final_table <- inner_join(final_table, destination_final, by = "X")
          
          distances <- callModule(distances, "distances", reactive(destination_final), reactive(departure_final))
          distances$X <- as.character(distances$X)
          
          #id, car_type, duration, two_loadings, weight, region, city, latidude, longitude, region.y, city.y, 
          #latidude.y, longitude.y, distances
          final_table <- inner_join(final_table, distances)

          final_table <- separate(final_table, latitude.x, into = c("lat_from", "titude"), sep = "\\.", remove = TRUE)
          final_table$lat_from <- as.integer(final_table$lat_from)
          final_table$titude <- substr(final_table$titude, 1, 2)
          final_table$titude <- as.integer(final_table$titude)
          final_table$titude <- final_table$titude*0.01
          final_table$lat_from <- final_table$lat_from+final_table$titude
          final_table <- select(final_table, -c(titude))

          final_table <- separate(final_table, latitude.y, into = c("lat_to", "titude"), sep = "\\.", remove = TRUE)
          final_table$lat_to <- as.integer(final_table$lat_to)
          final_table$titude <- substr(final_table$titude, 1, 2)
          final_table$titude <- as.integer(final_table$titude)
          final_table$titude <- final_table$titude*0.01
          final_table$lat_to <- final_table$lat_to+final_table$titude
          final_table <- select(final_table, -c(titude))

          final_table <- separate(final_table, longitude.x, into = c("long_from", "titude"), sep = "\\.", remove = TRUE)
          final_table$long_from <- as.integer(final_table$long_from)
          final_table$titude <- substr(final_table$titude, 1, 2)
          final_table$titude <- as.integer(final_table$titude)
          final_table$titude <- final_table$titude*0.01
          final_table$long_from <- final_table$long_from+final_table$titude
          final_table <- select(final_table, -c(titude))

          final_table <- separate(final_table, longitude.y, into = c("long_to", "titude"), sep = "\\.", remove = TRUE)
          final_table$long_to <- as.integer(final_table$long_to)
          final_table$titude <- substr(final_table$titude, 1, 2)
          final_table$titude <- as.integer(final_table$titude)
          final_table$titude <- final_table$titude*0.01
          final_table$long_to <- final_table$long_to+final_table$titude
          final_table <- select(final_table, -c(titude, city.x, city.y, X))
          
          final_table$calendar <- as.double(final_table$calendar)
          final_table$diesel_price <- as.double(final_table$diesel_price)
          final_table$dollar <- as.double(final_table$dollar)
          
          output$thedataframe <- DT::renderDataTable(final_table)
          
          #total function
          observeEvent(input$save_dt, {complete_dts <- read.csv2("complete_dts.csv")
            complete_dts <- dplyr::select(complete_dts, user_name, system_name, cargo_types_ds)
            user_name <- input$dt_name
            system_name <- paste0(as.numeric(tail(complete_dts$system_name, n=1))+1, ".csv")
            
            cargo_profiles <- read.csv2("cargo_profiles.csv")
            cargo_profiles <- filter(cargo_profiles, user_cargo_profile_name == input$select_cargo_type_profile)
            
            cargo_types_ds <- paste0(cargo_profiles$system_cargo_profile_name, ".csv")
            write.csv2(final_table, file = system_name)
            addition <- data.frame(user_name, system_name, cargo_types_ds)
            complete_dts <- rbind(complete_dts, addition)
            write.csv2(complete_dts, file = "complete_dts.csv")
          })
        })#final table maker observer end
        
        
        
        
        
        
        
      
    })#end of the upload data observer
}

shinyApp(ui, server)
