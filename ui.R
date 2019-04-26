library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(dplyr)
citiesnew <- read.csv2("~/Downloads/citiesnew.csv")
stpetersburgstreets <- read.csv2("~/Downloads/stpetersburgstreets.csv")
moscowstreets <- read.csv2("~/Downloads/moscowstreets.csv")
car_type <- read.csv2("~/Downloads/car_type.csv")
models <- read.csv2("~/Downloads/models.csv")
the_model_to_use <- read.csv2("~/Downloads/cargo_type_module/the_model_to_use.csv")
models <- filter(models, user_model_name == as.character(the_model_to_use$the_model_to_use))
cargo_types <- read.csv2(file = paste0("~/Downloads/", as.character(models$cargo_types_ds)))
cargo_types <- filter(cargo_types, cargo_type != "необработанные")
cargo_types <- cargo_types[!duplicated(cargo_types$cargo_type), ]
car_types <- read.csv2(file = paste0("~/Downloads/", as.character(models$car_types_ds)))
car_types <- car_types[!duplicated(car_types$car_type), ]
car_types <- filter(car_types, car_type != "необработанные")
fluidPage(
  div(class="hidden",
      regions <- read.csv2("~/Downloads/regions.csv"),
      regions <- select(regions, region)
  ),
  column(4,
         selectInput(inputId = "load_city", h3("Загрузка"), 
                     choices = list("Санкт-Петербург", "Москва", остальные=citiesnew$city)),
         selectInput(inputId = "stp_streets", label = "Улица",
                     choices = stpetersburgstreets$street, selected = "Цветочная"),
         selectInput(inputId = "moscow_streets", label = "Улица",
                     choices = moscowstreets$street, selected = "Факультетский"),
         tableOutput('load_city_dt'),
         sliderInput("volume", "Объём",
                     min = 1, max = 120, value = 82),
         shinyjs::hidden(div(id = "calendar", dateInput(inputId = "calendar", 
                                                        h3("Дата загрузки"), 
                                                        value = Sys.Date()))),
         shinyjs::hidden(div(id = "two_loadings.x", radioButtons(inputId = "two_loadings", h3("Более одной точек погрузки"),
                                                                 choices = list("Нет" = FALSE, "Да" = TRUE),selected = FALSE))),
         shinyjs::hidden(div(id = "cargo_price", numericInput(inputId = "cargo_price", 
                                                              h3("Стоимость груза"), 
                                                              value = 0))),
         shinyjs::hidden(div(id = "money_transfer_form", selectInput(inputId = "money_transfer_form", h3("Форма оплаты"), 
                                                                     choices = list("по ОТТН и документам на оплату", "по сканам ТТН и квитку", 
                                                                                    "в обмен на ТТН", "перед погрузкой", "по скану счета",
                                                                                    "по счету", "по факту выгрузки", "по факту загрузки")))),
         shinyjs::hidden(div(id = "cargo_type", selectInput(inputId = "cargo_type", h3("Тип груза"), 
                                                            choices = cargo_types$cargo_type)))
  ),
  column(4, 
         selectInput(inputId = "unload_city", h3("Разгрузка"), 
                     choices = list("Москва", "Санкт-Петербург", остальные=citiesnew$city)),
         selectInput(inputId = "stp_streets_unload", label = "Улица",
                     choices = stpetersburgstreets$street, selected = "Цветочная"),
         selectInput(inputId = "moscow_streets_unload", label = "Улица",
                     choices = moscowstreets$street, selected = "Факультетский"),
         tableOutput('load_city_dt_unload'),
         sliderInput("weight", "Вес",
                     min = 0.1, max = 24, value = 20, step = 0.1),
         shinyjs::hidden(div(id = "duration", numericInput(inputId = "duration", 
                                                           h3("Дней в пути"), 
                                                           value = 0))),
         shinyjs::hidden(div(id = "two_loadings.y", radioButtons(inputId = "two_unloadings", h3("Более одной точек выгрузки"),
                                                                 choices = list("Нет" = FALSE, "Да" = TRUE),selected = FALSE))),
         shinyjs::hidden(div(id = "car_type", selectInput(inputId = "car_type", h3("Тип авто"), 
                                                          choices = car_types$car_type))),
         shinyjs::hidden(div(id = "payment", selectInput(inputId = "payment", h3("Тип оплаты"), 
                                                         choices = list("безнал с НДС", "безнал без НДС", "нал на выгрузке",
                                                                        "нал на загрузке", "наличные", "перевод на карту")))),
         actionButton(inputId = "count", label = "Рассчитать", icon("calculator"), 
                      style="color: #fff; background-color: #EB70E5; border-color: #2e6da4"),
         tableOutput(outputId = "price")
  ),
  column(4,
         useShinyjs(),
         br(), br(), br(), actionButton("add_new_location", "Меню добавления адресов", icon("plus"), 
                                        style="color: #4A21EB; background-color: #C7FAFF; border-color: #2e6da4"),
         br(), br(),
         shinyjs::hidden(div(id = "advanced3",
                             selectizeInput("regions", "Выберите субъект федерации",
                                            choices = (regions$region),
                                            options = list(create = TRUE)),
                             textInput("newcity", "Введите наименование населенного пункта для записи в базе данных"),
                             textInput("newstreet", "Введите наименование улицы для записи в базе данных"),
                             numericInput("city_latitude", "Введите широту координат населенного пункта", value = 59.57, min = 40, max = 70, step = 0.01),
                             numericInput("city_longitude", "Введите долготу координат населенного пункта", value = 30.19, min = 19, max = 161, step = 0.01),
                             br(),
                             actionButton("add_new", "Добавить", icon("plus"), style="color: #fff; background-color: #1EF003; border-color: #2e6da4"))))
)