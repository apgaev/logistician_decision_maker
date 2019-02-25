library(shiny)
library(dplyr)
library(shinydashboard)
library(shinyjs)
library(shinyalert)

complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
cargo_profiles <- read.csv2("~/Downloads/groups_profiles.csv")

dashboardPage(
  dashboardHeader(title = "Модельер"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Математик", tabName = "data_load", icon = icon("download")),
      menuItem("Группировщик", tabName = "groupies", icon = icon("folder-open")),
      menuItem("Садовод", tabName = "treemaker", icon = icon("folder-open")),
      menuItem("Фильтратор", tabName = "filterer", icon = icon("folder-open")),
      menuItem("И вот модельер", tabName = "the_modelier", icon = icon("folder-open"))
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
                  br(),
                  selectInput("initial_select", h3("Выберите одну из таблиц антикафе"), choices = complete_dts$user_name),
                  textInput("plus_name", "Наименование нового столбца латиницей без пробелов"),
                  actionButton("make_select_work", "make_select_work"),
                  actionButton("plusplus", "+"),
                  actionButton("minusminus", "-"),
                  actionButton("multiplymultiply", "*"),
                  actionButton("dividedivide", "/"),
                  actionButton("loglog", "натуральный логарифм"),
                  actionButton("expexp", "экспонента")
                ),
                column(2)
              ),
              fluidRow(
                DT::dataTableOutput('initial_table')
              ),
              fluidRow(
                DT::dataTableOutput('added_table')
              )
      ),
      tabItem(tabName = "groupies",
                fluidPage(
                  fluidRow(
                    column(1),
                    column(5, selectInput("select_column", h5("Выберите столбец таблицы"), choices = complete_dts$user_name)),
                    column(5, br(), br(), actionButton("show_unhandled", "Выбрать столбец", icon("check-circle"), 
                                           style="color: #fff; background-color: #00FFD9; border-color: #2e6da4")
                    ),
                    column(1)
                  ),
                  fluidRow(
                    column(1),
                    column(
                      10, useShinyjs(),
                      DT::dataTableOutput('x14'),
                      #DT::dataTableOutput('z14'),
                      #DT::dataTableOutput('y14'),
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
                                           choices = NULL))
                           ),
                    column(5, br(), br(),
                           actionButton("add_elements_to_selected_groups", "Добавить выделенные элементы в выбранную группу", icon("plus"), 
                                        style="color: #fff; background-color: #1EF003; border-color: #2e6da4")),
                    column(1)
                  ),
                  
                  fluidRow(
                    column(2),
                    column(4,
                           actionButton("delete_group", "Удалить выделенную группу", icon("times"), 
                                        style="color: #fff; background-color: #F20303; border-color: #2e6da4")),
                    column(5,
                           actionButton("create_new_group", "Создать новую группу", icon("plus"), 
                                        style="color: #fff; background-color: #1EF003; border-color: #2e6da4")),
                    column(1)
                  ),
                  br(),
                  fluidRow(
                    column(6,
                           selectInput("select_cargo_type_profile", "Выберите профиль типов груза",
                                       choices = (cargo_profiles$user_groups_profile_name))
                           ),
                    column(6,
                           sliderInput("former_rule", "Выберите числовой диапазон:",
                                       min = 0, max = 1000, value = c(200,500)
                           ))
                  ),
                  
                  fluidRow(
                    column(6,actionButton("use_profile", "Применить профиль", icon("plus"), 
                                          style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                           ),
                    column(6,
                           actionButton("int_range_setup", "Добавить значения из числового диапазона в группу", icon("plus"), 
                                        style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                           )
                    ),
                  
                  fluidRow(
                    column(6,
                           useShinyalert(),
                           actionButton("delete_cargo_profile", "Удалить выбранный профиль", icon("times"), 
                                        style="color: #fff; background-color: #F20303; border-color: #2e6da4")
                    ),
                    column(6,
                           actionButton("save_new_column", "Сохранить новый столбец", icon("check"), 
                                        style="color: #fff; background-color: #EB70E5; border-color: #2e6da4")
                    )
                  ),
                  
                  
                  fluidRow(
                    column(
                      6, br(),
                      actionButton("save_cargo_profile", "Создать профиль", icon("plus"), 
                                   style="color: #fff; background-color: #1EF003; border-color: #2e6da4")
                    ),
                    column(6
                    )
                  )
              )
    ),
    tabItem(tabName = "treemaker",
            fluidRow(
              column(1),
              column(10, 
                     checkboxGroupInput("checkGroup", 
                                        h3("Выбор столбцов для обчучения модели"), 
                                        choices = list("Choice 1" = 1, 
                                                       "Choice 2" = 2, 
                                                       "Choice 3" = 3,
                                                       "Choice 4" = 4, 
                                                       "Choice 5" = 5, 
                                                       "Choice 6" = 6),
                                        selected = c(1, 2, 3), inline = TRUE)),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10, actionButton("many_plots_generator", "Обновить числовые графики")),
              column(1)
            ),
            uiOutput("plots"),
            fluidRow(
              column(1),
              column(10, actionButton("many_groups_generator", "Обновить графики групп")),
              column(1)
            ),
            uiOutput("groupies")
    ),
    tabItem(tabName = "filterer",
            fluidRow(
              column(3),
              column(8, selectInput("select_column_filter", h5("Выберите столбец таблицы"), choices = complete_dts$user_name)),
              column(1)
            ),
            fluidRow(
              column(3),
              column(8, actionButton("make_select_column_filter", "Выбрать столбец")),
              column(1)
            ),
            fluidRow(
              column(1),
              column(5, sliderInput("numeric_filter", "Оставить значения в диапазоне:",
                                     min = 0, max = 1000, value = c(200,500))),
              column(5, selectInput("select_filtered_value", h5("Не учитывать:"), choices = complete_dts$user_name)),
              column(1)
            ),
            fluidRow(
              column(1),
              column(5, actionButton("apply_numeric_filter", "Создать фильтр")),
              column(5, actionButton("apply_factor_filter", "Создать фильтр")),
              column(1)
            ),
            fluidRow(
              column(1),
              column(5, DT::dataTableOutput('lower_border'), DT::dataTableOutput('upper_border')),
              column(5, DT::dataTableOutput('factor_filter'), actionButton("apply_filters", "Применить выбранные фильтры")),
              column(1)
            )
    ),
    tabItem(tabName = "the_modelier",
            fluidRow(
              column(1),
              column(3, actionButton("unsupervised", "Добавить обучение без учителя")
              ),
              column(3, sliderInput("clusters_number", "Количество кластеров",
                                    min = 2, max = 53, value = 28)),
              column(3, selectInput("cluster_method", label = "Метод кластеризации", choices = c("complete", "average", "single", "centroid"))
              ),
              column(2)
            ),
            fluidRow(
              column(1),
              column(10, actionButton("make_the_tree", "Вырастить дерево!")
                     ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10, verbatimTextOutput("accuracy")
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10
                     #select the model to use in prediction
              ),
              column(1)
            )
    )
    
)
)
)