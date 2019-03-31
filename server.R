library(shiny)
library(dplyr)
library(shinydashboard)
library(shinyalert)
library(randomForest)

function(input, output, session) {
  #state reactive values
  hclusters <- NULL
  makeReactiveBinding("hclusters")
  
  #initiate dts rendered by fuctions
  output$x14 = DT::renderDataTable(df(), filter = 'top')
  output$lower_border = DT::renderDataTable(low_border(), filter = 'top', colnames = c('Значение', 'Больше чем'))
  output$upper_border = DT::renderDataTable(up_border(), filter = 'top', colnames = c('Значение', 'Меньше чем'))
  output$factor_filter = DT::renderDataTable(fac_filter(), filter = 'top', colnames = c('Значение', 'Не равно'))
  
  #delete irrelevant datasets
  observeEvent(input$delete_dts, {
    
    shinyalert(
      title = "Вы точно хотите удалить эту таблицу?",
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
      callbackR = function(x) { if(x == TRUE) {#load dts
        complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
        complete_dts <- select(complete_dts, -c(X))
        complete_dts_filter <- filter(complete_dts, user_name != input$initial_select)
        
        #update selector
        updateSelectInput(session, "initial_select", choices = complete_dts_filter$user_name)
        
        write.csv2(complete_dts_filter, "~/Downloads/complete_dts.csv")
      }}
    )
  })
  
  observeEvent(input$initial_select, {complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    #print(selected_path)
    selected_dt <- read.csv2(selected_path)
    selected_dt <- select(selected_dt, -c(X))
    
    models <- read.csv2("~/Downloads/models.csv")
    temporary_name <- paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv")
    
    #write temporary file with all prices
    write.csv2(selected_dt, temporary_name)
    
    #update selectors without prices not to allow user create other columns based on it
    updateSelectInput(session, "select_column_filter", choices = colnames(selected_dt))
    
    #forbid users to uncheck obligatory columns
    selected_dt <- select(selected_dt, -c(price, price_in_dollar, distance, volume, weight, region.x, region.y, lat_to, lat_from, long_to, long_from))
    updateCheckboxGroupInput(session, "checkGroup", label = "Выбор столбцов для обчучения модели", choices = colnames(selected_dt),
                             selected = colnames(selected_dt), inline = TRUE)
    
    complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    #print(selected_path)
    selected_dt <- read.csv2(selected_path)
    selected_dt <- select(selected_dt, -c(X))
    
    #show dataset without prices not to allow user create other columns based on it
    selected_dt <- select(selected_dt, -c(price, price_in_dollar))
    updateSelectInput(session, "select_column", choices = colnames(selected_dt))
    
    #show this dataset to users
    output$initial_table <- DT::renderDataTable(selected_dt, selection = list(target = 'column'), options = list(
      scrollX = TRUE
    ))
  })
  
  #it should activate select in groupies, but i need to check whether it works without this function
  observeEvent(input$make_select_work, {
    updateSelectInput(session, "select_column", choices = colnames(selected_dt))
  })
  
  #updates buttons content
  observeEvent(input$initial_table_columns_selected, {
    
    #isolate selected columns POSITIONS
    selected_popkas <- isolate(input$initial_table_columns_selected)
    
    complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    selected_dt <- read.csv2(selected_path)
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(X, price, price_in_dollar))
    
    positions <- c(selected_popkas)
    withfilters <- dplyr::select(selected_dt, positions)
    
    updateActionButton(session, "plusplus",
                       label = paste0(colnames(withfilters[1:ncol(withfilters)]), collapse = "+"))

    updateActionButton(session, "minusminus",
                       label = paste0(colnames(withfilters[1:ncol(withfilters)]), collapse = "-"))
    
    updateActionButton(session, "multiplymultiply",
                       label = paste0(colnames(withfilters[1:ncol(withfilters)]), collapse = "*"))
    
    updateActionButton(session, "dividedivide",
                       label = paste0(colnames(withfilters[1:ncol(withfilters)]), collapse = "/"))
    
    updateActionButton(session, "loglog",
                       label = paste0("натуральный логарифм ", colnames(withfilters[1])))
    
    updateActionButton(session, "expexp",
                       label = paste0("экспонента ", colnames(withfilters[1])))
    
    updateActionButton(session, "expexp",
                       label = paste0("степень ", colnames(withfilters[1])))
  })
  
  observeEvent(input$plusplus, {
    
    selected_popkas <- isolate(input$initial_table_columns_selected)
    
    complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    selected_dt <- read.csv2(selected_path)
    
    #save price and price_in_dollar into another dataset to add it later
    prices <- select(selected_dt, price, price_in_dollar)
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(X, price, price_in_dollar))
    
    positions <- c(selected_popkas)
    withfilters <- dplyr::select(selected_dt, positions)
    parent_column_names <- paste(colnames(withfilters), collapse = ", ")

    source("plus_module.R")
    selected_dt <- callModule(a, "plus", reactive(selected_dt), reactive(parent_column_names), reactive(input$plus_name))
    
    #dts should be written with price, price_in_dollar and new column
    #should add price and price_in_dollar before writing dts
    selected_dt <- cbind(prices, selected_dt)
    
    write.csv2(selected_dt, selected_path)
    
    models <- read.csv2("~/Downloads/models.csv")
    temporary_name <- paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv")
    
    #write temporary file with all prices
    write.csv2(selected_dt, temporary_name)
    
    columns_table <- read.csv2("columns_table.csv")
    columns_table <- select(columns_table, -c(X))
    columns_table <- filter(columns_table, column_name != input$plus_name)
    column_name <- input$plus_name
    profile_module_used <- "plus_module.R"
    addition <- data.frame(column_name, profile_module_used, parent_column_names)
    columns_table <- rbind(columns_table, addition)
    write.csv2(columns_table, "columns_table.csv")
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(price, price_in_dollar))
    
    output$initial_table <- DT::renderDataTable(selected_dt, selection = list(target = 'column'), options = list(
      scrollX = TRUE
    ))
    #forbid users to uncheck obligatory columns
    selected_dt <- select(selected_dt, -c(distance, volume, weight, region.x, region.y, lat_to, lat_from, long_to, long_from))
    updateCheckboxGroupInput(session, "checkGroup", label = "Выбор столбцов для обчучения модели", choices = colnames(selected_dt),
                             selected = colnames(selected_dt), inline = TRUE)
  })
  
  
  observeEvent(input$minusminus, {
    
    selected_popkas <- isolate(input$initial_table_columns_selected)
    
    complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    selected_dt <- read.csv2(selected_path)
    
    #save price and price_in_dollar into another dataset to add it later
    prices <- select(selected_dt, price, price_in_dollar)
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(X, price, price_in_dollar))
    
    positions <- c(selected_popkas)
    withfilters <- dplyr::select(selected_dt, positions)
    parent_column_names <- paste(colnames(withfilters), collapse = ", ")
    
    source("minus_module.R")
    selected_dt <- callModule(a, "minus", reactive(selected_dt), reactive(parent_column_names), reactive(input$plus_name))
    
    #dts should be written with price, price_in_dollar and new column
    #should add price and price_in_dollar before writing dts
    selected_dt <- cbind(prices, selected_dt)
    
    write.csv2(selected_dt, selected_path)
    
    models <- read.csv2("~/Downloads/models.csv")
    temporary_name <- paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv")
    
    #write temporary file with all prices
    write.csv2(selected_dt, temporary_name)
    
    columns_table <- read.csv2("columns_table.csv")
    columns_table <- select(columns_table, -c(X))
    columns_table <- filter(columns_table, column_name != input$plus_name)
    column_name <- input$plus_name
    profile_module_used <- "minus_module.R"
    addition <- data.frame(column_name, profile_module_used, parent_column_names)
    columns_table <- rbind(columns_table, addition)
    write.csv2(columns_table, "columns_table.csv")
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(price, price_in_dollar))
    output$initial_table <- DT::renderDataTable(selected_dt, selection = list(target = 'column'), options = list(
      scrollX = TRUE
    ))
    #forbid users to uncheck obligatory columns
    selected_dt <- select(selected_dt, -c(distance, volume, weight, region.x, region.y, lat_to, lat_from, long_to, long_from))
    updateCheckboxGroupInput(session, "checkGroup", label = "Выбор столбцов для обчучения модели", choices = colnames(selected_dt),
                             selected = colnames(selected_dt), inline = TRUE)
  })
  
  observeEvent(input$multiplymultiply, {
    
    selected_popkas <- isolate(input$initial_table_columns_selected)
    
    complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    selected_dt <- read.csv2(selected_path)
    
    #save price and price_in_dollar into another dataset to add it later
    prices <- select(selected_dt, price, price_in_dollar)
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(X, price, price_in_dollar))
    
    positions <- c(selected_popkas)
    withfilters <- dplyr::select(selected_dt, positions)
    parent_column_names <- paste(colnames(withfilters), collapse = ", ")
    
    source("multiply_module.R")
    selected_dt <- callModule(a, "multiply", reactive(selected_dt), reactive(parent_column_names), reactive(input$plus_name))

    #dts should be written with price, price_in_dollar and new column
    #should add price and price_in_dollar before writing dts
    selected_dt <- cbind(prices, selected_dt)
    
    write.csv2(selected_dt, selected_path)
    
    models <- read.csv2("~/Downloads/models.csv")
    temporary_name <- paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv")
    
    #write temporary file with all prices
    write.csv2(selected_dt, temporary_name)
    
    columns_table <- read.csv2("columns_table.csv")
    columns_table <- select(columns_table, -c(X))
    columns_table <- filter(columns_table, column_name != input$plus_name)
    column_name <- input$plus_name
    profile_module_used <- "multiply_module.R"
    addition <- data.frame(column_name, profile_module_used, parent_column_names)
    columns_table <- rbind(columns_table, addition)
    write.csv2(columns_table, "columns_table.csv")
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(price, price_in_dollar))
    
    output$initial_table <- DT::renderDataTable(selected_dt, selection = list(target = 'column'), options = list(
      scrollX = TRUE
    ))
    #forbid users to uncheck obligatory columns
    selected_dt <- select(selected_dt, -c(distance, volume, weight, region.x, region.y, lat_to, lat_from, long_to, long_from))
    updateCheckboxGroupInput(session, "checkGroup", label = "Выбор столбцов для обчучения модели", choices = colnames(selected_dt),
                             selected = colnames(selected_dt), inline = TRUE)
  })
  
  observeEvent(input$dividedivide, {
    
    selected_popkas <- isolate(input$initial_table_columns_selected)
    
    complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    selected_dt <- read.csv2(selected_path)
    
    #save price and price_in_dollar into another dataset to add it later
    prices <- select(selected_dt, price, price_in_dollar)
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(X, price, price_in_dollar))
    
    positions <- c(selected_popkas)
    withfilters <- dplyr::select(selected_dt, positions)
    parent_column_names <- paste(colnames(withfilters), collapse = ", ")
    
    source("divide_module.R")
    selected_dt <- callModule(a, "divide", reactive(selected_dt), reactive(parent_column_names), reactive(input$plus_name))

    #dts should be written with price, price_in_dollar and new column
    #should add price and price_in_dollar before writing dts
    selected_dt <- cbind(prices, selected_dt)
    
    write.csv2(selected_dt, selected_path)
    
    models <- read.csv2("~/Downloads/models.csv")
    temporary_name <- paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv")
    
    #write temporary file with all prices
    write.csv2(selected_dt, temporary_name)
    
    columns_table <- read.csv2("columns_table.csv")
    columns_table <- select(columns_table, -c(X))
    columns_table <- filter(columns_table, column_name != input$plus_name)
    column_name <- input$plus_name
    profile_module_used <- "divide_module.R"
    addition <- data.frame(column_name, profile_module_used, parent_column_names)
    columns_table <- rbind(columns_table, addition)
    write.csv2(columns_table, "columns_table.csv")
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(price, price_in_dollar))
    
    output$initial_table <- DT::renderDataTable(selected_dt, selection = list(target = 'column'), options = list(
      scrollX = TRUE
    ))
    #forbid users to uncheck obligatory columns
    selected_dt <- select(selected_dt, -c(distance, volume, weight, region.x, region.y, lat_to, lat_from, long_to, long_from))
    updateCheckboxGroupInput(session, "checkGroup", label = "Выбор столбцов для обчучения модели", choices = colnames(selected_dt),
                             selected = colnames(selected_dt), inline = TRUE)
  })
  
  observeEvent(input$loglog, {
    
    selected_popkas <- isolate(input$initial_table_columns_selected)
    
    complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    selected_dt <- read.csv2(selected_path)
    
    #save price and price_in_dollar into another dataset to add it later
    prices <- select(selected_dt, price, price_in_dollar)
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(X, price, price_in_dollar))
    
    positions <- c(selected_popkas)
    withfilters <- dplyr::select(selected_dt, positions)
    parent_column_names <- paste(colnames(withfilters), collapse = ", ")
    
    source("log_module.R")
    selected_dt <- callModule(a, "log", reactive(selected_dt), reactive(parent_column_names), reactive(input$plus_name))

    #dts should be written with price, price_in_dollar and new column
    #should add price and price_in_dollar before writing dts
    selected_dt <- cbind(prices, selected_dt)
    
    write.csv2(selected_dt, selected_path)
    
    models <- read.csv2("~/Downloads/models.csv")
    temporary_name <- paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv")
    
    #write temporary file with all prices
    write.csv2(selected_dt, temporary_name)
    
    columns_table <- read.csv2("columns_table.csv")
    columns_table <- select(columns_table, -c(X))
    columns_table <- filter(columns_table, column_name != input$plus_name)
    column_name <- input$plus_name
    profile_module_used <- "log_module.R"
    addition <- data.frame(column_name, profile_module_used, parent_column_names)
    columns_table <- rbind(columns_table, addition)
    write.csv2(columns_table, "columns_table.csv")
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(price, price_in_dollar))
    
    output$initial_table <- DT::renderDataTable(selected_dt, selection = list(target = 'column'), options = list(
      scrollX = TRUE
    ))
    #forbid users to uncheck obligatory columns
    selected_dt <- select(selected_dt, -c(distance, volume, weight, region.x, region.y, lat_to, lat_from, long_to, long_from))
    updateCheckboxGroupInput(session, "checkGroup", label = "Выбор столбцов для обчучения модели", choices = colnames(selected_dt),
                             selected = colnames(selected_dt), inline = TRUE)
  })
  
  observeEvent(input$expexp, {
    
    selected_popkas <- isolate(input$initial_table_columns_selected)
    
    complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    selected_dt <- read.csv2(selected_path)
    
    #save price and price_in_dollar into another dataset to add it later
    prices <- select(selected_dt, price, price_in_dollar)
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(X, price, price_in_dollar))
    
    positions <- c(selected_popkas)
    withfilters <- dplyr::select(selected_dt, positions)
    parent_column_names <- paste(colnames(withfilters), collapse = ", ")
    
    source("exp_module.R")
    selected_dt <- callModule(a, "exp", reactive(selected_dt), reactive(parent_column_names), reactive(input$plus_name))

    #dts should be written with price, price_in_dollar and new column
    #should add price and price_in_dollar before writing dts
    selected_dt <- cbind(prices, selected_dt)
    
    write.csv2(selected_dt, selected_path)
    
    models <- read.csv2("~/Downloads/models.csv")
    temporary_name <- paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv")
    
    #write temporary file with all prices
    write.csv2(selected_dt, temporary_name)
    
    columns_table <- read.csv2("columns_table.csv")
    columns_table <- select(columns_table, -c(X))
    columns_table <- filter(columns_table, column_name != input$plus_name)
    column_name <- input$plus_name
    profile_module_used <- "exp_module.R"
    addition <- data.frame(column_name, profile_module_used, parent_column_names)
    columns_table <- rbind(columns_table, addition)
    write.csv2(columns_table, "columns_table.csv")
    
    #these positions have to equal the positions selected, that is why dt has to be the same
    selected_dt <- select(selected_dt, -c(price, price_in_dollar))
    
    output$initial_table <- DT::renderDataTable(selected_dt, selection = list(target = 'column'), options = list(
      scrollX = TRUE
    ))
    #forbid users to uncheck obligatory columns
    selected_dt <- select(selected_dt, -c(distance, volume, weight, region.x, region.y, lat_to, lat_from, long_to, long_from))
    updateCheckboxGroupInput(session, "checkGroup", label = "Выбор столбцов для обчучения модели", choices = colnames(selected_dt),
                             selected = colnames(selected_dt), inline = TRUE)
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
      callbackR = function(x) { if(x != FALSE) {
        
        cargo_types <- read.csv2("data_table_column_vector.csv")
        cargo_types <- select(cargo_types, -c(X))
        the_group <- input$shinyalert
        column_vector <- input$shinyalert
        Freq <- 1
        addition <- data.frame(column_vector, Freq, the_group)
        cargo_type_table <- rbind(addition, cargo_types)
        
        nocargotype <- data.frame(cargo_type_table[!duplicated(cargo_type_table$the_group), ])
        updateSelectInput(session, "select_cargo_type", choices = (nocargotype[ncol(nocargotype)]))

      }}
    )
  })
  
  observeEvent(input$show_unhandled, {
    complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    selected_dt <- read.csv2(selected_path)
    selected_dt <- select(selected_dt, -c(X))
    column_vector <- select(selected_dt, input$select_column)
    column_vector <- data.frame(column_vector)
    data_table_column_vector <- data.frame(table(column_vector))
    data_table_column_vector$the_group <- data_table_column_vector$column_vector
    write.csv2(data_table_column_vector, file = "data_table_column_vector.csv")
  })
  
  observeEvent(input$add_elements_to_selected_groups, {
    
    cargo_types <- read.csv2("data_table_column_vector.csv")
    cargo_types <- select(cargo_types, -c(X))
    selected_popkas <- isolate(input$x14_rows_selected)
    
    if (length(selected_popkas) > 0){
      cargo_types$id <- c(1:nrow(cargo_types))
      changed_values <- filter(cargo_types, id == selected_popkas[1])
      
      if (length(selected_popkas) > 1){
        
        for (i in 2:length(selected_popkas)){
          changed_values_add <- filter(cargo_types, id == selected_popkas[i])
          changed_values <- rbind(changed_values, changed_values_add)
        }
        
      }
      
      changed_values$the_group <- input$select_cargo_type
      
      for (i in 1:length(selected_popkas)){
        cargo_types <- filter(cargo_types, id != selected_popkas[i])
      }
      
      cargo_types <- rbind(changed_values, cargo_types)
      cargo_types <- select(cargo_types, -c(id))
    }
    
    write.csv2(cargo_types, file = "data_table_column_vector.csv")
    #click("show_unhandled")
  })
  
  observeEvent(input$int_range_setup, {
    
    #select current column
    cargo_types <- read.csv2("data_table_column_vector.csv")
    cargo_types <- select(cargo_types, -c(X))
    
    
    #filter the selected numbers from it
    #column_vector into int
    changed_values <- filter(cargo_types, as.numeric(as.matrix(column_vector)) > input$former_rule[1])
    changed_values <- filter(changed_values, as.numeric(as.matrix(column_vector)) < input$former_rule[2])
    
    #assign the selected group to these column_vectors
    changed_values$the_group <- input$select_cargo_type
    
    #filter filtered rows from original cargo_types
    cargo_types_low <- filter(cargo_types, as.numeric(as.matrix(column_vector)) < input$former_rule[1])
    cargo_types_high <- filter(cargo_types, as.numeric(as.matrix(column_vector)) > input$former_rule[2])
    cargo_types <- rbind(cargo_types_low, changed_values, cargo_types_high)
    write.csv2(cargo_types, file = "data_table_column_vector.csv")
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
      callbackR = function(x) { if(x == TRUE) {
        
        cargo_types <- read.csv2("data_table_column_vector.csv")
        cargo_types <- select(cargo_types, -c(X))
        the_group_to_delete <- filter(cargo_types, the_group == input$select_cargo_type)
        cargo_types <- filter(cargo_types, the_group != input$select_cargo_type)
        the_group_to_delete$the_group <- the_group_to_delete$column_vector
        cargo_types <- rbind(the_group_to_delete, cargo_types)
        write.csv2(cargo_types, file = "data_table_column_vector.csv")
        click("add_elements_to_selected_groups")
        
      }}
    )
  })
  
  observeEvent(input$save_new_column, {
    
    shinyalert(
      title = "Создать новый столбец",
      text = "Введите названия для нового столбца латиницей",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "input",
      inputType = "text",
      inputValue = "",
      inputPlaceholder = "new_column",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Создать",
      confirmButtonCol = "#AEDEF4",
      cancelButtonText = "Отменить",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) {
        
        #load profiles
        cargo_profiles <- read.csv2("~/Downloads/groups_profiles.csv")
        cargo_profiles <- select(cargo_profiles, user_groups_profile_name, system_groups_profile_name)
        
        #check the last number, then plus one
        system_groups_profile_name <- paste0("groups_profile_", as.numeric(gsub('\\D+','', tail(cargo_profiles, n=1)$system_groups_profile_name))+1, ".csv")
        
        #load input name
        user_groups_profile_name <- input$shinyalert
        
        addition <- data.frame(user_groups_profile_name, system_groups_profile_name)
        
        #upload created dataset
        cargo_types <- read.csv2("data_table_column_vector.csv")
        cargo_types <- select(cargo_types, -c(X))
        print(head(cargo_types))
        write.csv2(cargo_types, file = paste0("groups_profile_", as.numeric(gsub('\\D+','', tail(cargo_profiles, n=1)$system_groups_profile_name))+1, ".csv"))
        cargo_profiles <- rbind(cargo_profiles, addition)
        write.csv2(cargo_profiles, file = "~/Downloads/groups_profiles.csv")
        
        updateSelectInput(session, "select_cargo_type_profile", choices = cargo_profiles$user_groups_profile_name)

        #upload original dataset
        complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
        complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
        selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
        selected_dt <- read.csv2(selected_path)
        selected_dt <- select(selected_dt, -c(X))
        selected_dt$column_vector <- pull(select(selected_dt, input$select_column))
        
        #left_join it
        selected_dt <- left_join(selected_dt, cargo_types, by = "column_vector")
        #give the name to the new column
        colnames(selected_dt)[ncol(selected_dt)] <- 
          input$shinyalert
        
        selected_dt <- select(selected_dt, -c(column_vector))
        
        #save it
        write.csv2(selected_dt, selected_path)
        
        models <- read.csv2("~/Downloads/models.csv")
        temporary_name <- paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv")
        
        #write temporary file with all prices
        write.csv2(selected_dt, temporary_name)
        
        #output without price and price_in_dollar
        selected_dt <- select(selected_dt, -c(price, price_in_dollar, Freq))
        output$initial_table <- DT::renderDataTable(selected_dt, selection = list(target = 'column'), options = list(
          scrollX = TRUE
        ))
        
        #for checkbox exclude necessary columns
        selected_dt <- select(selected_dt, -c(distance, volume, weight, region.x, region.y, lat_to, lat_from, long_to, long_from))
        updateCheckboxGroupInput(session, "checkGroup", label = "Выбор столбцов для обчучения модели", choices = colnames(selected_dt),
                                 selected = colnames(selected_dt), inline = TRUE)
        columns_table <- read.csv2("columns_table.csv")
        columns_table <- select(columns_table, -c(X))
        columns_table <- filter(columns_table, column_name != input$shinyalert)
        column_name <- input$shinyalert
        profile_module_used <- paste0("groups_profile_", 
                                      as.numeric(gsub('\\D+','', tail(cargo_profiles, n=1)$system_groups_profile_name)), ".csv")
        
        parent_column_names <- input$select_column
        addition <- data.frame(column_name, profile_module_used, parent_column_names)
        columns_table <- rbind(columns_table, addition)
        write.csv2(columns_table, "columns_table.csv")
        
      }
    )
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
      callbackR = function(x) { if(x != FALSE) {
        #load profiles
        cargo_profiles <- read.csv2("~/Downloads/groups_profiles.csv")
        cargo_profiles <- select(cargo_profiles, user_groups_profile_name, system_groups_profile_name)
        
        #check the last number, then plus one
        system_groups_profile_name <- paste0("groups_profile_", as.numeric(gsub('\\D+','', tail(cargo_profiles, n=1)$system_groups_profile_name))+1, ".csv")
        
        #load input name
        user_groups_profile_name <- input$shinyalert
        
        addition <- data.frame(user_groups_profile_name, system_groups_profile_name)
        
        cargo_types <- read.csv2("data_table_column_vector.csv")
        cargo_types <- select(cargo_types, -c(X))
        
        write.csv2(cargo_types, file = paste0("groups_profile_", as.numeric(gsub('\\D+','', tail(cargo_profiles, n=1)$system_groups_profile_name))+1, ".csv"))
        cargo_profiles <- rbind(cargo_profiles, addition)
        write.csv2(cargo_profiles, file = "~/Downloads/groups_profiles.csv")
        
        updateSelectInput(session, "select_cargo_type_profile", choices = cargo_profiles$user_groups_profile_name)
      }}
    )
    
  })
  
  observeEvent(input$use_profile, {
    
    cargo_profiles <- read.csv2("~/Downloads/groups_profiles.csv")
    cargo_profiles <- filter(cargo_profiles, user_groups_profile_name == input$select_cargo_type_profile)
    group_profiles <- read.csv2(as.matrix(cargo_profiles$system_groups_profile_name)[1])
    group_profiles <- select(group_profiles, -c(X))
    cargo_types <- read.csv2("data_table_column_vector.csv")
    cargo_types <- select(cargo_types, -c(X))
    the_column_vector <- select(cargo_types, column_vector)
    group_profiles <- inner_join(the_column_vector, group_profiles)
    cargo_types <- rbind(group_profiles, cargo_types)
    cargo_types <- data.frame(cargo_types[!duplicated(cargo_types$column_vector), ])
    write.csv2(cargo_types, file = "data_table_column_vector.csv")
    
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
        cargo_profiles <- read.csv2("~/Downloads/groups_profiles.csv")
        cargo_profiles <- select(cargo_profiles, -c(X))
        #filter the input value
        cargo_profiles <- filter(cargo_profiles, user_groups_profile_name != input$select_cargo_type_profile)
        #save
        write.csv2(cargo_profiles, file = "~/Downloads/groups_profiles.csv")
        updateSelectInput(session, "select_cargo_type_profile", choices = cargo_profiles$user_cargo_profile_name)
      }}
    )
  })
  
  observeEvent(input$many_plots_generator, {
    models <- read.csv2("~/Downloads/models.csv")
    
    #read temporary
    selected_dt <- read.csv2(paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv"))
    selected_dt <- select(selected_dt, -c(X))
    selected_dt <- dplyr::select_if(selected_dt, is.numeric)
    # Insert the right number of plot output objects into the web page
    output$plots <- renderUI({
      plot_output_list <- lapply(1:ncol(selected_dt), function(i) {
        plotname <- paste("plot", i, sep="")
        plotOutput(plotname)
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in 1:ncol(selected_dt)) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        output[[plotname]] <- renderPlot({
          x <- as.numeric(as.matrix(selected_dt[my_i]))
          hist(x, breaks = 75,
               main = colnames(selected_dt[my_i]))
        })
      })
    }
  })
  
  observeEvent(input$many_groups_generator, {
    models <- read.csv2("~/Downloads/models.csv")
    
    #read temporary
    selected_dt <- read.csv2(paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv"))
    selected_dt <- select(selected_dt, -c(X))
    selected_dt_logic <- dplyr::select_if(selected_dt, is.logical)
    # To do it for all names
    selected_dt_logic[] <- lapply( selected_dt_logic, factor) # the "[]" keeps the dataframe structure
    col_names <- names(selected_dt_logic)
    # do do it for some names in a vector named 'col_names'
    selected_dt_logic[col_names] <- lapply(selected_dt_logic[col_names] , factor)
    selected_dt <- data.frame(selected_dt, selected_dt_logic)
    selected_dt <- dplyr::select_if(selected_dt, is.factor)
    # Insert the right number of plot output objects into the web page
    output$groupies <- renderUI({
      plot_output_list <- lapply(1:ncol(selected_dt), function(i) {
        plotname <- paste("group", i, sep="")
        #checkboxInput("checkbox", "Choice A", value = TRUE)
        plotOutput(plotname)
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in 1:ncol(selected_dt)) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("group", my_i, sep="")
        output[[plotname]] <- renderPlot({
          
          barplot(summary(pull(selected_dt[my_i])), col="grey50", 
                  main="",
                  ylab=colnames(selected_dt[my_i]),
                  xlab = "",
                  space=1)
          
        })
      })
    }
  })
  
  observeEvent(input$make_select_column_filter, {
    
    complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
    complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
    selected_path <- paste0("~/Downloads/", complete_dts_filter$system_name)
    selected_dt <- read.csv2(selected_path)
    selected_dt <- select(selected_dt, -c(X))
    selected_dt <- select(selected_dt, input$select_column_filter)
    selected_dt$numeric_column <- pull(selected_dt[1])
    selected_dt <- select(selected_dt, numeric_column)
    selected_dt <- na.omit(selected_dt)
    selected_dt$numeric_column <- sort(selected_dt$numeric_column)
    updateSliderInput(session, "numeric_filter", min=selected_dt[1,1], max=selected_dt[nrow(selected_dt), 1], 
                      value=c(selected_dt[1,1], selected_dt[nrow(selected_dt), 1]), step=0.5)
    updateSelectInput(session, "select_filtered_value", choices = (selected_dt$numeric_column))
  })
  
  observeEvent(input$apply_numeric_filter, {
    
    lower_border <- read.csv2("~/Downloads/cargo_type_module/lower_border.csv")
    lower_border <- select(lower_border, -c(X))
    column_name <- input$select_column_filter
    the_filter <- input$numeric_filter[1]
    filters_dataset <- data.frame(column_name, the_filter)
    lower_border <- rbind(lower_border, filters_dataset)
    write.csv2(lower_border, "~/Downloads/cargo_type_module/lower_border.csv")
    
    upper_border <- read.csv2("~/Downloads/cargo_type_module/upper_border.csv")
    upper_border <- select(upper_border, -c(X))
    column_name <- input$select_column_filter
    the_filter <- input$numeric_filter[2]
    filters_dataset <- data.frame(column_name, the_filter)
    upper_border <- rbind(upper_border, filters_dataset)
    write.csv2(upper_border, "~/Downloads/cargo_type_module/upper_border.csv")

  })
  
  observeEvent(input$apply_factor_filter, {
    
    factor_filter <- read.csv2("~/Downloads/cargo_type_module/factor_filter.csv")
    factor_filter <- select(factor_filter, -c(X))
    column_name <- input$select_column_filter
    the_filter <- input$select_filtered_value
    filters_dataset <- data.frame(column_name, the_filter)
    factor_filter <- rbind(factor_filter, filters_dataset)
    write.csv2(factor_filter, "~/Downloads/cargo_type_module/factor_filter.csv")
    
  })
  
  low_border <- eventReactive({
    input$apply_numeric_filter
    input$delete_filters
    input$select_column_filter
    }, {
    lower_border <- read.csv2("~/Downloads/cargo_type_module/lower_border.csv")
    lower_border <- select(lower_border, -c(X))
    lower_border
  })
  
  up_border <- eventReactive({
    input$apply_numeric_filter
    input$delete_filters
    input$select_column_filter
  }, {
    upper_border <- read.csv2("~/Downloads/cargo_type_module/upper_border.csv")
    upper_border <- select(upper_border, -c(X))
    upper_border
  })
  
  fac_filter <- eventReactive({
    input$apply_factor_filter
    input$delete_filters
    input$select_column_filter
  }, {
    factor_filter <- read.csv2("~/Downloads/cargo_type_module/factor_filter.csv")
    factor_filter <- select(factor_filter, -c(X))
    factor_filter
  })
  
  observeEvent(input$apply_filters, {
    low_border_col_sel <- isolate(input$lower_border_rows_selected)
    up_border_col_sel <- isolate(input$upper_border_rows_selected)
    fac_filter_col_sel <- isolate(input$factor_filter_rows_selected)
    
    models <- read.csv2("~/Downloads/models.csv")
    
    #read temporary
    temporary <- read.csv2(paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv"))
    temporary <- select(temporary, -c(X))
    
    lower_border <- read.csv2("lower_border.csv")
    if (length(low_border_col_sel) > 0){
      lower_border$id <- c(1:nrow(lower_border))
      changed_values <- filter(lower_border, id == low_border_col_sel[1])
      
      if (length(low_border_col_sel) > 1){
        for (i in 2:length(low_border_col_sel)){
          changed_values_add <- filter(lower_border, id == low_border_col_sel[i])
          changed_values <- rbind(changed_values, changed_values_add)
        }
      }
    
    
      if (nrow(changed_values) > 0) {
        for (i in 1:nrow(changed_values)) {
          temporary <- filter(temporary, 
                              temporary[which(colnames(temporary) == changed_values$column_name[i])] >= as.double(changed_values$the_filter[i]))
        }
      }
    }
    
    upper_border <- read.csv2("upper_border.csv")
    if (length(up_border_col_sel) > 0) {
      upper_border$id <- c(1:nrow(upper_border))
      changed_values <- filter(upper_border, id == up_border_col_sel[1])
      
      if (length(up_border_col_sel) > 1){
        for (i in 2:length(up_border_col_sel)){
          changed_values_add <- filter(upper_border, id == up_border_col_sel[i])
          changed_values <- rbind(changed_values, changed_values_add)
        }
      }
    
      if (nrow(changed_values) > 0) {
        for (i in 1:nrow(changed_values)) {
          temporary <- filter(temporary, 
                              temporary[which(colnames(temporary) == changed_values$column_name[i])] <= as.double(changed_values$the_filter[i]))
        }
      }
    }
    
    factor_filter <- read.csv2("factor_filter.csv")
    if (length(fac_filter_col_sel) > 0) {
      factor_filter$id <- c(1:nrow(factor_filter))
      changed_values <- filter(factor_filter, id == fac_filter_col_sel[1])
      
      if (length(fac_filter_col_sel) > 1){
        for (i in 2:length(fac_filter_col_sel)){
          changed_values_add <- filter(factor_filter, id == fac_filter_col_sel[i])
          changed_values <- rbind(changed_values, changed_values_add)
        }
      }
      if (nrow(changed_values) > 0) {
        for (i in 1:nrow(changed_values)) {
          temporary <- filter(temporary, 
                              temporary[which(colnames(temporary) == changed_values$column_name[i])] != as.character(changed_values$the_filter[i]))
        }
      }
    }
    models <- read.csv2("~/Downloads/models.csv")
    temporary_name <- paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv")
    
    #write temporary file with all prices
    write.csv2(temporary, temporary_name)
  })
  
  observeEvent(input$delete_filters, {
    low_border_col_sel <- isolate(input$lower_border_rows_selected)
    up_border_col_sel <- isolate(input$upper_border_rows_selected)
    fac_filter_col_sel <- isolate(input$factor_filter_rows_selected)
    
    #upload filter dataset
    if (length(low_border_col_sel) > 0){
      lower_border <- read.csv2("lower_border.csv")
      lower_border <- select(lower_border, -c(X))
      lower_border$id <- c(1:nrow(lower_border))
      changed_values <- filter(lower_border, id != low_border_col_sel[1])
      
      if (length(low_border_col_sel) > 1){
        for (i in 2:length(low_border_col_sel)){
          changed_values <- filter(changed_values, id != low_border_col_sel[i])
        }
      }
      changed_values <- select(changed_values, -c(id))
      write.csv2(changed_values, "lower_border.csv")
    }
    
    if (length(up_border_col_sel) > 0){
      upper_border <- read.csv2("upper_border.csv")
      upper_border <- select(upper_border, -c(X))
      upper_border$id <- c(1:nrow(upper_border))
      changed_values <- filter(upper_border, id != up_border_col_sel[1])
      
      if (length(up_border_col_sel) > 1){
        for (i in 2:length(up_border_col_sel)){
          changed_values <- filter(changed_values, id != up_border_col_sel[i])
        }
      }
      changed_values <- select(changed_values, -c(id))
      write.csv2(changed_values, "upper_border.csv")
    }
    
    if (length(fac_filter_col_sel) > 0){
      factor_filter <- read.csv2("factor_filter.csv")
      factor_filter <- select(factor_filter, -c(X))
      factor_filter$id <- c(1:nrow(factor_filter))
      changed_values <- filter(factor_filter, id != fac_filter_col_sel[1])
      
      if (length(fac_filter_col_sel) > 1){
        for (i in 2:length(fac_filter_col_sel)){
          changed_values <- filter(changed_values, id != fac_filter_col_sel[i])
        }
      }
      changed_values <- select(changed_values, -c(id))
      write.csv2(changed_values, "factor_filter.csv")
    }
  })
  
  observeEvent(input$unsupervised, {
    #import temporary dataset
    models <- read.csv2("~/Downloads/models.csv")
    
    #read temporary
    temporary <- read.csv2(paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv"))
    temporary <- select(temporary, -c(X))
    #do na.omit
    temporary <- na.omit(temporary)
    
    #perform select according to the checkBoxGroup
    temporary <- select(temporary, distance, volume, weight, lat_to, lat_from, long_to, long_from, input$checkGroup)
    print(sapply(temporary, class))
    hc.complete=hclust(dist(temporary), method=input$cluster_method)
    hclusters <<- cutree(hc.complete, input$clusters_number)
    
  })
  
  observeEvent(input$make_the_tree, {
    #import temporary dataset
    models <- read.csv2("~/Downloads/models.csv")
    
    #read temporary
    temporary <- read.csv2(paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv"))
    temporary <- select(temporary, -c(X))
    #do na.omit
    temporary <- na.omit(temporary)
    #perform select according to the checkBoxGroup
    temporary <- select(temporary, price, distance, volume, weight, lat_to, lat_from, long_to, long_from, input$checkGroup)
    
    if (length(hclusters)>0) {
      temporary$clusters <- hclusters
    }
    
    #do all the tree things
    train_ind <- sample(1:nrow(temporary), 0.85*nrow(temporary))
    train <- temporary[train_ind,]
    test <- temporary[-train_ind,]
    rfModel <-randomForest(price ~ ., data=train)
    rfPredict<-predict(rfModel, test, probability=FALSE)
    rfPredict<-data.frame(rfPredict)
    ruble <- paste0((1-(mean(abs(rfPredict$rfPredict-test$price)/test$price)))*100,"%")
   
    #import temporary dataset
    models <- read.csv2("~/Downloads/models.csv")
    
    #read temporary
    temporary <- read.csv2(paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv"))
    temporary <- select(temporary, -c(X))
    #do na.omit
    temporary <- na.omit(temporary)
    #perform select according to the checkBoxGroup
    temporary <- select(temporary, price_in_dollar, distance, volume, weight, lat_to, lat_from, long_to, long_from, input$checkGroup)
    
    if (length(hclusters)>0) {
      temporary$clusters <- hclusters
    }
    
    #do all the tree things
    train_ind <- sample(1:nrow(temporary), 0.85*nrow(temporary))
    train <- temporary[train_ind,]
    test <- temporary[-train_ind,]
    rfModel <-randomForest(price_in_dollar ~ ., data=train)
    rfPredict<-predict(rfModel, test, probability=FALSE)
    rfPredict<-data.frame(rfPredict)
    dollar <- paste0((1-(mean(abs(rfPredict$rfPredict-test$price_in_dollar)/test$price_in_dollar)))*100,"%")
    output$ruble_dollar <- renderTable(data.frame(ruble, dollar))#, colnames = c('точность прогноза в рублях', 'точность прогноза в долларах'))
  })
  
  observeEvent(input$save_the_model, {

    shinyalert(
      title = "Создать модель",
      text = "Введите название для модели",
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
      callbackR = function(x) { if(x != FALSE) {
        
        #import models dt that contains all the models created
        models <- read.csv2("~/Downloads/models.csv")
        models <- select(models, -c(X))
        
        #load input name
        user_model_name <- input$shinyalert
        server_model_name <- paste0(tail((as.numeric(gsub('\\D+','', models$server_model_name))), n=1)+1, ".Rdata")
        dollar_model_name <- paste0("dollar_", tail((as.numeric(gsub('\\D+','', models$dollar_model_name))), n=1)+1, ".Rdata")
        columns_table <- paste0("columns_table_", tail((as.numeric(gsub('\\D+','', models$columns_table))), n=1)+1, ".csv")
        
        #if there are clusters, create cluster prediction model
        if (length(hclusters)>0) {
          
          #import temporary dataset
          models <- read.csv2("~/Downloads/models.csv")
          
          #read temporary
          temporary <- read.csv2(paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv"))
          temporary <- select(temporary, -c(X))
          #do na.omit
          temporary <- na.omit(temporary)
          #perform select according to the checkBoxGroup
          temporary <- select(temporary, distance, volume, weight, lat_to, lat_from, long_to, long_from, input$checkGroup)
            
          temporary$clusters <- hclusters
          
          #make clusters character - factor values
          temporary$clusters <- as.factor(temporary$clusters)
          
          rfModel_cluster <-randomForest(factor(clusters) ~ ., data=temporary)
          cluster_model_name <- paste0("cluster", tail((as.numeric(gsub('\\D+','', na.omit(models$cluster_model_name)))), n=1)+1, ".Rdata")
          save(rfModel_cluster, file = cluster_model_name)
        } else {
          cluster_model_name <- NA
        }
        #import temporary dataset
        models <- read.csv2("~/Downloads/models.csv")
        
        #read temporary
        temporary <- read.csv2(paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv"))
        temporary <- select(temporary, -c(X))
        #do na.omit
        temporary <- na.omit(temporary)
        #perform select according to the checkBoxGroup
        temporary <- select(temporary, price, distance, volume, weight, lat_to, lat_from, long_to, long_from, input$checkGroup)
        
        if (length(hclusters)>0) {
          temporary$clusters <- hclusters
          #make clusters character - factor values
          temporary$clusters <- as.factor(temporary$clusters)
        }
        
        #machine learning on price prediction
        #print(colnames(temporary))
        rfModel <-randomForest(price ~ ., data=temporary)
        
        complete_dts <- read.csv2("~/Downloads/complete_dts.csv")
        complete_dts_filter <- filter(complete_dts, user_name == input$initial_select)
        cargo_types_ds <- as.character(complete_dts_filter$cargo_types_ds)
        save(rfModel, file = server_model_name)
        
        #import temporary dataset
        models <- read.csv2("~/Downloads/models.csv")
        models <- select(models, -c(X))
        #read temporary
        temporary <- read.csv2(paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv"))
        temporary <- select(temporary, -c(X))
        #do na.omit
        temporary <- na.omit(temporary)
        #perform select according to the checkBoxGroup
        temporary <- select(temporary, price_in_dollar, distance, volume, weight, lat_to, lat_from, long_to, long_from, input$checkGroup)
        
        if (length(hclusters)>0) {
          temporary$clusters <- hclusters
          #make clusters character - factor values
          temporary$clusters <- as.factor(temporary$clusters)
        }
        
        #machine learning on price prediction
        rfModel_dollar <-randomForest(price_in_dollar ~ ., data=temporary)
        save(rfModel_dollar, file = dollar_model_name)
        
        original_dt <- as.character(complete_dts_filter$system_name)
        car_types_ds <- as.character(complete_dts_filter$car_types_ds)
        column_name <- temporary
        temporary <- paste0("temporary", as.numeric(gsub('\\D+','', tail(models, n=1)$temporary))+1, ".csv")
        addition <- data.frame(user_model_name, server_model_name, cluster_model_name, columns_table, cargo_types_ds, original_dt, dollar_model_name, car_types_ds, temporary)
        models <- rbind(models, addition)
        updateSelectInput(session, "choose_for_prediction", choices = (models$user_model_name))
        write.csv2(models, "~/Downloads/models.csv")

        column_name <- colnames(column_name)
        column_name <- data.frame(column_name)
        
        columns_table <- read.csv2("columns_table.csv")
        columns_table <- select(columns_table, -c(X))
        
        column_name <- left_join(column_name, columns_table)
        write.csv2(column_name, file = as.character(addition$columns_table))
        
        print("Done")
      }}
    )
  })
  
  observeEvent(input$choose_for_prediction, {
    the_model_to_use <- input$choose_for_prediction
    the_model_to_use <- data.frame(the_model_to_use)
    write.csv2(the_model_to_use, "the_model_to_use.csv")
  })
  
  observeEvent(input$delete_model, {
    
    shinyalert(
      title = "Вы точно хотите удалить эту модель?",
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
      callbackR = function(x) { if(x == TRUE) {
        
        models <- read.csv2("~/Downloads/models.csv")
        models <- select(models, -c(X))
        
        models <- filter(models, user_model_name != input$choose_for_prediction)
        
        updateSelectInput(session, "choose_for_prediction", choices = (models$user_model_name))
        
        write.csv2(models, "~/Downloads/models.csv")
      }}
    )
  })
  
  df <- eventReactive({input$create_new_rule
    input$add_elements_to_selected_groups
    input$use_profile
    input$int_range_setup
    #input$delete_rule
    input$show_unhandled}, {
      Sys.sleep(1)
      cargo_types <- read.csv2("data_table_column_vector.csv")
      cargo_types <- select(cargo_types, -c(X))
      nocargotype <- data.frame(cargo_types[!duplicated(cargo_types$the_group), ])
      updateSelectInput(session, "select_cargo_type", choices = (nocargotype[ncol(nocargotype)]))
      updateSliderInput(session, "former_rule", min=cargo_types$column_vector[1], max=cargo_types$column_vector[nrow(cargo_types)], value=c(cargo_types$column_vector[1], cargo_types$column_vector[nrow(cargo_types)]),
                        step=1)
      cargo_types
    })

}