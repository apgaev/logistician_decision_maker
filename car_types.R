car_type_preparation <- function(input, output, session) {
  car_types <- read.csv2("car_type.csv")
  car_types <- dplyr::select(car_types, car_type, original_value)
  
  #upload current data
  daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
  daf$nas <- c(is.na(daf[13]))
  daf <- filter(daf, nas == FALSE)
  
  id = dplyr::select(daf, X)
  
  dfsix = as.matrix(daf[28])
  
  #start deleting unnecesary objects
  todelete = grepl("авиа", dfsix)
  df = data.frame(daf, todelete)
  moscow = filter(df, todelete == FALSE)
  
  moscow = dplyr::select(moscow, -todelete)
  dfsix = as.matrix(moscow[28])
  todelete = grepl("жд", dfsix)
  df = data.frame(moscow, todelete)
  moscow = filter(df, todelete == FALSE)
  
  moscow = dplyr::select(moscow, -todelete)
  dfsix = as.matrix(moscow[28])
  todelete = grepl("ж/д", dfsix)
  df = data.frame(moscow, todelete)
  moscow = filter(df, todelete == FALSE)
  
  dfsix = as.matrix(moscow[28])
  
  todelete = grepl("юбой", dfsix)
  df = data.frame(moscow, todelete)
  moscow = filter(df, todelete == TRUE)
  df = filter(df, todelete == FALSE)
  df = dplyr::select(df, -todelete)
  dfsix = as.matrix(df[28])
  car_type <- "тент"
  todelete = TRUE
  car_type <- data.frame(car_type, todelete)
  moscow = left_join(moscow, car_type, by = "todelete")
  adresestojoin = moscow
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Обработка типов ТС", value = 0)
  
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
    car_type <- data.frame(car_type, todelete)
    moscow = left_join(moscow, car_type, by = "todelete")
    adresestojoin = rbind(adresestojoin, moscow)
  }
  
  nocartype <- left_join(id, adresestojoin)
  nocartype$nas <- c(is.na(nocartype[2]))
  return(nocartype)
}

withcartype_func <- function(input, output, session, nocartype) {
  withcartype <- filter(nocartype(), nas == FALSE)
  positions <- c(1,35)
  
  #adds X and car_type to the final table
  withcartype <- dplyr::select(withcartype, positions)
  
  #withcartype and nocartype as outputs of the process
  return(withcartype)
}

nocartype_func <- function(input, output, session, nocartype) {
  nocartype <- filter(nocartype(), nas == TRUE)
  nocartype = dplyr::select(nocartype, X)
  
  #upload current data
  daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
  daf$nas <- c(is.na(daf[13]))
  daf <- filter(daf, nas == FALSE)
  
  nocartype <- left_join(nocartype, daf)
  positions <- c(1,28)
  nocartype <- dplyr::select(nocartype, positions)
  return(nocartype)
}