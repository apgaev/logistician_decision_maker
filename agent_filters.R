agent_filter <- function(input, output, session) {
  
  #upload current data
  daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
  daf$nas <- c(is.na(daf[13]))
  daf <- filter(daf, nas == FALSE)
  
  id = dplyr::select(daf, X)
  
  agent_filters <- read.csv2("agent_filters.csv")
  agent_filters <- select(agent_filters, agent_number, agent_filter)
  dfsix <- as.matrix(agent_filters)
  allagents <- data.frame(sort(table(daf[5]), decreasing = TRUE))
  for (i in 1:nrow(agent_filters)) {
    daf = filter(daf, daf[2] != dfsix[i, 2])
  }
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Обработка фильтров контрагентов", value = 0)
  
  for (i in 1:nrow(agent_filters)) {
    # Increment the progress bar, and update the detail text.
    progress$inc(1/nrow(agent_filters), detail = percent(i/nrow(agent_filters)))
    allagents = filter(allagents, allagents$Var1 != dfsix[i, 2])
  }
  
  write.csv2(allagents, "allagents.csv")
}