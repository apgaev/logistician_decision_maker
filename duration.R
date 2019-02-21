duration <- function(input, output, session) {
  
  #import dataset
  daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
  #get rid of unfilled columns
  daf$nas <- c(is.na(daf[13]))
  daf <- filter(daf, nas == FALSE)
  
  dfsix = as.matrix(daf[9])
  dfsix <- as.Date(dfsix, format = "%d.%m.%Y")
  moscow = as.matrix(daf[10])
  moscow <- as.Date(moscow, format = "%d.%m.%Y")
  dfsix <- data.frame(dfsix, moscow)
  dfsix$duration <- dfsix$moscow - dfsix$dfsix
  dfsix$calendar <- dfsix$dfsix
  positions <- c(3,4)
  dfsix <- select(dfsix, positions)
  dfsix$X <- c(1:nrow(dfsix))
  return(dfsix)
  
}