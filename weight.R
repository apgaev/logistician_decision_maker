weight <- function(input, output, session) {
  
  #import dataset
  daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
  #get rid of unfilled columns
  daf$nas <- c(is.na(daf[13]))
  daf <- filter(daf, nas == FALSE)
  
  addition = as.matrix(daf[22])
  addition = gsub(",", ".", addition)
  daf[22] <- as.double(addition)
  dfsix = filter(daf, daf[23] == "кг")
  daf = filter(daf, daf[23] != "кг")
  dfsix[22] <- dfsix[22]*0.001
  daf <- rbind(daf, dfsix)
  daf = filter(daf, daf[22] < 23)
  
  positions <- c(1,22)
  withweight <- dplyr::select(daf, positions)
  withweight$weight <- c(as.matrix(withweight[2]))
  positions <- c(1,3)
  withweight <- select(withweight, positions)
  
  return(withweight)
  
}