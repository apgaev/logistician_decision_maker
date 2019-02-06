two_loadings_detector <- function(input, output, session) {
  
  #define necessary columns from the original dataset
  positions <- c(1,5,9:14,19,22,23,26,28,31,32)
  
  #import dataset
  daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
  #get rid of unfilled columns
  daf$nas <- c(is.na(daf[13]))
  daf <- filter(daf, nas == FALSE)
  
  df <- dplyr::select(daf, positions)
  
  #in order to work with departure addresses extract it from the dataset
  dfsix = as.matrix(df[7])
  
  #join destination addresses
  dfsix <- rbind(dfsix, as.matrix(df[8]))
  
  #define possible beginnings of the enumeration
  withpoint <- substr(dfsix, 1, 2) == "1."
  scopka <- substr(dfsix, 1, 2) == "1)"
  probel <- substr(dfsix, 1, 2) == "1 "
  with_two_loadings <- data.frame(withpoint, scopka, probel)
  with_two_loadings <- unite(with_two_loadings, two_loads, c(1:3), sep = "", remove = TRUE)
  with_two_loadings$two_loadings <- grepl("TRUE", with_two_loadings$two_loads)
  with_two_loadings$X <- c(1:nrow(with_two_loadings))
  with_two_loadings <- select(with_two_loadings, X, two_loadings)
  
  #separate destination addresses
  with_two_loadings_low <- tail(with_two_loadings, n=nrow(with_two_loadings)/2)
  with_two_loadings_low$X <- c(1:nrow(with_two_loadings_low))
  with_two_loadings <- head(with_two_loadings, n=nrow(with_two_loadings)/2)
  
  #join destination addresses to appropriate departure
  with_two_loadings <- left_join(with_two_loadings, with_two_loadings_low, by = "X")
  return(with_two_loadings)
}