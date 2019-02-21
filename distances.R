distances <- function(input, output, session, destination_final, departure_final) {
  distances <- read.csv2("distances.csv")
  distances <- select(distances, distance, route)
  
  departure <- departure_final()
  destination <- destination_final()
  
  #i should keep the coords
  
  departure  <- select(departure, X, region, city, latitude, longitude)
  destination  <- select(destination, X, region, city, latitude, longitude)
  destination$X <- as.numeric(destination$X)
  departure$X <- as.character(departure$X)
  destination$X <- as.character(destination$X)
  distance_search <- inner_join(departure, destination, by="X")
  distance_search <- unite(distance_search, route, city.x, region.x, city.y, region.y)
  distance_search$route <- gsub("_", " ", distance_search$route)
  distance_search <- left_join(distance_search, distances)
  
  #i need all NAs
  distance_search$nas <- is.na(distance_search$distance)
  distance_search_na <- filter(distance_search, nas == TRUE)
  distance_search <- filter(distance_search, nas == FALSE)
  distance_search <- select(distance_search, -c(nas))
  distance_search_na <- select(distance_search_na, X, route, latitude.x, longitude.x, latitude.y, longitude.y)
  distance_search_na <- data.frame(distance_search_na[!duplicated(distance_search_na$route), ])
  write.csv(distance_search_na, "distance_search.csv")
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  #delete strings that cannot be defined through Yandex parser
  progress$set(message = "Парсинг неопределенных значений", value = 0)
  
  #deploy the parser
  py_run_file("distances.py")
  
  #unpack the parsed json
  jnastr240 = readLines("parsed_distances.json") %>% 
    str_c(collapse = ",") %>%  
    (function(str) str_c("[", str, "]")) %>% 
    fromJSON(simplifyDataFrame = T)
  jnastr240<-jnastr240[[1]]
  
  if (length(jnastr240) > 0) {
    jnastr240 <- filter(jnastr240, distance != 0)
    jnastr240 <- filter(jnastr240, distance != "NA")
    distance_search_na <- inner_join(distance_search_na, jnastr240)
    distance_search <- rbind(distance_search, distance_search_na)
    distance_search_na <- select(distance_search_na, distance, route)
    distances <- rbind(distances, distance_search_na)
    write.csv2(distances, "distances.csv")
  }
  distance_search <- select(distance_search, -c(route))
  return(distance_search)
}