nodeparture_original <- function(input, output, session) {
  
  #import dataset
  daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
  daf$nas <- c(is.na(daf[13]))
  daf <- filter(daf, nas == FALSE)
  
  #extract original IDs for further usage
  id = dplyr::select(daf, X)
  addition <- id
  addition$X <- addition$X*0.000001
  id <- rbind(id, addition)
  
  #define necessary columns from the original dataset
  positions <- c(1,13)
  df <- dplyr::select(daf, positions)
  positions <- c(1,14)
  dest_f <- dplyr::select(daf, positions)
  df$addresses <- pull(df[2])
  dest_f$addresses <- pull(dest_f[2])
  df <- select(df, X, addresses)
  dest_f <- select(dest_f, X, addresses)
  dest_f$X <- dest_f$X*0.000001
  
  #combine departure and destination addresses
  df <- rbind(df, dest_f)
  
  #in order to works with departure addresses extract it from the dataset
  dfsix = as.matrix(df[2])
  
  #define St.Petersburg addresses
  todelete = grepl("Петербург", dfsix)
  df = data.frame(df, todelete)
  moscow = filter(df, todelete == TRUE)
  moscow = dplyr::select(moscow, -c(todelete))
  df = filter(df, todelete == FALSE)
  df = dplyr::select(df, -todelete)
  
  #continue the works without the processed addresses
  dfsix = as.matrix(df[2])
  
  #define St.Petersburg addresses with possible slang namings
  todelete = grepl("СП", dfsix)
  df = data.frame(df, todelete)
  sp = filter(df, todelete == TRUE)
  sp = dplyr::select(sp, -c(todelete))
  moscow = rbind(moscow, sp)
  moscowsix = as.matrix(moscow[2])
  
  #upload the database of St.Petersburg street names
  stpetersburgstreets <- read.csv2("stpetersburgstreets.csv")
  stpetersburgstreets <- dplyr::select(stpetersburgstreets, region,	city,	street,	latitude,	longitude)
  
  #initiate the street-defining process
  todelete = grepl(stpetersburgstreets[1, 3], moscowsix)
  moscow = data.frame(moscow, todelete)
  withstreet = filter(moscow, todelete == TRUE)
  moscow = dplyr::select(moscow, -todelete)
  themoscowstreet = head(stpetersburgstreets, n=1)
  themoscowstreet$todelete = TRUE
  withstreet = left_join(withstreet, themoscowstreet, by = "todelete")
  adresestojoin = withstreet
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Обработка адресов загрузки в Санкт-Петербурге", value = 0)
  
  for (i in 2:nrow(stpetersburgstreets)) {
    # Increment the progress bar, and update the detail text.
    progress$inc(1/nrow(stpetersburgstreets), detail = percent(i/nrow(stpetersburgstreets)))
    todelete = grepl(stpetersburgstreets[i, 3], moscowsix)
    moscow = data.frame(moscow, todelete)
    withstreet = filter(moscow, todelete == TRUE)
    moscow = filter(moscow, todelete == FALSE)
    moscow = dplyr::select(moscow, -todelete)
    moscowsix = as.matrix(moscow[2])
    themoscowstreet = head(stpetersburgstreets, n=i)
    themoscowstreet = tail(themoscowstreet, n=1)
    themoscowstreet$todelete = TRUE
    withstreet = left_join(withstreet, themoscowstreet, by = "todelete")
    adresestojoin = rbind(adresestojoin, withstreet)
  }
  
  #repeat the process with moscow streets
  moscowstreets <- read.csv2("moscowstreets.csv")
  moscowstreets <- dplyr::select(moscowstreets, region,	city,	street,	latitude,	longitude)
  
  df = filter(df, todelete == FALSE)
  df = dplyr::select(df, -todelete)
  dfsix = as.matrix(df[2])
  todelete = grepl("Москва", dfsix)
  df = data.frame(df, todelete)
  moscow = filter(df, todelete == TRUE)
  moscow = dplyr::select(moscow, -c(todelete))
  moscowsix = as.matrix(moscow[2])
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Обработка адресов загрузки в Москве", value = 0)
  
  for (i in 1:nrow(moscowstreets)) {
    # Increment the progress bar, and update the detail text.
    progress$inc(1/nrow(moscowstreets), detail = percent(i/nrow(moscowstreets)))
    todelete = grepl(moscowstreets[i, 3], moscowsix)
    moscow = data.frame(moscow, todelete)
    withstreet = filter(moscow, todelete == TRUE)
    moscow = filter(moscow, todelete == FALSE)
    moscow = dplyr::select(moscow, -todelete)
    moscowsix = as.matrix(moscow[2])
    themoscowstreet = head(moscowstreets, n=i)
    themoscowstreet = tail(themoscowstreet, n=1)
    themoscowstreet$todelete = TRUE
    withstreet = left_join(withstreet, themoscowstreet, by = "todelete")
    adresestojoin = rbind(adresestojoin, withstreet)
  }
  
  #define the cities
  citiestest <- read.csv2("citiesnew.csv")
  citiestest <- dplyr::select(citiestest, region,	city,	street,	latitude,	longitude)
  
  df = filter(df, todelete == FALSE)
  df = dplyr::select(df, -todelete)
  dfsix = as.matrix(df[2])
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Обработка адресов городов загрузки", value = 0)
  
  for (i in 1:nrow(citiestest)) {
    # Increment the progress bar, and update the detail text.
    progress$inc(1/nrow(citiestest), detail = percent(i/nrow(citiestest)))
    todelete = grepl(citiestest[i, 2], dfsix)
    df = data.frame(df, todelete)
    withstreet = filter(df, todelete == TRUE)
    df = filter(df, todelete == FALSE)
    df = dplyr::select(df, -todelete)
    dfsix = as.matrix(df[2])
    themoscowstreet = head(citiestest, n=i)
    themoscowstreet = tail(themoscowstreet, n=1)
    themoscowstreet$todelete = TRUE
    withstreet = left_join(withstreet, themoscowstreet, by = "todelete")
    adresestojoin = rbind(adresestojoin, withstreet)
  }
  
  #there are some cities with the same name in different regions
  #it is necessary to define the exact location
  badcities <- read.csv2("badcities.csv")
  badcities <- dplyr::select(badcities, region,	city,	street,	latitude,	longitude)
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Уточнение одинаковых адресов", value = 0)
  
  for (i in 1:nrow(badcities)) {
    # Increment the progress bar, and update the detail text.
    progress$inc(1/nrow(badcities), detail = percent(i/nrow(badcities)))
    todelete = grepl(badcities[i, 2], dfsix)
    df = data.frame(df, todelete)
    withstreet = filter(df, todelete == TRUE)
    df = filter(df, todelete == FALSE)
    df = dplyr::select(df, -todelete)
    dfsix = as.matrix(df[2])
    themoscowstreet = head(badcities, n=i)
    themoscowstreet = tail(themoscowstreet, n=1)
    themoscowstreet$todelete = TRUE
    withstreet = left_join(withstreet, themoscowstreet, by = "todelete")
    adresestojoin = rbind(adresestojoin, withstreet)
  }
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  #it may occure that the addresses from St.Petersburg did not state the exact name of the city, but had the name of the street
  progress$set(message = "Проверка необработанных адресов в Санкт-Петербурге", value = 0)
  
  for (i in 1:nrow(stpetersburgstreets)) {
    # Increment the progress bar, and update the detail text.
    progress$inc(1/nrow(stpetersburgstreets), detail = percent(i/nrow(stpetersburgstreets)))
    todelete = grepl(stpetersburgstreets[i, 3], dfsix)
    df = data.frame(df, todelete)
    withstreet = filter(df, todelete == TRUE)
    df = filter(df, todelete == FALSE)
    df = dplyr::select(df, -todelete)
    dfsix = as.matrix(df[2])
    themoscowstreet = head(stpetersburgstreets, n=i)
    themoscowstreet = tail(themoscowstreet, n=1)
    themoscowstreet$todelete = TRUE
    withstreet = left_join(withstreet, themoscowstreet, by = "todelete")
    adresestojoin = rbind(adresestojoin, withstreet)
  }
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  #it may occure that the addresses from Moscow did not state the exact name of the city, but had the name of the street
  progress$set(message = "Проверка необработанных адресов в Москве", value = 0)
  
  for (i in 1:nrow(moscowstreets)) {
    # Increment the progress bar, and update the detail text.
    progress$inc(1/nrow(moscowstreets), detail = percent(i/nrow(moscowstreets)))
    todelete = grepl(moscowstreets[i, 3], dfsix)
    df = data.frame(df, todelete)
    withstreet = filter(df, todelete == TRUE)
    df = filter(df, todelete == FALSE)
    df = dplyr::select(df, -todelete)
    dfsix = as.matrix(df[2])
    themoscowstreet = head(moscowstreets, n=i)
    themoscowstreet = tail(themoscowstreet, n=1)
    themoscowstreet$todelete = TRUE
    withstreet = left_join(withstreet, themoscowstreet, by = "todelete")
    adresestojoin = rbind(adresestojoin, withstreet)
  }
  
  #combine all the undefined addresses
  nodeparture <- left_join(id, adresestojoin)
  nodeparture$nas <- c(is.na(nodeparture[2]))
  return(nodeparture) 
}

withdeparture_original <- function(input, output, session, nodeparture_one) {
  withdeparture <- filter(nodeparture_one(), nas == FALSE)
  return(withdeparture)
}

nodeparture_success <- function(input, output, session, nodeparture_one) {
  nodeparture <- filter(nodeparture_one(), nas == TRUE)
  
  nodestination <- filter(nodeparture, X < 1)
  nodeparture <- filter(nodeparture, X>=1)
  
  nodeparture = dplyr::select(nodeparture, X)
  nodestination = dplyr::select(nodestination, X)
  
  #import dataset
  daf <- read.csv2("inputclicks.csv", na.strings=c("","NA"))
  daf$nas <- c(is.na(daf[13]))
  daf <- filter(daf, nas == FALSE)
  
  nodeparture <- left_join(nodeparture, daf)
  positions <- c(1,13)
  nodeparture <- dplyr::select(nodeparture, positions)
  
  daf$X <- daf$X*0.000001
  
  nodestination <- left_join(nodestination, daf)
  
  positions <- c(1,14)
  nodestination <- dplyr::select(nodestination, positions)
  
  #addresses parsing block
  nodeparture$addresses <- c(as.matrix(nodeparture[2]))
  nodestination$addresses <- c(as.matrix(nodestination[2]))
  positions <- c(1,3)
  nodeparture <- dplyr::select(nodeparture, positions)
  nodestination <- dplyr::select(nodestination, positions)
  positions <- c(2)
  
  nodeparture <- rbind(nodeparture, nodestination)
  
  to_maps <- dplyr::select(nodeparture, positions)
  to_maps <- data.frame(to_maps[!duplicated(to_maps), ])
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  #delete strings that cannot be defined through Yandex parser
  progress$set(message = "Удаление неподдающихся определению значений", value = 0)
  
  unparsable <- read.csv2(file = "unparsable.csv")
  unparsable <- select(unparsable, -c(X))
  
  to_maps_v <- to_maps$to_maps..duplicated.to_maps....
  
  for (i in 1:nrow(unparsable)) {
    # Increment the progress bar, and update the detail text.
    progress$inc(1/nrow(unparsable), detail = percent(i/nrow(unparsable)))
    
    todelete = grepl(unparsable[i, 1], to_maps_v)
    
    to_maps_v = data.frame(to_maps_v, todelete)
    to_maps_v = filter(to_maps_v, todelete == FALSE)
    
    to_maps_v <- to_maps_v$to_maps_v
  }
  
  to_maps <- data.frame(to_maps_v)
  to_maps$to_maps..duplicated.to_maps.... <- to_maps$to_maps_v
  to_maps <- select(to_maps, to_maps..duplicated.to_maps....)
  
  #it appears that ":" can cause errors  
  to_maps$to_maps..duplicated.to_maps.... <- gsub(":", "", to_maps$to_maps..duplicated.to_maps....)
  
  #create file with unrecognized addresses that would go to the parser
  write.csv(to_maps, file = "to_maps.csv")
  print(to_maps)
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  #delete strings that cannot be defined through Yandex parser
  progress$set(message = "Парсинг неопределенных значений", value = 0)
  
  #deploy the parser
  py_run_file("addresses_parser.py")
  
  #unpack the parsed json
  jnastr240 = readLines("parsed_addresses.json") %>% 
    str_c(collapse = ",") %>%  
    (function(str) str_c("[", str, "]")) %>% 
    fromJSON(simplifyDataFrame = T)
  jnastr240<-jnastr240[[1]]
  if (length(jnastr240) > 0) {
    #delete irrelevant signs
    jnastr240$region <- gsub(",", "", jnastr240$region)
    jnastr240$street <- gsub(",", "", jnastr240$street)
    jnastr240$latitude <- gsub(",", "", jnastr240$latitude)
    
    to_maps$X <- c(1:nrow(to_maps))
    to_maps <- left_join(to_maps, jnastr240, by = "X")
    
    unparsable <- read.csv2("unparsable.csv")
    unparsable <- select(unparsable, to_maps..duplicated.to_maps....)
    
    leningrad <- filter(jnastr240, region == "Ленинградская")
    if (nrow(leningrad) > 0) {
      jnastr240 <- filter(jnastr240, region != "Ленинградская")
      leningrad$city_test <- grepl("Санкт-Петербург", leningrad$city)
      city_test_positive <- filter(leningrad, city_test == TRUE)
      if (nrow(city_test_positive) > 0) {
        leningrad <- filter(leningrad, city_test == FALSE)
        leningrad <- select(leningrad, -c(city_test))
        city_test_positive$region <- "Санкт-Петербург"
        city_test_positive <- select(city_test_positive, -c(city_test))
        
        jnastr240 <- rbind(jnastr240, leningrad, city_test_positive)
      }
    }
    
    leningrad <- filter(jnastr240, region == "Московская")
    if (nrow(leningrad) > 0) {
      jnastr240 <- filter(jnastr240, region != "Московская")
      leningrad$city_test <- grepl("Москва", leningrad$city)
      city_test_positive <- filter(leningrad, city_test == TRUE)
      if (nrow(city_test_positive) > 0) {
        leningrad <- filter(leningrad, city_test == FALSE)
        leningrad <- select(leningrad, -c(city_test))
        city_test_positive$region <- "Москва"
        city_test_positive <- select(city_test_positive, -c(city_test))
        
        jnastr240 <- rbind(jnastr240, leningrad, city_test_positive)
      }
    }
    #prepare new St.Petersburg streets
    new_stp <- filter(jnastr240, region == "Санкт-Петербург")
    
    #extract them from the parsed addresses
    jnastr240 <- filter(jnastr240, region != "Санкт-Петербург")
    
    #delete poorly parsed addresses
    unparsable_add <- filter(new_stp, street == "Санкт-Петербург")
    #prepare them for rbind
    unparsable_add$to_maps..duplicated.to_maps.... <- unparsable_add$city
    unparsable_add <- select(unparsable_add, to_maps..duplicated.to_maps....)
    unparsable <- rbind(unparsable, unparsable_add)
    new_stp <- filter(new_stp, street != "Санкт-Петербург")
    new_stp <- select(new_stp, -c(X))
    
    #upload the database of St.Petersburg street names
    stpetersburgstreets <- read.csv2("stpetersburgstreets.csv")
    stpetersburgstreets <- dplyr::select(stpetersburgstreets, region,	city,	street,	latitude,	longitude)
    
    #add new streets
    stpetersburgstreets <- rbind(stpetersburgstreets, new_stp)
    write.csv2(stpetersburgstreets, file = "stpetersburgstreets.csv")
    
    #repeat the process with Moscow
    new_stp <- filter(jnastr240, region == "Москва")
    jnastr240 <- filter(jnastr240, region != "Москва")
    
    unparsable_add <- filter(new_stp, street == "Москва")
    unparsable_add$to_maps..duplicated.to_maps.... <- unparsable_add$city
    unparsable_add <- select(unparsable_add, to_maps..duplicated.to_maps....)
    unparsable <- rbind(unparsable, unparsable_add)
    
    new_stp <- filter(new_stp, street != "Москва")
    new_stp <- select(new_stp, -c(X))
    
    #upload the database of Moscow street names
    moscowstreets <- read.csv2("moscowstreets.csv")
    moscowstreets <- dplyr::select(moscowstreets, region,	city,	street,	latitude,	longitude)
    
    moscowstreets <- rbind(moscowstreets, new_stp)
    write.csv2(moscowstreets, file = "moscowstreets.csv")
    
    regions <- read.csv2("~/Downloads/regions.csv")
    
    #delete last 2 letters in region names
    jnastr240$region <- substr(jnastr240$region,1,nchar(jnastr240$region)-2)
    #make empty strings NAs
    jnastr240$region <- gsub("^$|^ $", NA, jnastr240$region)
    #form a vector of region names without last two letters
    region_shortened <- na.omit(jnastr240$region)
    #NAs to unparsables
    jnastr240$street <- is.na(jnastr240$region)
    
    unparsable_add <- filter(jnastr240, street == TRUE)
    
    #get rid of NAs
    jnastr240 <- na.omit(jnastr240)
    #set an empty dataframe for the regions vector
    defined_regions <- head(stpetersburgstreets, n=0)
    defined_regions <- select(defined_regions, region, street)
    
    if (length(region_shortened) > 1) {
      
      #define the region
      for (i in 1:length(region_shortened)) {
        regions$street <- region_shortened[i]
        regions$detector <- grepl(regions$street, regions$region)
        the_region <- filter(regions, detector == TRUE)
        the_region <- select(the_region, -c(detector))
        defined_regions <- rbind(defined_regions, the_region)
      }
      
      #add parsed region to the complete region dataframe
      jnastr240$region <- defined_regions$region
      
    }
    
    unrecognized_addresses <- select(unparsable_add, X, city)
    
    jnastr240 <- select(jnastr240, region, city, street, latitude, longitude)
    
    unparsable_add$to_maps..duplicated.to_maps.... <- unparsable_add$city
    unparsable_add <- select(unparsable_add, to_maps..duplicated.to_maps....)
    unparsable <- rbind(unparsable, unparsable_add)
    write.csv2(unparsable, file = "unparsable.csv")
    
    #define the cities
    citiestest <- read.csv2("citiesnew.csv")
    citiestest <- dplyr::select(citiestest, region,	city,	street,	latitude,	longitude)
    jnastr240$street <- jnastr240$city
    
    #add new cities to the complete cities dataframe
    citiestest <- rbind(citiestest, jnastr240)
    write.csv2(citiestest, file = "citiesnew.csv")
    
    #add defined addresses to current dataframe
    to_maps <- select(to_maps, -c(X))
    to_maps <- left_join(to_maps, unrecognized_addresses, by = "city")
    
    to_maps$addresses <- to_maps$to_maps..duplicated.to_maps....
    to_maps$success <- is.na(to_maps$X)
    to_maps <- select(to_maps, -c(to_maps..duplicated.to_maps...., X))
    
    nodeparture <- left_join(nodeparture, to_maps, by = "addresses")
    nodestination <- left_join(nodestination, to_maps, by = "addresses")
    
    nodeparture <- rbind(nodeparture, nodestination)
    
  } else {
    nodeparture$success <- FALSE
  }
  return(nodeparture)
}

withdeparture <- function(input, output, session, nodeparture_second, withdeparture_one) {  
  to_withdeparture <- filter(nodeparture_second(), success == TRUE)
  if (nrow(to_withdeparture) > 0) {
    to_withdeparture <- select(to_withdeparture, X, region,	city,	street,	latitude,	longitude)
    withdeparture <- select(withdeparture_one(), X, region,	city,	street,	latitude,	longitude)
    withdeparture <- rbind(withdeparture, to_withdeparture)
  } else {
    withdeparture <- withdeparture_one()
  }
  return(withdeparture)
}

departure <- function(input, output, session, withdeparture_and_destination) {  
  departure_to_join <- filter(withdeparture_and_destination(), X >= 1)
  return(departure_to_join)
}

destination <- function(input, output, session, withdeparture_and_destination) {  
  destination_to_join <- filter(withdeparture_and_destination(), X < 1)
  return(destination_to_join)
}