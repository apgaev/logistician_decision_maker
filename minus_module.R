a <- function(input, output, session, selected_dt, parent_column_names, input_name) {
  selected_dt <- selected_dt()
  parent_column_names <- unlist(strsplit(parent_column_names(), split=", "))
  for (i in 1:length(parent_column_names)) {
    selected_dt <- bind_cols(selected_dt, (selected_dt[which(colnames(selected_dt) == parent_column_names[i])]))
  }
  positions <- 1:(ncol(selected_dt)-length(parent_column_names))
  
  withfilters <- selected_dt[,(ncol(selected_dt)-length(parent_column_names)+1):ncol(selected_dt)]
  selected_dt <- select(selected_dt, positions)
  withfilters$minusminus <- withfilters[1]-withfilters[2]
  if (ncol(withfilters)>3) {
    for (i in 3:(ncol(withfilters)-1)) {
      withfilters$minusminus <- withfilters$minusminus-withfilters[i] 
    }
  }
  selected_dt$minusminus <- as.matrix(withfilters$minusminus)
  colnames(selected_dt)[ncol(selected_dt)] <- 
    input_name()
  return(selected_dt)
}