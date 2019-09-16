a <- function(input, output, session, selected_dt, parent_column_names, input_name) {
  selected_dt <- selected_dt()
  parent_column_names <- unlist(strsplit(parent_column_names(), split=", "))
  
  for (i in 1:length(parent_column_names)) {
    selected_dt <- bind_cols(selected_dt, (selected_dt[which(colnames(selected_dt) == parent_column_names[i])]))
  }
  positions <- 1:(ncol(selected_dt)-length(parent_column_names))
  
  withfilters <- selected_dt[,(ncol(selected_dt)-length(parent_column_names)+1):ncol(selected_dt)]
  selected_dt <- select(selected_dt, positions)
  
  withfilters$loglog <- log(withfilters[1])
  selected_dt$loglog <- as.matrix(withfilters$loglog)
  colnames(selected_dt)[ncol(selected_dt)] <- 
    input_name()
  return(selected_dt)
}