get_id <- function(full_table){
  table_evday <- full_table[1]
  for (i in 2:length(full_table)){
    table_evday <- merge(x = table_evday, y = full_table[i], by = "id", suffixes = c(i - 1, i))
  }
  
  temps <- table_evday[-1]
  days_count <- length(temps)
  people_ev <- nrow(table_evday)
  mean_temps <- vector(length = people_ev, mode = "double")
  for (j in 1:people_ev) {
    sum <- 0
    for (temp in temps) {
      sum <- sum + temp[j]
    }
    mean_temps[j] <- sum / days_count
  }
  
  return (data.frame(id = table_evday[1], mean_temp = mean_temps))
}


