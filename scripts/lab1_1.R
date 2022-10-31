

fix_data <- function(data){
  res = list()
  for (i in 1:length(names(data))){
    t = list()
    for (j in 1:length(data[[i]])){
      if (!grepl("[a-zA-Z]", data[[i]][j])){
        t[j] <- as.double(as.character((gsub("\\s", "", data[[i]][j]))))
      }else{
        t[j] <- data[[i]][j]
      }
    }
    res[i] <- list(t)
  }
  names(res) <- names(data)
  
  return(res)
}

