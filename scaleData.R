scaleData <- function(data) {
  
  data.scaled <- as.data.frame(data)
  
  for(column in 1:ncol(data)) {
    min <- min(data[, column])
    max <- max(data[, column])
    data.scaled[, column] <- scale(data[, column]
                                   , center = min
                                   , scale = max - min)
  }
  
  data.scaled
}

scaleBackData <- function(data, min.price, max.price) {
  
  data.scaledBack <- data*(max.price-min.price)+min.price
  
  data.scaledBack
}