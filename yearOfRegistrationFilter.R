yearOfRegistrationFilter <- function(data, brandList=c('ford', 'volkswagen', 'mercedes_benz', 'bmw')) {
  rowIndices <- which(cleanData$yearOfRegistration > 2015 | cleanData$yearOfRegistration < 1950)
  data <- data[-rowIndices, ]
  row.names(data) <- NULL
  for(brand in brandList) {
    brandIndices <- which(data$brand == brand)
    carData <- data[brandIndices, ]
    
    averageList <- list()
    for(year in carData$yearOfRegistration) {
      yearIndices <- which(carData$yearOfRegistration == year)
      average <- mean(carData$price[yearIndices])
      averageList <- unlist(c(averageList, average))
    }
    png(filename = paste0(brand, ".png"))
    plot(carData$yearOfRegistration, averageList, main = brand, xlab = "yearOfRegistration", ylab = "averagePrice")
    dev.off()
  }
}