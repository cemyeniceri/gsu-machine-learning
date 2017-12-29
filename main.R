library('nnet')

setwd("D:/Development/workspace/ML")
source("scaleData.R")

autos <- read.csv("D:/Development/workspace/ML/autos.csv")

## dateCrawled, name, abtest, vehicleType, monthOfRegistration, dateCreated, numberOfPictures, postalCode, lastSeen
deletedColumns <- c(1, 2, 6, 7, 13, 17, 18, 19, 20)
cleanData <- autos[, -deletedColumns]

## Exclude cars gewerblich -> galeri
rowIndices <- which(cleanData$seller == 'gewerblich')
cleanData <- cleanData[-rowIndices, ]
row.names(cleanData) <- NULL

## Exclude cars gesuch -> alici
rowIndices <- which(cleanData$offerType == 'Gesuch')
cleanData <- cleanData[-rowIndices, ]
row.names(cleanData) <- NULL

# Exclude cars if price is higher than 60k or lower than 1K euro
rowIndices <- which(cleanData$price < 1000 | cleanData$price > 60000)
cleanData <- cleanData[-rowIndices, ]
row.names(cleanData) <- NULL

# Exclude cars if brand is sonstige_autos
rowIndices <- which(cleanData$brand == 'sonstige_autos')
cleanData <- cleanData[-rowIndices, ]
row.names(cleanData) <- NULL

# Exclude cars if powerPs is higher than 700 
rowIndices <- which(cleanData$powerPS >= 700)
cleanData <- cleanData[-rowIndices, ]
row.names(cleanData) <- NULL

# Call the yearOfRegistrationFilter function to find out the exact limits for the year
## yearOfRegistrationFilter(cleanData)

# Exclude cars if year of registration is higher than 2015 or lower than 1995
rowIndices <- which(cleanData$yearOfRegistration > 2015 | cleanData$yearOfRegistration < 1995 )
cleanData <- cleanData[-rowIndices, ]
row.names(cleanData) <- NULL

## Delete the prices with recurring values
rowIndices <- grep("\\b(\\d)\\1+\\b", cleanData$price)
cleanData <- cleanData[-rowIndices, ]
row.names(cleanData) <- NULL

## seller, offerType
deletedColumns <- c(1, 2)
cleanData <- cleanData[, -deletedColumns]

# Create a new modelData specifically chosen by the user
brand <- "volkswagen"
model <- "golf"
fuel <- "benzin"
gearBox <- "manuell"
notRepairedDamage <- "ja"
modelIndices <- which(cleanData$brand == brand & cleanData$model == model & cleanData$gearbox == gearBox 
                      & cleanData$fuelType == fuel & cleanData$notRepairedDamage == notRepairedDamage)
modelData <- cleanData[modelIndices, c(1, 2, 4, 6)]

# Scale data to (0,1) range for each column
modelData.scaled <- scaleData(modelData)
max.price <- max(modelData$price)
min.price <- min(modelData$price)


x <- modelData.scaled[,2:4]
# Fit model
fit <- nnet(price~., modelData.scaled, size=100, maxit=1000, linout=T)

# Make predictions
predictions <- predict(fit, x, type="raw")

# Scale back the predictions to actual price range
predictions.scaledBack <- scaleBackData(predictions, min.price = min.price, max.price = max.price)
cbind(modelData$price, predictions.scaledBack)

totalError <- abs(modelData$price - predictions.scaledBack)
meanError <- mean(totalError)
minError <- min(totalError)
maxError <- max(totalError)

totalPercentageError <- abs((modelData$price - predictions.scaledBack)/modelData$price * 100)
meanPercentageError <- mean(totalPercentageError)
minPercentageError <- min(totalPercentageError)
maxPercentageError <- max(totalPercentageError)


totalError
meanError
minError
maxError

totalPercentageError
meanPercentageError
minPercentageError
maxPercentageError
