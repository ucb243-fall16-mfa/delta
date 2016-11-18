### Import the data needed for testing
wines <- read.csv('wines.csv')
wines <- wines[, 2:(ncol(wines) - 4)]   # Get rid of the last 4 columns
g <- c(6, 6, 6, 5, 6, 5, 4, 6, 5, 4)
sets <- list(1:6, 7:12, 13:18, 19:23, 24:29, 30:34, 35:38, 39:44, 45:49, 50:53)
