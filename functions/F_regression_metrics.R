regressionMetrics <- function(real, predicted) {
  # Mean Squera Error
  MSE <- mean((real - predicted)^2)
  # Root Mean Square Error
  RMSE <- sqrt(MSE)
  # Mean Absolute Error
  MAE <- mean(abs(real - predicted))
  # Median Absolute Error
  MedAE <- median(abs(real - predicted))
  # Mean Logarithmic Absolute Error
  MSLE <- mean((log(1 + real) - log(1 + predicted))^2)
  # Total Sum of Squares
  TSS <- sum((real - mean(real))^2)
  # Explained Sum of Squares
  RSS <- sum((predicted - real)^2)
  # R2
  R2 <- 1 - RSS/TSS
  
  result <- data.frame(MSE, RMSE, MAE, MedAE, MSLE, R2)
  return(result)
}
