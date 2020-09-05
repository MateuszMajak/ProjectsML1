accuracy_multinom <- function(predicted, real) {
  ctable_m <- table(predicted, 
                    real)
    # accuracy
    accuracy <- (100 * sum(diag(ctable_m)) / sum(ctable_m))
    # balanced accuracy
    base_ <- diag(ctable_m) / colSums(ctable_m)
    balanced_accuracy <- mean(100 * ifelse(is.na(base_), 0, base_))
    # balanced correctly predicted
    base_2 <- diag(ctable_m) / rowSums(ctable_m)
    correctly_predicted <- mean(100 * ifelse(is.na(base_2), 0, base_2))
  
  return(c(accuracy = accuracy, 
           balanced_accuracy = balanced_accuracy,
           balanced_correctly_predicted = correctly_predicted))
}