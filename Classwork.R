#MAKING FUNCTION

#1. FIRSST MEAN IS CALCULATED, THEN STANDARD DEVIATION IS CALCULATED, FINALLY CV IS CALCULATED

#MAKING FUNCTION OF MEAN


field_value <- c(10, 20, 30) # assigning value

calc_mean <- function(field_value) {
    mean_value <- sum(field_value) / length(field_value)
    return(mean_value)
}
calc_mean(field_value) # mean aayo

#Standard deviation
calc_sd <- function(field_value) {
    sd <- sqrt(sum((field_value - mean(field_value))^2) / (length(field_value) - 1))
    return(sd)
}
calc_sd(field_value)

# CV

calc_cv <- function(field_value) {
    cv <- (calc_sd(field_value) / mean(field_value)) * 100
    return(cv)
}
calc_cv(field_value) # cv
