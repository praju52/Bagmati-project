#MAKING FUNCTION

#1. FIRSST MEAN IS CALCULATED, THEN STANDARD DEVIATION IS CALCULATED, FINALLY CV IS CALCULATED

#MAKING FUNCTION OF MEAN


field_value <- c(10, 20, 30, -30,NA) # assigning value

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
calc_cv(field_value) # for cv

#example 2

workdata<- c(1,2,3)
calc_cv(workdata)
calc_mean(workdata)


#Guard Banako

#field_value <- c(10, 20, 30,"30", -30, NA)

# topic 2 = function and guard , guard le notice dincha ra funcytion banayo
calculate_cv <- function(dbh_cm){
#guard1
if (!is.numeric(dbh_cm)) {
stop("Error: DBH must be numeric(measured in cm)!")
}
 #guard2
if(any(dbh_cm<=0, na.rm= TRUE)){    # any le dbh 0 vanda sano vo vane stop gar vanecham
    stop("Error: DBH must be strictly greaterthan 0cm !")
}
 #guard3
if (any(dbh_cm> 150, na.rm = TRUE)) {
    warning("warning: DBH>150 cm detected. did you record in mm?")
}
result <- (calc_sd(dbh_cm) / mean(dbh_cm)) * 100
return(result)
}


#DATA CLEANING
clean_data <- function(data_to_clean) {
  cleaned_data <- data_to_clean[!is.na(data_to_clean) & data_to_clean > 0]
  return(cleaned_data)
}

data <- clean_data(field_value)


calculate_cv(data)
print(calucalte_cv)


