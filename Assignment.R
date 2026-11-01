dbh <- 28.4
Work <- read.csv("./DATA/Raw/bagmati_raw_inventory.csv")
# Inspect data
head(Work)
head(Work, 2)
nrow(Work)
ncol(Work)
names(Work)
str(Work)


#READING CLASS AND "NA"
Work$Height_m
Work$DBH_cm
class(Work$DBH_cm)
class(Work$Height_m)
class(Work$Species)
typeof(Work$Species)
typeof(Work$DBH_cm)

is.na(Work$Height_m)
is.na(Work$DBH_cm)

# CHECKING
sum(Work$DBH_cm, na.rm = TRUE)
Work$DBH_cm == -99


#inv[inv$Plot_ID == "P17" & inv$Tree_ID == "T203", ]
#FUNCTION
calc_square <- function(x) {
    square<- x^2
return(square)}
#checking formula
calc_square (Work$DBH_cm)

#second formula
calc_mean <- function(x) {
    mean_value <- mean(x, na.rm = TRUE)
    return(round(mean_value, digits = 2))}
#checking formula
calc_mean(Work$DBH_cm)

#third formula
calc_log <- function(x) {
    log<- log(x)
  return(log)}

calc_log(Work$DBH_cm)


#HISTOGRAM
Work$Height_m
Work$DBH_cm
hist(Work$DBH_cm)
hist(Work$Height_m)
plot(Work$DBH_cm,Work$Height_m )

#plot
plot(Work$DBH_cm, Work$Height_m)
plot(Height_m ~ DBH_cm, data = Work)
scatter.smooth(Work$DBH_cm, Work$Height_m)
with(Work, scatter.smooth(Height_m ~ DBH_cm))

#library(ggplot2)
#ggplot(Work, aes(x=DBH_m, y=Height_m))+
#geom_point(color = "red")+
#geom_smooth(se = FALSE, color="black")


hist(data$DBH_cm,
     main = "Histogram of DBH",
     xlab = "DBH (cm)",
     col = "lightgreen",
     border = "black")

abline(v = mean(Work$DBH_cm, na.rm = TRUE),
       col = "red",
       lwd = 2,
       lty = 2)



hist(Work$Height_m,
     main = "Histogram of Tree Height",
     xlab = "Height (m)",
     col = "lightblue",
     border = "black")

abline(v = median(Work$Height_m, na.rm = TRUE),
       col = "blue",
       lwd = 2,
       lty = 2)

#BOXPLOT
boxplot(DBH_cm ~ Stand_ID,
        data = Work,
        main = "DBH Comparison Among Four Stands",
        xlab = "Stand ID",
        ylab = "DBH (cm)",
        col = c("lightgreen", "lightblue",
                "lightpink", "lightyellow"))


#scatterplot
plot(Work$DBH_cm, Work$Height_m,
     main = "DBH vs Tree Height",
     xlab = "DBH (cm)",
     ylab = "Height (m)",
     pch = 19,
     col = "darkgreen")

abline(lm(Height_m ~ DBH_cm, data = Work),
       col = "red",
       lwd = 2)

