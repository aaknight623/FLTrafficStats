library(ggplot2)
library(reshape2)

#' Grouped Bar Chart showing Age of Driver Fatalities
#'
#' Creates a Grouped Bar Chart visualizing the ages of driver fatalities grouped by year.
#' @return Grouped Bar Chart of Driver Fatality Ages
#' @export
DriverAgeGroupBar <- function() {
  TAData = read.csv("data/FatalTrafficAge.txt", TRUE, ",", check.names=FALSE)
  x <- melt(TAData, id.vars = "Age", variable.name="Year", value.name="Value")
  ggplot(x, aes(x=x$Year, y=x$Value, fill=x$Age)) +
    geom_bar(stat="identity", position = "dodge")
}

#' Bar Chart showing Age of Driver Fatalities
#'
#' Creates a Bar Chart visualizing the ages of driver fatalities.
#' @return Bar Chart of Driver Fatality Ages
#' @export
DriverAgeBar <- function() {
  TAData = read.csv("data/FatalTrafficAge.txt", TRUE, ",", check.names=FALSE)
  x <- melt(TAData, id.vars = "Age", variable.name="Year", value.name="Value")
  ggplot(x, aes(x=x$Age, y=x$Value, fill=x$Age)) +
    geom_bar(stat="identity")
}

#' Pie Chart showing Age of Driver Fatalities
#'
#' Creates a Pie Chart visualizing the ages of driver fatalities.
#' @return Pie Chart of Driver Fatality Ages
#' @export
DriverAgePie <- function() {
  TAData = read.csv("data/FatalTrafficAge.txt", TRUE, ",", check.names=FALSE)
  a <- sum(TAData[1,2:11])
  b <- sum(TAData[2,2:11])
  c <- sum(TAData[3,2:11])
  d <- sum(TAData[4,2:11])
  slices <- c(a, b, c, d)
  pie(slices, labels = TAData$Age, main="Fatal Traffic Accident Ages", radius=1.09, col=rainbow(length(TAData$Age)))
  legend("bottomright", legend=TAData$Age, fill=rainbow(length(TAData$Age)))
}

#' Line Graph showing Age of Driver Fatalities
#'
#' Creates a Line Graph visualizing the ages of driver fatalities.
#' @return Line Graph of Driver Fatality Ages
#' @export
DriverAgeLine <- function() {
  TAData = read.csv("data/FatalTrafficAge.txt", TRUE, ",", check.names=FALSE)
  x <- melt(TAData, id.vars = "Age", variable.name="Year", value.name="Value")
  ggplot(x, aes(x=x$Year, y=x$Value, group=x$Age)) +
    geom_line(aes(color=x$Age))
}
