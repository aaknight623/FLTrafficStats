library(ggplot2)
library(reshape2)

#' Grouped Bar Chart showing the Causes of Driver Fatalities
#'
#' Creates a Grouped Bar Chart visualizing the causes of driver fatalities grouped by year.
#' @return Grouped Bar Chart of Driver Fatality Causes
#' @export
CauseGroupBar <- function() {
  TCData = read.csv("data/TrafficCause.txt", TRUE, ",", check.names=FALSE)
  x <- melt(TCData, id.vars = "Cause", variable.name="Year", value.name="Value")
  ggplot(x, aes(x=x$Year, y=x$Value, fill=x$Cause)) +
    geom_bar(stat="identity", position = "dodge")
}

#' Line Graph showing the Causes of Driver Fatalities
#'
#' Creates a Line Graph visualizing the causes of driver fatalities.
#' @return Line Graph of Driver Fatality Causes
#' @export
CauseLine <- function() {
  TCData = read.csv("data/TrafficCause.txt", TRUE, ",", check.names=FALSE)
  x <- melt(TCData, id.vars = "Cause", variable.name="Year", value.name="Value")
  ggplot(x, aes(x=x$Year, y=x$Value, group=x$Cause)) +
    geom_line(aes(color=x$Cause))
}

#' Bar Chart showing the Causes of Driver Fatalities
#'
#' Creates a Bar Chart visualizing the causes of driver fatalities.
#' @return Bar Chart of Driver Fatality Causes
#' @export
CauseBar <- function() {
  TCData = read.csv("data/TrafficCause.txt", TRUE, ",", check.names=FALSE)
  x <- melt(TCData, id.vars = "Cause", variable.name="Year", value.name="Value")
  ggplot(x, aes(x=x$Cause, y=x$Value, fill=x$Cause)) +
    geom_bar(stat="identity")
}

#' Pie Chart showing the Causes of Driver Fatalities
#'
#' Creates a Pie Chart visualizing the causes of driver fatalities.
#' @return Pie Chart of Driver Fatality Causes
#' @export
CausePie <- function() {
  TCData = read.csv("data/TrafficCause.txt", TRUE, ",", check.names=FALSE)
  b <- sum(TCData[1,2:11])
  c <- sum(TCData[2,2:11])
  d <- sum(TCData[3,2:11])
  slices <- c(b, c, d)
  pie(slices, labels = TCData$Cause, main="Fatal Traffic Accident Causes", radius=1.09, col=rainbow(length(TCData$Cause)))
}
