library(ggplot2)
library(reshape2)

#' Grouped Bar Chart showing the Victims of Driver Fatalities
#'
#' Creates a Grouped Bar Chart visualizing the Victims of driver fatalities grouped by year.
#' @return Grouped Bar Chart of Driver Fatality Victims
#' @export
VictimGroupBar <- function() {
  TVData = read.csv("data/TrafficVictims.txt", TRUE, ",", check.names=FALSE)
  x <- melt(TVData, id.vars = "Victim", variable.name="Year", value.name="Value")
  ggplot(x, aes(x=x$Year, y=x$Value, fill=x$Victim)) +
    geom_bar(stat="identity", position = "dodge")
}

#' Line Graph showing the Victims of Driver Fatalities
#'
#' Creates a Line Graph visualizing the Victims of driver fatalities.
#' @return Line Graph of Driver Fatality Victims
#' @export
VictimLine <- function() {
  TVData = read.csv("data/TrafficVictims.txt", TRUE, ",", check.names=FALSE)
  x <- melt(TVData, id.vars = "Victim", variable.name="Year", value.name="Value")
  ggplot(x, aes(x=x$Year, y=x$Value, group=x$Victim)) +
    geom_line(aes(color=x$Victim))
}

#' Bar Chart showing the Victims of Driver Fatalities
#'
#' Creates a Bar Chart visualizing the Victims of driver fatalities.
#' @return Bar Chart of Driver Fatality Victims
#' @export
VictimBar <- function() {
  TVData = read.csv("data/TrafficVictims.txt", TRUE, ",", check.names=FALSE)
  x <- melt(TVData, id.vars = "Victim", variable.name="Year", value.name="Value")
  ggplot(x, aes(x=x$Victim, y=x$Value, fill=x$Victim)) +
    geom_bar(stat="identity")
}
