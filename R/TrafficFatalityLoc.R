library(ggplot2)

#' Stacked Area Chart showing the Locations of Driver Fatalities
#'
#' Creates a Stacked Area Chart visualizing the Locations of driver fatalities.
#' @return Stack Area Chart of Driver Fatality Locations
#' @export
LocationSAC <- function() {
  TFData = read.csv("data/TrafficFatalities.txt", TRUE, ",")
  TFData$Location = factor(TFData$Location, levels=levels(TFData$Location)[c(2,4,1,3)])
  p <- ggplot(TFData, aes(x=TFData$Year, y=TFData$Value, fill=TFData$Location)) +
    geom_area()
  return(p)
}

#' Pie Chart showing the Locations of Driver Fatalities
#'
#' Creates a Pie Chart visualizing the Locations of driver fatalities.
#' @return Pie Chart of Driver Fatality Locations
#' @export
FatalityPie <- function() {
  TFData = read.csv("data/TrafficFatalities.txt", TRUE, ",")
  x <- reshape(TFData, timevar="Year", idvar=c("Location"), direction="wide")
  b <- sum(x[2,2:11])
  c <- sum(x[3,2:11])
  d <- sum(x[4,2:11])
  slices <- c(b, c, d)
  pie(slices, labels = x$Location[2:4], main="Fatal Traffic Accident Locations", radius=1.09, col=rainbow(length(x$Location)))
}

#' Line Graph showing the Locations of Driver Fatalities
#'
#' Creates a Line Graph visualizing the Locations of driver fatalities.
#' @return Line Graph of Driver Fatality Locations
#' @export
LocationLine <- function() {
  TFData = read.csv("data/TrafficFatalities.txt", TRUE, ",")
  p <- ggplot(TFData, aes(x=TFData$Year, y=TFData$Value, group=TFData$Location)) +
    geom_line(aes(color=TFData$Location))
  return(p)
}
