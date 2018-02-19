"=========================================INIT=================================================="
packages <- c('igraph','geosphere','plyr')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)  
}

for(package in packages){
  require(package, character.only = TRUE)
}

#geosphere package
"ftp://cran.r-project.org/pub/R/web/packages/geosphere/geosphere.pdf"

setwd("D:\\Google Drive\\masters\\UHD MSDA\\CS6390\\Assignment\\1")
"==========================================3===================================================="

"3.	Download the Wikipedia voting network 
wiki-Vote.txt.gz: http://snap.stanford.edu/ data/wiki-Vote.html. 
A description of the network is also included in the link. 
Use network analysis library igraph in R to perform the following tasks."

a1.q3.data <- read.table(gzfile("wiki-Vote.txt.gz"))
dim(a1.q3.data)
head(a1.q3.data)


"3.1.	The number of nodes and edges in the network."
"a vertex (plural vertices) or node"

a1.q3.g <- graph.data.frame(a1.q3.data)

#number of nodes
vcount(a1.q3.g)
#7115

#number of edges
ecount(a1.q3.g)
#103689

"3.2.	The number of nodes of zero out-degree."

a3.2 <- degree(a1.q3.g, v = V(a3.g), mode = c("out"), loops = TRUE, normalized = FALSE)
a3.2[which(a3.2 == 0)]
length(which(a3.2 == 0))

"3.3.	The number of nodes of zero in-degree."
a3.3 <- degree(a1.q3.g, v = V(a1.q3.g), mode = c("in"), loops = TRUE, normalized = FALSE)

a3.3[which(a3.3 == 0)]
length(which(a3.3 == 0))

"3.4.	The number of nodes with more than 10 outgoing edges (out-degree > 10)."

a3.4 <- degree(a3.g, v = V(a3.g), mode = c("out"), loops = TRUE, normalized = FALSE)

a3.4[which(a3.4 > 10)]
length(which(a3.4 > 0))

"3.5.	The number of nodes with more than 10 incoming edges (in-degree > 10)."

a3.5 <- degree(a1.q3.g, v = V(a1.q3.g), mode = c("in"), loops = TRUE, normalized = FALSE)

a3.5[which(a3.5 > 10)]
length(which(a3.5 > 0))


"3.6.	Plot the distribution of out-degrees of nodes in the network on a log-log scale. 
Each data point is a pair (x, y) where x is a positive integer and y is the number of nodes
in the network with out-degree equal to x. 
Restrict the range of x between the minimum and maximum out-degrees. 
You may filter out data points with a 0 entry. 
For the log-log scale, use base 10 for both x and y axes."

a3.6 <- degree(a1.q3.g, v = V(a1.q3.g), mode = c("out"), loops = TRUE, normalized = FALSE)
a3.6 <- as.data.frame(table(a3.6))

#Function to plot the degree distribution of the nodes
plotDegree <- function(iGraphObject,networkName)
{
  igrah_degree <- degree(iGraphObject)
  #degree of each nodes
  igrah_degree
  degree.hist <- hist(igrah_degree,breaks=c(0:max(igrah_degree)))
  degree.hist.x <- degree.hist$breaks[-1]
  ind <- (degree.hist$counts != 0)
  plot(degree.hist.x[ind], degree.hist$counts[ind], log="xy", col="blue",xlab=c("Log-Degree"), ylab=c("Log-Intensity"),main=paste(networkName,"Log-Log DegreeDistribution"))
}
plotDegree(a1.q3.g,'Wiki network')

"==============================================================================================="

"============================================4=================================================="
#Analysis of Airlines network [45 points]
"4.	You are given two data frames: Flights.txt, Airports.txt. 
Airports.txt contains information for each airport (nodes) in the network. 
The first column is the airport name, second, and third columns are 
longitudes (x) and latitudes (y) locations of the airports, respectively.  
The Flights.txt contains the information of direct flights between two airports. 
Each row indicates a direct flight between two airports (first two columns).  
The third column in the file indicates how much the flight costs."

a1.q4.data.airports <- read.table("Airports.txt")
a1.q4.data.flights <- read.table("Flights.txt")


"4.1.	Write an R function PossibleFlights, 
that will find all possible paths (allow at most 3 stops) to go from airport 1 to airport2. 
Test your function for some example airports in the network. [10 points]"

#airlinesNetwork: this is the network consisting of all the airports.

a1.q4.g.flights <- graph.data.frame(a1.q4.data.flights)

PossibleFlights <- function (airlinesNetwork, airport1, airport2,maxPossiblePath)
{
  print("retrieving all simple paths")
  res <- all_simple_paths(a1.q4.g.flights,to=airport1, from=airport2, mode = "all", maxPossiblePath)
  for(i in res){
    if(length(i) <=maxPossiblePath)
      print(i)
  }
}

#sample call
maxPossiblePath <- 3
#PossibleFlights(a1.q4.g.flights,"","",maxPossiblePath)


"4.2.	Write an R function CheapestFlight that will find the cheapest possible way
(display the path and the cost associated with the path) to go from airport 1 to airport2. 
Here, assume that you don't care about the number of stops. 
You will need to make the weight of an edge as the cost of the flight represented by the edge
(given in data frame Flights.txt).  
Test your function by using some example airports in the network.[15 points]"

#airlinesNetwork: this is the network consisting of all the airports.

a1.q4.2.data <- data.frame(from=a1.q4.data.flights$V1, to=a1.q4.data.flights$V2, weight=a1.q4.data.flights$V3)
a1.q4.2.g <- graph.data.frame(a1.q4.2.data)

CheapestFlight <- function (airlinesNetwork, airport1, airport2)
{
  shortest_paths(airlinesNetwork, from=airport1, to = airport2, mode = "all",output = "both")
  distances(airlinesNetwork, mode="out")[airport1,airport2]
}

#sample call
#CheapestFlight(a1.q4.2.g,'DCA','PSC')


"4.3.	Write an R function ShortestFlight that will find the shortest possible way
(display the path and the distance associated with the path) to go from airport 1 to airport2. 
Here assume that you don't care about the cost of the flight. 
You will need to make the weight of an edge connecting two airports as the distance 
between the two airports. The distance can be calculated using the latitude and longitude
information of the airports given in Airports.txt 
(simply use Euclidean distance as the distance).
Test your function by using some example airports in the network. [20 points]"

#airlinesNetwork: this is the network consisting of all the airports.

#prepare distance
a1.q4.data.flights.distance <- data.frame()
for(row in 1:nrow(a1.q4.data.flights)){
  dist <- distm(c(a1.q4.data.airports[a1.q4.data.airports$V1 %in% a1.q4.data.flights[row,][[1]],][[2]], a1.q4.data.airports[a1.q4.data.airports$V1 %in% a1.q4.data.flights[row,][[1]],][[3]]), c(a1.q4.data.airports[a1.q4.data.airports$V1 %in% a1.q4.data.flights[row,][[2]],][[2]], a1.q4.data.airports[a1.q4.data.airports$V1 %in% a1.q4.data.flights[row,][[2]],][[3]]), fun = distHaversine)
  a1.q4.data.flights.distance <- rbind(a1.q4.data.flights.distance,c(count,dist))
  
}
a1.q4.data.flights.withdistance <- cbind(a1.q4.data.flights, distance =a1.q4.data.flights.distance$X826464.295321042)

#build 

a1.q4.3.data.graph <- data.frame(from=a1.q4.data.flights.withdistance$V1, to=a1.q4.data.flights.withdistance$V2, weight=a1.q4.data.flights.withdistance$distance)
a1.q4.3.graph <- graph.data.frame(a1.q4.3.data.graph)

ShortestFlight <- function(airlinesNetwork,airport1, airport2)
{
  shortest_paths(airlinesNetwork, from=airport1, to = airport2, mode = "all",output = "both")
  distances(airlinesNetwork, mode="out")[airport1,airport2]
}

#sample call 

ShortestFlight(a1.q4.3.graph,"ALB","EWR")

"==============================================================================================="
