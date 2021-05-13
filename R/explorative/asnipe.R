

## first load the package

library(asnipe)



## define group memberships (these would be read from a file)
individuals <- data.frame(ID=c("C695905","H300253","H300253",
                               "H300283","H839876","F464557","H300296","H300253",
                               "F464557","H300296","C695905","H300283","H839876"),
                          GROUP=c(1,1,2,2,2,3,3,4,5,5,6,6,6))

## create a time column
individuals <- cbind(individuals,
                     DAY=c(1,1,1,1,1,2,2,2,3,3,3,3,3))

gbi <- get_group_by_individual(individuals, 
                               data_format="individuals")

## define group memberships (these would be read from a file)
groups <- list(G1=c("C695905","H300253"),
               G2=c("H300253","H300283","H839876"),
               G3=c("F464557","H300296"),
               G4=c("H300253"),
               G5=c("F464557","H300296"),
               G6=c("C695905","H300283","H839876"))

## create a time variable
days <- c(1,1,2,2,3,3)

gbi <- get_group_by_individual(groups, 
                               data_format="groups")


data("group_by_individual")
data("times")

# subset GBI (to reduce run time of the example)
gbi <- gbi[,1:80]

## define to 2 x N x N network to hold two association matrices
networks <- array(0, c(2, ncol(gbi), ncol(gbi)))

## calculate network for first half of the time
networks[1,,] <- get_network(gbi, data_format="GBI",
                             association_index="SRI", times=times, start_time=0, 
                             end_time=max(times)/2)
networks[2,,] <- get_network(gbi, data_format="GBI",
                             association_index="SRI", times=times, 
                             start_time=max(times)/2, end_time=max(times))

## test if one predicts the other via a mantel test (must be loaded externally)
library(ape)
mantel.test(networks[1,,],networks[2,,])

## convert to igraph network and calculate degree of the first network
## Not run: 
library(igraph)
net <- graph.adjacency(networks[1,,], mode="undirected", diag=FALSE, weighted=TRUE)
deg_weighted <- graph.strength(net)
detach(package:igraph)


## alternatively package SNA can use matrix stacks directly
library(sna)
deg_weighted <- degree(networks,gmode="graph", g=c(1,2), ignore.eval=FALSE)
detach(package:sna)











