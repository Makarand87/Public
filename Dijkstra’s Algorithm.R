library(igraph)

# your data
mat <- as.matrix(read.table(text=
"
node	SL	CH	IN	LV	NV	CI	LX	KV	CO	CL	CN	MT	GR	HB	WA	RI
SL	0	300	245	263	312	0	0	0	0	0	0	0	0	0	0	0
CH	300	0	201	0	0	0	0	0	0	362	0	0	0	0	0	0
IN	245	201	0	114	0	112	0	0	176	0	0	0	0	0	0	0
LV	263	0	114	0	175	0	86	0	0	0	0	0	0	0	0	0
NV	312	0	0	175	0	0	0	180	0	0	0	0	0	0	0	0
CI	0	0	112	0	0	0	95	0	105	0	204	0	0	0	0	0
LX	0	0	0	86	0	95	0	170	0	0	177	0	0	0	0	0
KV	0	0	0	0	180	0	170	0	0	0	0	0	299	0	0	0
CO	0	0	176	0	0	105	0	0	0	142	0	0	0	0	0	0
CL	0	362	0	0	0	0	0	0	142	0	251	201	0	332	0	0
CN	0	0	0	0	0	204	177	0	0	251	0	157	244	0	0	318
MT	0	0	0	0	0	0	0	0	0	201	157	0	0	213	209	0
GR	0	0	0	0	0	0	0	299	0	0	244	0	0	0	0	205
HB	0	0	0	0	0	0	0	0	0	332	0	213	0	0	120	0
WA	0	0	0	0	0	0	0	0	0	0	0	209	0	120	0	111
RI	0	0	0	0	0	0	0	0	0	0	318	0	205	0	111	0
"
, header=T))

# prepare data for graph functions - set NA to zero to indicate no direct edge
nms <- mat[,1]
mat <- mat[, -1]
colnames(mat) <- rownames(mat) <- nms
mat[is.na(mat)] <- 0
mat


# create graph from adjacency matrix
g <- graph.adjacency(mat, weighted=TRUE, add.rownames = "code")
plot.igraph(g)

# Get all path distances
(s.paths <- shortest.paths(g, algorithm = "dijkstra"))



# TSP ---------------------------------------------------------------------


# install.packages("TSP")
library(TSP)
# data("USCA50")
# tours <- solve_TSP(USCA50, "nn")
# tours
# data("iris") 
# d <- dist(iris[-5])
# ## create a TSP 
# tsp <- TSP(d) 
# tsp
# ## use some methods
# n_of_cities(tsp) 
# labels(tsp) 
# image(tsp)

data <- read.csv("TSP.csv", header = T)

rownames(data) <- colnames(data)
data <- as.matrix(data)

tsp <- TSP(data) 

tour <- solve_TSP(tsp, "nn") 
tour 
tour_length(tour)
plot(tour)

tour2 <- solve_TSP(tsp, method="nn", two_opt=TRUE)
tour2
plot(tsp, tour2)
methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion", 
             "arbitrary_insertion", "nn", "repetitive_nn", "two_opt") 

tours <- sapply(methods, FUN = function(m) solve_TSP(tsp, method = m),
                simplify = FALSE)
dotchart(sort(c(sapply(tours, tour_length))), 
         xlab = "tour length")


d <- dist(data)
tsp2 <-  as.TSP(d)
tour3 <- solve_TSP(tsp2, "nn")
image(tsp2, tour3)



# TSP ---------------------------------------------------------------------

setwd("~/")
## Distances between German cities 
dmat <- as.matrix(read.table("./wg22_dist.txt"))

## City names
vn <- as.character(read.table("./wg22_name.txt",skip=2)[[1]])

## XY-coordinates of cities

xycoord <- as.matrix(read.table("./wg22_xy.txt",skip=2))

rownames(dmat) <- colnames(dmat) <- rownames(xycoord)<- vn

# source("tspdata/TSP-fun.R") 
idx <- c(1,3,6,8,10) 
dmat2 <- dmat[idx,idx] 
xycoord2 <- xycoord[idx,] 
dmat2
library(TSP)
tp <- TSP(dmat2)
tour <- solve_TSP(tp, "nn")
tour
plot(tour)
plot(tp)



# IRIS DataSet ------------------------------------------------------------

data("iris") 
tsp <- TSP(dist(iris[-5]), labels = iris[, "Species"]) 
n_of_cities(tsp)
labels(tsp)
image(tsp)

tsp_dummy <- insert_dummy(tsp, n = 3, label = "boundary") 
tour <- solve_TSP(tsp_dummy)

## plot the distance matrix 
image(tsp_dummy, tour, xlab = "objects", ylab ="objects") 
## draw lines where the dummy cities are located 
abline(h = which(labels(tour)=="boundary"), col = "red") 
abline(v = which(labels(tour)=="boundary"), col = "red")
  

out <- rle(labels(tour)) 
data.frame(Species = out$values, 
           Lenghts = out$lengths, 
           Pos = cumsum(out$lengths))

prc <- prcomp(iris[1:4]) 
plot(prc$x, pch = as.numeric(iris[,5]), col = as.numeric(iris[,5])) 
indices <- c(tour, tour[1]) 
indices[indices > 150] <- NA 
lines(prc$x[indices,])
