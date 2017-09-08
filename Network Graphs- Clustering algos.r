 data <- content(httr::GET(url = access_url, add_headers ("Content-Type" = "application/json", "X-Auth-Token" = x_subject_token)), as="text")
    textConnection(data)
}

edges15 <- read.csv(file = getObjectStorageFileWithCredentials_("new", "edges15.csv"))
nodes15 <- read.csv(file = getObjectStorageFileWithCredentials_("new", "nodes15.csv"))
nodes14 <- read.csv(file = getObjectStorageFileWithCredentials_("new", "nodes14.csv"))   
edges14 <- read.csv(file = getObjectStorageFileWithCredentials_("new", "edges14.csv"))


#required libraries
install.packages("networkD3")
library(networkD3)
library(igraph)

#----------------------------------------------------------------------
#import graph edges and nodes as csv
#----------------------------------------------------------------------
edges15 <- read.csv('/resources/data/edges15.csv', header =T, as.is =T)
nodes15 <- read.csv('/resources/data/nodes15.csv', header =T, as.is =T)
#-

#View data imported correctly
head(edges15)
head(nodes15)

#convert raw graph data to a igraph object for use later
network15 <- graph_from_data_frame(d=edges15, vertices = nodes15, directed =T)
class(network15)
#----------------------------------------------------------------------------

#plot number 1 - simple interactive igraph plot
edges <- read.csv('/resources/data/edges14.csv', header=T, as.is=T)
head(edges)

#----------------------------------------------------------------------------
#create the data frame with source edges and destination nodes

network <- (data.frame(edges$SOURCE.NODE,edges$TARGET.NODE))

#Creates a simple network graph of connected nodes
simpleNetwork(network, Source = NULL, Target = NULL, height = NULL,
              width = NULL, linkDistance = 300, charge = -200, fontSize = 12,
              fontFamily = "serif", linkColour = "#666", nodeColour = "#3182bd",
              nodeClickColour = "#E34A33", textColour = "#E34A33", opacity = 0.9,
              zoom = T)
#plot "network" graph
simpleNetwork(network)
#--------------------------------------------------------------------------
#1. cluster edge betweeness
cdb <- cluster_edge_betweenness(network15)
class(cdb)
length(cdb)
membership(cdb)
plot(cdb,network15, col = membership(cdb))
#-------------------------------------------------------------------------

### 2. Progating Labels : Group/Community detection method 1

#Group/Community detection based on propgating labels method 1

proplab <- cluster_label_prop(network15)
proplab[]
#plot results in graph
plot(proplab, network15)

#Group/Community detection - Label prop method 2
proplab2 <- label.propagation.community (network15, weights = NULL,
                             initial = NULL, fixed = NULL)
proplab


#Group/Community detection based on walk-trap clustering algo
walk <- walktrap.community(network15, weights = E(network15)$weight, steps = 4, merges =
                     TRUE, modularity = TRUE, membership = TRUE)
walk[]

#--------------------------------------------------------------
#To plot the detected communities converting igraph object 
#to networkD3 object i.e edges/nodes as list of numbers in order 
#to create force network
#---------------------------------------------------------------

#call class function to verify "igraph" object
class(network15)
network15_D3 <- igraph_to_networkD3(network15)

#call class function to verify "list" object
class(network15_D3)
#view new networkD3 converted elements
network15_D3

### Force Network Graph
#create a force network graph
#need to include group col to dataset 
networkF <- forceNetwork(Links = network15_D3$links, Nodes = network15_D3$nodes,
                         Source = 'source', Target = 'target', NodeID = 'name', Group = 'Group',
                         fontSize = '8', linkDistance = 200, bounded =T, opacityNoHover = T)

forceNetwork(network15_D3)