
#####Libraries used
library(sna)
library(network)
library(RColorBrewer)
library(intergraph)
#library(ergm)
#detach(package:intergraph)

################################################## 1
##### Load countries for the first matrix - 2008
setwd(".../data/2008/CSV")
FriendMat<-read.csv("alliance_20082.csv",header=TRUE, stringsAsFactors=FALSE)
FriendMat<-FriendMat[,2:16]
FriendMat<-as.matrix(FriendMat)
Friend.any <- ifelse(FriendMat > 0, 1, 0)

##### Convert Network
suppressPackageStartupMessages(library(sna))
FriendNet.any<-as.network(Friend.any,directed=FALSE, matrix.type = "adjacency", diag=FALSE)
names <- network.vertex.names(FriendNet.any)

suppressPackageStartupMessages(library(ergm))
suppressPackageStartupMessages(library(coda))
suppressPackageStartupMessages(library(igraph))

#Create Graph to add attributes for ERGM
FriendGraph.any <-graph.adjacency(Friend.any,
                                  mode=c("undirected"),
                                  weighted=NULL,
                                  diag=FALSE)

##Add all attributes for 2008
HR_att<-read.csv("attributes_2008.csv",header=TRUE, stringsAsFactors=FALSE)
names<-HR_att$state
expenditure<-as.numeric(HR_att$milex)/10
personnel<-as.numeric(HR_att$milper)
ironsteel<-as.numeric(HR_att$irst)
primaryenergy<-as.numeric(HR_att$pec)
population<-as.numeric(HR_att$tpop)
urbanp<-as.numeric(HR_att$upop)
compositeindexc<-as.numeric(HR_att$cinc)*1000
Freedom<-as.numeric(HR_att$FHI)*10
BTindex<-as.numeric(HR_att$BTI)*10
import<-as.numeric(HR_att$imports)
export<-as.numeric(HR_att$exports)
energyimport<-as.numeric(HR_att$energyimport)*10
mildependency<-as.numeric(HR_att$militarydep)
nGDP<-as.numeric(HR_att$GDP)
DevIndex<-as.numeric(HR_att$HDI)*10


###### Create vectors to store attributes
expenditure_vector<-vector()
personnel_vector<-vector()
ironsteel_vector<-vector()
primaryenergy_vector<-vector()
population_vector<-vector()
urbanp_vector<-vector()
compositeindexc_vector<-vector()
Freedom_vector<-vector()
BTindex_vector<-vector()
import_vector<-vector()
export_vector<-vector()
energyimport_vector<-vector()
mildependency_vector<-vector()
nGDP_vector<-vector()
DevIndex_vector<-vector()

####### Set of all network nodes
for(i in 1:15){ 
  for(j in 1:15){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(FriendGraph.any)$name[i]==names[j]){
      #if we match, we add the attribute to a vector
      expenditure_vector[i]<-expenditure[j]
      personnel_vector[i]<-personnel[j]
      ironsteel_vector[i]<-ironsteel[j]
      primaryenergy_vector[i]<-primaryenergy[j]
      population_vector[i]<-population[j]
      urbanp_vector[i]<-urbanp[j]
      compositeindexc_vector[i]<-compositeindexc[j]
      Freedom_vector[i]<-Freedom[j]
      BTindex_vector[i]<-BTindex[j]
      import_vector[i]<-import[j]
      export_vector[i]<-export[j]
      energyimport_vector[i]<-energyimport[j]
      mildependency_vector[i]<-mildependency[j]
      nGDP_vector[i]<-nGDP[j]
      DevIndex_vector[i]<-DevIndex[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{
    #  gender_vector[i]<-0
   # age_vector[i]<-0
    }
  }
}

## Set vertex attributes
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'milexpenditure', value=c(expenditure_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'milpersonnel', value=c(personnel_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'ironsteel', value=c(ironsteel_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'primaryenergy', value=c(primaryenergy_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'population', value=c(population_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'urbanp', value=c(urbanp_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'compositeindexc', value=c(compositeindexc_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'Freedom', value=c(Freedom_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'BTindex', value=c(BTindex_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'import', value=c(import_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'export', value=c(export_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'energyimport', value=c(energyimport_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'mildependency', value=c(mildependency_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'GDP', value=c(nGDP_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'HDI', value=c(DevIndex_vector))


## Convert the graph to a network
library(intergraph)
FriendNet.any<-asNetwork(FriendGraph.any)
FriendNet.any


######################################################## 2
##### Load countries for the second matrix - 2014
setwd(".../data/2014/CSV")
FriendMat2<-read.csv("alliance_20142.csv",header=TRUE, stringsAsFactors=FALSE)
FriendMat2<-FriendMat2[,2:16]
FriendMat2<-as.matrix(FriendMat2)
Friend.any2 <- ifelse(FriendMat2 > 0, 1, 0)
suppressPackageStartupMessages(library(sna))
FriendNet.any2<-as.network(Friend.any2,directed=FALSE, matrix.type = "adjacency")
names <- network.vertex.names(FriendNet.any2)

suppressPackageStartupMessages(library(ergm))
suppressPackageStartupMessages(library(coda))


######next add attributes
suppressPackageStartupMessages(library(igraph))
FriendGraph.any2 <-graph.adjacency(Friend.any2,
                                  mode=c("undirected"),
                                  weighted=NULL,
                                  diag=FALSE)
####### Read attributes for2014
HR_att2<-read.csv("attributes_2014.csv",header=TRUE, stringsAsFactors=FALSE)
names<-HR_att2$state
expenditure<-as.numeric(HR_att2$milex)/10
personnel<-as.numeric(HR_att2$milper)
ironsteel<-as.numeric(HR_att2$irst)
primaryenergy<-as.numeric(HR_att2$pec)
population<-as.numeric(HR_att2$tpop)
urbanp<-as.numeric(HR_att2$upop)
compositeindexc<-as.numeric(HR_att2$cinc)*1000
Freedom<-as.numeric(HR_att2$FHI)*10
BTindex<-as.numeric(HR_att2$BTI)*10
import<-as.numeric(HR_att2$imports)
export<-as.numeric(HR_att2$exports)
energyimport<-as.numeric(HR_att2$energyimport)*10
mildependency<-as.numeric(HR_att2$militarydep)
nGDP<-as.numeric(HR_att2$GDP)
DevIndex<-as.numeric(HR_att2$HDI)*10

########## Create vectors to store attributes
expenditure_vector2<-vector()
personnel_vector2<-vector()
ironsteel_vector2<-vector()
primaryenergy_vector2<-vector()
population_vector2<-vector()
urbanp_vector2<-vector()
compositeindexc_vector2<-vector()
Freedom_vector2<-vector()
BTindex_vector2<-vector()
import_vector2<-vector()
export_vector2<-vector()
energyimport_vector2<-vector()
mildependency_vector2<-vector()
nGDP_vector2<-vector()
DevIndex_vector2<-vector()

############### this is our set of all network nodes
for(i in 1:15){ 
  for(j in 1:15){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(FriendGraph.any2)$name[i]==names[j]){
      #if we match, we add the attribute to a vector
      expenditure_vector2[i]<-expenditure[j]
      personnel_vector2[i]<-personnel[j]
      ironsteel_vector2[i]<-ironsteel[j]
      primaryenergy_vector2[i]<-primaryenergy[j]
      population_vector2[i]<-population[j]
      urbanp_vector2[i]<-urbanp[j]
      compositeindexc_vector2[i]<-compositeindexc[j]
      Freedom_vector2[i]<-Freedom[j]
      BTindex_vector2[i]<-BTindex[j]
      import_vector2[i]<-import[j]
      export_vector2[i]<-export[j]
      energyimport_vector2[i]<-energyimport[j]
      mildependency_vector2[i]<-mildependency[j]
      nGDP_vector2[i]<-nGDP[j]
      DevIndex_vector2[i]<-DevIndex[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{
    }
  }
}

############### Set vertex attributes:
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'milexpenditure', value=c(expenditure_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'milpersonnel', value=c(personnel_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'primaryenergy', value=c(primaryenergy_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'population', value=c(population_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'urbanp', value=c(urbanp_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'compositeindexc', value=c(compositeindexc_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'Freedom', value=c(Freedom_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'BTindex', value=c(BTindex_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'import', value=c(import_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'export', value=c(export_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'energyimport', value=c(energyimport_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'mildependency', value=c(mildependency_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'GDP', value=c(nGDP_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'HDI', value=c(DevIndex_vector2))
FriendGraph.any2<-set_vertex_attr(FriendGraph.any2, 'ironsteel', value=c( ironsteel_vector2))

## Convert graph to network
library(intergraph)
FriendNet.any2<-asNetwork(FriendGraph.any2)
FriendNet.any2

######################################################## 3
##### Load countries for the third matrix - 2020
setwd(".../data/2020/CSV")
FriendMat3<-read.csv("alliance_20202.csv",header=TRUE, stringsAsFactors=FALSE)
FriendMat3<-FriendMat3[,2:16]
FriendMat3<-as.matrix(FriendMat3)
Friend.any3 <- ifelse(FriendMat3 > 0, 1, 0)
suppressPackageStartupMessages(library(sna))
FriendNet.any3<-as.network(Friend.any3, directed=FALSE, matrix.type = "adjacency")
names <- network.vertex.names(FriendNet.any3)

suppressPackageStartupMessages(library(ergm))
suppressPackageStartupMessages(library(coda))


###### Next add attributes 
suppressPackageStartupMessages(library(igraph))
FriendGraph.any3 <-graph.adjacency(Friend.any3,
                                  mode=c("undirected"),
                                  weighted=NULL,
                                  diag=FALSE)

##### Read attributes
HR_att3<-read.csv("attributes_2020.csv",header=TRUE, stringsAsFactors=FALSE)
names<-HR_att3$state
expenditure<-as.numeric(HR_att3$milex)/10
personnel<-as.numeric(HR_att3$milper)
ironsteel<-as.numeric(HR_att3$irst)
primaryenergy<-as.numeric(HR_att3$pec)
population<-as.numeric(HR_att3$tpop)
urbanp<-as.numeric(HR_att3$upop)
compositeindexc<-as.numeric(HR_att3$cinc)*1000
Freedom<-as.numeric(HR_att3$FHI)*10
BTindex<-as.numeric(HR_att3$BTI)*10
import<-as.numeric(HR_att3$imports)
export<-as.numeric(HR_att3$exports)
energyimport<-as.numeric(HR_att3$energyimport)*10
mildependency<-as.numeric(HR_att3$militarydep)
nGDP<-as.numeric(HR_att3$GDP)
DevIndex<-as.numeric(HR_att3$HDI)*10

##### Create vectors to store attributes
expenditure_vector3<-vector()
personnel_vector3<-vector()
ironsteel_vector3<-vector()
primaryenergy_vector3<-vector()
population_vector3<-vector()
urbanp_vector3<-vector()
compositeindexc_vector3<-vector()
Freedom_vector3<-vector()
BTindex_vector3<-vector()
import_vector3<-vector()
export_vector3<-vector()
energyimport_vector3<-vector()
mildependency_vector3<-vector()
nGDP_vector3<-vector()
DevIndex_vector3<-vector()

################# this is our set of all network nodes
for(i in 1:15){ 
  for(j in 1:15){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(FriendGraph.any3)$name[i]==names[j]){
      #if we match, we add the attribute to a vector
      expenditure_vector3[i]<-expenditure[j]
      personnel_vector3[i]<-personnel[j]
      ironsteel_vector3[i]<-ironsteel[j]
      primaryenergy_vector3[i]<-primaryenergy[j]
      population_vector3[i]<-population[j]
      urbanp_vector3[i]<-urbanp[j]
      compositeindexc_vector3[i]<-compositeindexc[j]
      Freedom_vector3[i]<-Freedom[j]
      BTindex_vector3[i]<-BTindex[j]
      import_vector3[i]<-import[j]
      export_vector3[i]<-export[j]
      energyimport_vector3[i]<-energyimport[j]
      mildependency_vector3[i]<-mildependency[j]
      nGDP_vector3[i]<-nGDP[j]
      DevIndex_vector3[i]<-DevIndex[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{
    }
  }
}

## Set vertex attributes
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'milexpenditure', value=c(expenditure_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'milpersonnel', value=c(personnel_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'primaryenergy', value=c(primaryenergy_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'population', value=c(population_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'urbanp', value=c(urbanp_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'compositeindexc', value=c(compositeindexc_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'Freedom', value=c(Freedom_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'BTindex', value=c(BTindex_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'import', value=c(import_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'export', value=c(export_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'energyimport', value=c(energyimport_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'mildependency', value=c(mildependency_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'GDP', value=c(nGDP_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'HDI', value=c(DevIndex_vector3))
FriendGraph.any3<-set_vertex_attr(FriendGraph.any3, 'ironsteel', value=c( ironsteel_vector3))

## Convert the graph to a network: 
library(intergraph)
FriendNet.any3<-asNetwork(FriendGraph.any3)


detach(package:igraph)
detach(package:ergm)

################################ Adding Dyadic indicator
#####  NETWORK COVARIATES CONFLICT
############### 
##### 2008
setwd("C:/.../data/2008/CSV")
conflict2008<-read.csv("conflict_matrix_2008.csv",header=TRUE, stringsAsFactors=FALSE)
conflict2008<-as.matrix(conflict2008)
conflict2008.mat <- ifelse(conflict2008 > 0, 1, 0)
conflict2008.net<-as.network(conflict2008.mat,directed=FALSE, matrix.type = "adjacency")
##### 2014
setwd(".../data/2014/CSV")
conflict2014<-read.csv("conflict_matrix_2014.csv",header=TRUE, stringsAsFactors=FALSE)
conflict2014<-as.matrix(conflict2014)
conflict2014.mat <- ifelse(conflict2014 > 0, 1, 0)
conflict2014.net<-as.network(conflict2014.mat,directed=FALSE, matrix.type = "adjacency")
##### 2020
setwd(".../data/2020/CSV")
conflict2020<-read.csv("conflict_matrix_2020.csv",header=TRUE, stringsAsFactors=FALSE)
conflict2020<-as.matrix(conflict2020)
conflict2020.mat <- ifelse(conflict2020 > 0, 1, 0)
conflict2020.net<-as.network(conflict2020.mat,directed=FALSE, matrix.type = "adjacency")

war123 <- list(conflict2008.net,conflict2014.net,conflict2020.net)
war123

######## Additional Attributes - RUSSIAN BASES
setwd(".../data/2008/CSV")
militarydependency2008<-read.csv("troops_matrix_2008.csv",header=TRUE, stringsAsFactors=FALSE)
militarydependency2008<-as.matrix(militarydependency2008)
militarydependency2008.mat <- ifelse(militarydependency2008 > 0, 1, 0)
militarydependency2008.net<-as.network(militarydependency2008.mat,directed=FALSE, matrix.type = "adjacency")

setwd(".../data/2014/CSV")
militarydependency2014<-read.csv("troops_matrix_2014.csv",header=TRUE, stringsAsFactors=FALSE)
militarydependency2014<-as.matrix(militarydependency2014)
militarydependency2014.mat <- ifelse(militarydependency2014 > 0, 1, 0)
militarydependency2014.net<-as.network(militarydependency2014.mat,directed=FALSE, matrix.type = "adjacency")

setwd(".../data/2020/CSV")
militarydependency2020<-read.csv("troops_matrix_2020.csv",header=TRUE, stringsAsFactors=FALSE)
militarydependency2020<-as.matrix(militarydependency2020)
militarydependency2020.mat <- ifelse(militarydependency2020 > 0, 1, 0)
militarydependency2020.net<-as.network(militarydependency2020.mat,directed=FALSE, matrix.type = "adjacency")

mildep.list <- list(militarydependency2008.net,militarydependency2014.net,militarydependency2020.net)

###### Additional Attributes - GEOGRAPHIC CONTIGUITY
setwd(".../data/2020/CSV")
geocontiguity<-read.csv("geographiccontiguity2020.csv",header=TRUE, stringsAsFactors=FALSE)
geocontiguity<-as.matrix(geocontiguity)
geocontiguity.mat <- ifelse(geocontiguity > 0, 1, 0)
geocontiguity.net<-as.network(geocontiguity.mat,directed=FALSE, matrix.type = "adjacency")
geocontiguity.net

###### Additional Attributes - TRADE
setwd(".../data/2008/CSV")
trade2008<-read.csv("trade2008.csv",header=TRUE, stringsAsFactors=FALSE)
trade2008<-as.matrix(trade2008)
trade2008.mat <- ifelse(trade2008 > 0, 1, 0)
trade2008.net<-as.network(trade2008.mat,directed=FALSE, matrix.type = "adjacency")

setwd(".../data/2014/CSV")
trade2014<-read.csv("trade2014.csv",header=TRUE, stringsAsFactors=FALSE)
trade2014<-as.matrix(trade2014)
trade2014.mat <- ifelse(trade2014 > 0, 1, 0)
trade2014.net<-as.network(trade2014.mat,directed=FALSE, matrix.type = "adjacency")

setwd(".../data/2020/CSV")
trade2020<-read.csv("trade2020.csv",header=TRUE, stringsAsFactors=FALSE)
trade2020<-as.matrix(trade2020)
trade2020.mat <- ifelse(trade2020 > 0, 1, 0)
trade2020.net<-as.network(trade2020.mat,directed=FALSE, matrix.type = "adjacency")

trade <- list(trade2008.net,trade2014.net,trade2020.net)


################## Preparing ERGM/TERGM Packages
#### PACKAGES
library("statnet")
library("texreg")
library("xergm")


###### Removing missing data
FriendNet.any<-  handleMissings(FriendNet.any, na = NA, method = "fillmode")
FriendNet.any2<-  handleMissings(FriendNet.any2, na = NA, method = "fillmode")
FriendNet.any3<-  handleMissings(FriendNet.any3, na = NA, method = "fillmode")
war123<-  handleMissings(war123, na = NA, method = "fillmode")
geocontiguity.net <- handleMissings(geocontiguity.net, na = NA, method = "fillmode")
mildep.list <- handleMissings(mildep.list, na = NA, method = "fillmode")

###Creating a list of all 3 networks
allylist <- list(FriendNet.any,FriendNet.any2,FriendNet.any3)



###################### Starting the analysis
##### - Considering different variations

a1 <- btergm(allylist ~ edges + triangles + kstar(2), R = 100)
summary(a1, level = 0.95)

b1 <- btergm(allylist ~ edges + kstar(2), R = 100)
summary(b1, level = 0.95)

modelb <- btergm(allylist ~ edges +  kstar(2)+ triangles
             +    edgecov(war123)
             +     edgecov(geocontiguity.net)
             +     edgecov(mildep.list)
             +     nodecov("energyimport")
             +     nodecov("milpersonnel") 
             +     nodecov("Freedom")
             , R = 100)

summary(modelb, level = 0.95)
interpret(b4, type = "tie", i = 1, j = 1, t = 2)
View(Friend.any)


model2 <- btergm(allylist ~ edges + triangles 
             +     kstar(2) #+gwesp(0, fixed = TRUE)
             +     edgecov(war123)
             +     edgecov(geocontiguity.net)
             +     edgecov(mildep.list)
             , R = 100,parallel = "snow", ncpus = 2)

summary(model2, level = 0.95 )


model3 <- btergm(allylist ~ edges + triangles 
                 +     kstar(2)#+gwesp(0, fixed = TRUE)
                 +     edgecov(war123)
                 +    edgecov(geocontiguity.net)
                 +    edgecov(mildep.list)
                 +     nodecov("Freedom")
                 +     nodecov("BTindex")
                 +     nodecov("HDI")
                 +     absdiff("HDI")
                 +     absdiff("Freedom")
                 , R = 100,parallel = "snow", ncpus = 2)

summary(model3, level = 0.95)


model4 <- btergm(allylist ~ edges + triangles +kstar(2)
                 +     edgecov(war123)
                 +     edgecov(geocontiguity.net)
                 +     edgecov(mildep.list)
                 +     absdiff("milpersonnel")
                 +     nodecov("energyimport")
                 +     nodecov("Freedom")
                 , R = 100,parallel = "snow", ncpus = 2)

summary(model4, level = 0.95)

model <- btergm(allylist ~ edges + triangles #+gwesp(0, fixed = TRUE)
                 +     edgecov(war123)
                 +     edgecov(geocontiguity.net)
                 +     edgecov(mildep.list)
                 +     nodecov("population")
                 , R = 100,parallel = "snow", ncpus = 2)

summary(model, level = 0.95)

########### Goodness-of-Fit tested on each
gof.model <- gof(model)
plot(gof.model)

#######################################################
## Considering dyadic ties, probability of alliances between 
## individual countries

interpret(model, type = "tie", i = 2, j = 1, t = 1)
interpret(model, type = "tie", i = 1, j = 2 )
interpret(model, type = "tie", i = 1, j = 4)
interpret(model, type = "tie", i = 1, j = 5)#geo
interpret(model, type = "tie", i = 1, j = 8)#latv
interpret(model, type = "tie", i = 1, j = 9)#lithuania
interpret(model, type = "tie", i = 1, j = 10)#moldova
interpret(model, type = "tie", i = 1, j = 13)#turkmenista
interpret(model, type = "tie", i = 1, j = 14)#ukraine
interpret(model, type = "tie", i = 1, j = 15)#uzb
interpret(model, type = "tie", i = 2, j = 3)#az&bel
interpret(model, type = "tie", i = 2, j = 4)#estonia
interpret(model, type = "tie", i = 2, j = 5)#azGEO
interpret(model, type = "tie", i = 2, j = 6)#azKazakhstan
interpret(model, type = "tie", i = 2, j = 7)#azKyrgistan
interpret(model, type = "tie", i = 2, j = 8)#azLatvia
interpret(model, type = "tie", i = 2, j = 9)#azLithania
interpret(model, type = "tie", i = 2, j = 10)#azMoldova
interpret(model, type = "tie", i = 2, j = 11)#azRussia
interpret(model, type = "tie", i = 2, j = 12)#azTajikistan
interpret(model, type = "tie", i = 2, j = 13)#azTurkmenistan
interpret(model, type = "tie", i = 2, j = 14)#azUkraine
interpret(model, type = "tie", i = 2, j = 15)#azUzbekistan
interpret(model, type = "tie", i = 3, j = 4)#bel^estonia
interpret(model, type = "tie", i = 3, j = 5)#belGEO
interpret(model, type = "tie", i = 3, j = 8)#belLavtia
interpret(model, type = "tie", i = 3, j = 9)#BelLithuania
interpret(model, type = "tie", i = 3, j = 10)#BelMoldova
interpret(model, type = "tie", i = 3, j = 13)#BelTurkmenistan
interpret(model, type = "tie", i = 3, j = 14)#Bel Ukraine
interpret(model, type = "tie", i = 3, j = 15)#Bel Uzb
 
 EstoniaGeorgia <- interpret(model, type = "tie", i = 4, j = 5)
 EstoniaKaz <- interpret(model, type = "tie", i = 4, j = 6)
 EstoniaKyrg <-interpret(model, type = "tie", i = 4, j = 7)#
 EstoniaMoldova <-interpret(model, type = "tie", i = 4, j = 10)#
 EstoniaRussia <-interpret(model, type = "tie", i = 4, j = 11)#
 EstoniaTajik <-interpret(model, type = "tie", i = 4, j = 12)#
 EstoniaTurkmenistan <-interpret(model, type = "tie", i = 4, j = 13)#
 EstoniaUkraine <-interpret(model, type = "tie", i = 4, j = 14)#
 EstoniaUzbek <-interpret(model, type = "tie", i = 4, j = 15)#
 
 GeorgiaKaz <- interpret(model, type = "tie", i = 5, j = 6)
 GeorgiaKyrg <- interpret(model, type = "tie", i = 5, j = 7)
 GeorgiaLatv <-interpret(model, type = "tie", i = 5, j = 8)#
 GeorgiaLith <-interpret(model, type = "tie", i = 5, j = 9)#
 GeorgiaMoldova <-interpret(model, type = "tie", i = 5, j = 10)#
 GeorgiaRussia <-interpret(model, type = "tie", i = 5, j = 11)#
 GeorgiaTajik <-interpret(model, type = "tie", i = 5, j = 12)#
 GeorgiaTurkmenistan <-interpret(model, type = "tie", i = 5, j = 13)#
 GeorgiaUkraine <-interpret(model, type = "tie", i = 5, j = 14)#
 GeorgiaUzbek <-interpret(model, type = "tie", i = 5, j = 15)#
 
 KazLat <-interpret(model, type = "tie", i = 6, j = 8)#
 KazLith <-interpret(model, type = "tie", i = 6, j = 9)#
 KazMold <-interpret(model, type = "tie", i = 6, j = 10)#
 KazTurkm <-interpret(model, type = "tie", i = 6, j = 13)#
 KazUkr<-interpret(model, type = "tie", i = 6, j = 14)#
 KazUzb <-interpret(model, type = "tie", i = 6, j = 15)#
 
 KyrgLat <-interpret(model, type = "tie", i = 7, j = 8)#
 KyrgLith <-interpret(model, type = "tie", i = 7, j = 9)#
 KyrgMol <-interpret(model, type = "tie", i = 7, j = 10)#
 KyrgTurk <-interpret(model, type = "tie", i = 7, j = 13)#
 KyrgUKr <-interpret(model, type = "tie", i = 7, j = 14)#
 KyrgUzbek <-interpret(model, type = "tie", i = 7, j = 15)#
 
 LatvMold <-interpret(model, type = "tie", i = 8, j = 10)#
 LatvRuss <-interpret(model, type = "tie", i = 8, j = 11)#
 LatvTajik <-interpret(model, type = "tie", i = 8, j = 12)#
 LatvTurk <-interpret(model, type = "tie", i = 8, j = 13)#
 LatvUkr <-interpret(model, type = "tie", i = 8, j = 14)#
 LatvUzbek <-interpret(model, type = "tie", i = 8, j = 15)#
 
 LithMold <-interpret(model, type = "tie", i = 9, j = 10)#
 LithRuss <-interpret(model, type = "tie", i = 9, j = 11)#
 LithTajik <-interpret(model, type = "tie", i = 9, j = 12)#
 LithTurk <-interpret(model, type = "tie", i = 9, j = 13)#
 LithUkr <-interpret(model, type = "tie", i = 9, j = 14)#
 LithUzbek <-interpret(model, type = "tie", i = 9, j = 15)#
 
 MolRuss <-interpret(model, type = "tie", i = 10, j = 11)#
 MolTajik <-interpret(model, type = "tie", i = 10, j = 12)#
 MolTurk <-interpret(model, type = "tie", i = 10, j = 13)#
 MolUkr <-interpret(model, type = "tie", i = 10, j = 14)#
 MolUzb <-interpret(model, type = "tie", i = 10, j = 15)#

 
 RussTurk <-interpret(model, type = "tie", i = 11, j = 13)#
 RussUkr <-interpret(model, type = "tie", i = 11, j = 14)#
 RussUzb <-interpret(model, type = "tie", i = 11, j = 15)#
 
 TajikTurk <-interpret(model, type = "tie", i = 12, j = 13)#
 TajikUkr <-interpret(model, type = "tie", i = 12, j = 14)#
 TajikUzb <-interpret(model, type = "tie", i = 12, j = 15)#

 
 TurkUkr <-interpret(model, type = "tie", i = 13, j = 14)#
 TurkUzb <-interpret(model, type = "tie", i = 13, j = 15)#
 
 UkrUzb <-interpret(model, type = "tie", i = 14, j = 15)#
