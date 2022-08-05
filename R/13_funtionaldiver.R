#From taxonomical to functional
#and phylogenetic diversity in R
#Andrea Sanchez
#04/08/2022


comm <- read.csv("data/raw/cestes/comm.csv")
traits <- read.csv("data/raw/cestes/traits.csv")
head(comm[,1:6])
head(traits)[,1:6]

rownames(comm)[1:6]
#Putting the name Site in the first columm
rownames(comm) <- paste0("Site", comm[,1])
comm <- comm[,-1]
head(comm)[,1:6]

?rownames
#Transform column traits$Sp into the rownames of the dataframe.
head(traits)[,1:6]
rownames(traits) <- paste0("", traits[,1])
traits <- traits[,-1]
head(traits) [,1:6]

#Species richness can be calculated with the vegan package:
#library(vegan)
#Shannon
richness <- vegan::specnumber(comm)
shannon <- vegan::diversity(comm)
simpson <- vegan::diversity(comm, index = "simpson")

##Functional diversity
##These traits can be continuous, but vary in different scales, or they can be
##categorical, and appropriate distance measures have to be used to deal with this
##difference. Gower distance is a common distance metric used in trait-based ecology.

#library(cluster)
#library(FD)
gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)

#implementations in R vary and the literature reports extensions and modifications
#not the same but why?
identical(gow, gow2)

#different classes
class(gow)
class(gow2)

plot(gow, gow2, asp = 1) #same values

#Rao’s quadratic entropy calculations in R
#library(SYNCSA)
tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)
#plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
#abline(a = 0, b = 1)

#Calculating FD (funtional diversity) indices with package PD
#library(FD)
#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
#the returned object has Villéger's indices and Rao calculation
names(FuncDiv1)
head(FuncDiv)

#We can also do the calculation using the traits matrix directly
FuncDiv <- dbFD(x = traits, a = comm, messages = F)


##How to we summarize visually, interpret community composition and trait data?

splist <- read.csv("data/raw/cestes/splist.csv")
#library(taxine)
