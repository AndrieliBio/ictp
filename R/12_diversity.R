#class: Introduction to biological diversity
# Professor: Andrea Sanchez

#The data about species
comm <- read.csv("data/raw/cestes/comm.csv")
dim(comm)
head(comm[,1:6])

#Which ate the 5 most abundant species
#over in the dataset?

sort(colSums(comm), decreasing = T)[2:6]

#how many species are there in each plot?

##first we should change the number of occurring to a binary number
summary(comm)

bin <- pd.get_dummies(comm["sp1":"sp56"])
print(bin)

rich <- rowSums(comm[,-1])
print(rich)

richSites<-data.frame(
  Sites = 1:nrow(comm),
  NumberOfSpecies = rich)

richSites

#Which the species that most abundant in each plot?

c2 <- comm[,-1]
c2
max <- apply(c2, 1, which.max)

mostAbSp<-data.frame(
  Sites = 1:nrow(comm),
  Abundance = max)
mostAbSp


# Diversity functions
## richness the numbers of species
## diversidade considera a riqueza e a abundancia
#shannon diversity index

my_shannon <- function(x) {
  pi = x/sum(x)
  H = - sum(pi*log(pi))
}

#Simpsom

my_simpson <- function(x) {
  pi = x/sum(x)
  1 - sum(pi)*2
}

#inverse


#two community
#library(vegan)
com.a <- c(10, 6, 4, 1)
com.b <- c(17, rep(1,7))

vegan::diversity(com.a)
vegan::diversity(com.b)
?vegan::renyi
vegan::diversity(com.a, index = "simpson")
vegan::diversity(com.b, index = "simpson")

ren_com.a <- vegan::renyi(com.a)
ren_com.b <- vegan::renyi(com.b)
# this function put together two variables
Ren_com<- rbind(ren_com.a, ren_com.b)
matplot(t(Ren_com), type = "l", axes = F)
box()
axis(side = 2)
axis(side = 1, labels = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"), at = 1:11)
legend("topright",
       legend = c("Community A", "Community B"), lty = c(1,2), col = c (1,2))

?matplot

