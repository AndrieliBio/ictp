#Multivariate 11.04.2022
#Andrea Sanches Tapia

#Cluster using the package
library(vegan)
data(dune)
data(dune.env)
table(dune.env$Management)

#We calculate two dissimlairty indices between sites: Bray-Curtis distance and Chord distance
bray_distance <- vegdist(dune)
# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))

#We perform the cluster analysis. Which is the default clustering method?
#library(cluster)
#Let’s use "average", who will link clusters
b_cluster <- hclust(bray_distance, method = "average")
c_cluster <- hclust(chord_distance, method = "average")


# let's plot

par(mfrow = c(1, 2))
plot(b_cluster)
plot(c_cluster)
par(mfrow = c(1, 1))


par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
