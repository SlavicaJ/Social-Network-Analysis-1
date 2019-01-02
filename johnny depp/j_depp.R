## Working with the bipartite network describing Johnny Depp costars

# ---- C1 ----
# Load libraries
library("ggplot2")
library("sand")

# ---- C2 ----
# Load edge and attribute data
path <- "path"
setwd(path)
edge.df <- read.csv("depp_edges.csv")
node.df <- read.csv("depp_nodes.csv")

# ---- C3 ----
# Convert to graph
depp <- graph.data.frame(edge.df, vertices=node.df, directed=FALSE)

# ---- C4 ----
# Examine attributes
summary(depp)
is_bipartite(depp)
head(V(depp)$name)
head(V(depp)$decade)
head(V(depp)$type)
head(V(depp)$budget)

# ---- C5 ----
# Projections
projs <- bipartite_projection(depp, which="both")
depp_p2p <- projs[[1]]
summary(depp_p2p)

depp_m2m <- projs[[2]]
summary(depp_m2m)

# ---- C6 ----
# Remove non-matching attributes
depp_p2pf <- delete_vertex_attr(depp_p2p, "budget")
summary(depp_p2pf)

depp_m2mf <- delete_vertex_attr(depp_m2m, "decade")
summary(depp_m2mf)

# ---- C7a ----
# Edge weight histogrqm (base)
depp_p2pf_wts <- E(depp_p2pf)$weight
plt <- barplot(table(depp_p2pf_wts), xlab="# of Movies", 
               ylab="# of Actors/Actresses", main="Number of Movies with Johnny Depp")
axis(2, at=seq(0,900,by=100))

# ---- C7b ----
# Edge weight histogram (ggplot)
plt <- ggplot(data.frame(x=depp_p2pf_wts), aes(x=x))
plt <- plt + geom_bar(stat="Count")
plt <- plt + labs(x="# of Movies", y="# of Actors/Actresses",
                  title="Number of Movies with Johnny Depp")
plt <- plt + scale_y_continuous(breaks=seq(0,900,by=100))
print(plt)

# ---- C8 ----
# Filter edges of weight = 1
depp_p2pf <- delete_edges(depp_p2pf, E(depp_p2pf)[E(depp_p2pf)$weight < 2])
#table(E(depp_p2pf)$weight)

# ---- C9 ----
# Remove singleton nodes
depp_p2pf <- delete_vertices(depp_p2pf, V(depp_p2pf)[degree(depp_p2pf) < 1])
#table(degree(depp_p2pf))

# ---- C10 ----
# Plot
plot(depp_p2pf, layout=layout_with_kk)

# ---- C11a ----
# Weighted degree histogram (base)
wdeg <- graph.strength(depp_p2pf)
plt <- hist(wdeg, xlab="Weighted Degree", ylab="# of Actors/Actresses",
               main="Weighted Degree of Number of Movies with Johnny Depp")
axis(2, at=seq(0,25, by=5))
axis(1, at=seq(0,50,by=5))               

# ---- C11b ----
# Weighted degree histogram (ggplot)
plt <- ggplot(data.frame(x=wdeg), aes(x=x))
plt <- plt + geom_bar(stat="count")
plt <- plt + labs(x="Weighted Degree", y="# of Actors/Actresss", 
                  title="Number of Movies with Johnny Depp")
plt <- plt + scale_y_continuous(breaks=seq(0,14,by=2))
plt <- plt + scale_x_continuous(breaks=seq(0,50,by=5))
print(plt)

# ---- C12 ----
# Find the 2 highest weighted degree actors (other than Depp)
wdeg.df<-data.frame(sort(wdeg, TRUE)) #sorted dataframe of actors based on weighted degree
row.names(wdeg.df)[2]
row.names(wdeg.df)[3]

# ---- C13 ----
# Plot ego networks           
egos <- make_ego_graph(depp_p2pf, 1, c(which(V(depp_p2pf)$name == row.names(wdeg.df)[2]),
                                       which(V(depp_p2pf)$name == row.names(wdeg.df)[3])))
bloom.gr <- egos[[1]]
carter.gr <- egos[[2]]

bloom.lo <- layout_as_star(bloom.gr, V(bloom.gr)[V(bloom.gr)$name=="Orlando_Bloom"])
carter.lo <- layout_as_star(carter.gr, V(carter.gr)[V(carter.gr)$name=="Helena_Bonham_Carter"])

par(mfrow=c(1,2))
plot(bloom.gr, layout=bloom.lo)
plot(carter.gr, layout=carter.lo)
par(mfrow=c(1,2))

# Johnny Depp ego network
test <- make_ego_graph(depp_p2pf, 2, which(V(depp_p2pf)$name=="Johnny_Depp"))
test.gr <- test[[1]]
test.lo <- layout_as_star(test.gr, V(test.gr)[V(test.gr)$name=="Johnny_Depp"])
plot(test.gr, layout=test.lo)



