# Loading the libraries
# ---- C1 ----
library("ggplot2")
library("GGally")
library("mcclust")
library("sand")

# ---- C2 ----
# Loading the data
setwd("path")
twtr <- read.graph("twitter-20171004.graphml", format="graphml")

# ---- C3 ----
# Creating dataframe of frequency percentages of weighted degree
wdeg.tab <- tabulate(graph.strength(twtr)) / vcount(twtr)
wdeg.df <- data.frame(WDegree=seq(1, max(graph.strength(twtr))),
                      Freq=wdeg.tab)

wdeg.df <- wdeg.df[wdeg.df$Freq > 0,]  # removing cases where 0 frequency

g <- ggplot(wdeg.df, aes(x=WDegree, y=Freq))
g <- g + geom_point() + geom_smooth(se=F)
g <- g + scale_y_log10()  
g <- g + scale_x_log10()
g <- g + labs(x="Weighted Degree", 
              y="Frequency", 
              title="Log-Log Plot of Weighted Degree in Twitter Dataset")
print(g)

# ---- C4 ----
# Removing nodes with weighted degree with weight of 1 or 2
twtr.f <- delete_vertices(twtr, V(twtr)[graph.strength(twtr) %in% c(1,2)])

# ---- C5 ----
# Extracting primary component 
cmpnts <- decompose(twtr.f)
twtr.mc <- cmpnts[[which.max(sapply(cmpnts, vcount))]]

# ---- C6 ----
set.seed(0)

# ---- C7 ----
clust.le <- cluster_leading_eigen(twtr.mc)
clust.fg <- cluster_fast_greedy(twtr.mc)

twtr.mc.uw <- delete_edge_attr(twtr.mc, "weight")
clust.ebtw <- cluster_edge_betweenness(twtr.mc.uw)
clust.wt5 <- cluster_walktrap(twtr.mc, steps=5)
clust.wt10 <- cluster_walktrap(twtr.mc, steps=10)

# ---- C8 ----
res <- list(clust.le, clust.fg, clust.ebtw, clust.wt5, clust.wt10)
res.df <- data.frame(Modularity=sapply(res, modularity),
                     Size=sapply(res, length),
                     Alg = c("LE", "FG", "EBTW", "WT5", "WT10"))

# plot of modularity bar chart alone
g <- ggplot(res.df, aes(x=Alg, y=Modularity - mean(res.df$Modularity), label=Alg))
g <- g + geom_col(position="stack", aes(fill=Alg))
g <- g + labs(title="Comparison of Community Detection Metrics by Modularity")
print(g)

# plot of community count bar chart alone
g <- ggplot(res.df, aes(x=Alg, y=Size, label=Alg))
g <- g + geom_col(position="stack", aes(fill=Alg))
g <- g + labs(title="Comparison of Community Detection Metrics by # of Communities")
print(g)

# plot of modularity vs size in one bar chart
g <- ggplot(res.df, aes(x=Modularity, y=Size, label=Alg))
g <- g + geom_col(position="stack", aes(fill=Alg))
g <- g + labs(title="Comparison of Community Detection Metrics by Modularity and # of Communities")
print(g)

# ---- C9 ----
# removing the result which produces the largest number of clusters
res.dff <- res.df[-which.max(res.df$Size),]
  
dist.lefg <- arandi(membership(clust.le), membership(clust.fg))
dist.lewt5 <- arandi(membership(clust.le), membership(clust.wt5))
dist.lewt10 <- arandi(membership(clust.le), membership(clust.wt10))
dist.fgwt5 <- arandi(membership(clust.fg), membership(clust.wt5))
dist.fgwt10 <- arandi(membership(clust.fg), membership(clust.wt10))
dist.wt5wt10 <- arandi(membership(clust.wt5), membership(clust.wt10))

metrics <- list(c("LE", "FG", "WT5", "WT10"), c("LE", "FG", "WT5", "WT10"))
l3.dist <- c(0, dist.lefg, dist.lewt5, dist.lewt10, ## LE Row
             dist.lefg, 0, dist.fgwt5, dist.fgwt10, ## FG Row
             dist.lewt5, dist.fgwt5, 0, dist.wt5wt10, ## WT5 Row
             dist.lewt10, dist.fgwt10, dist.wt5wt10, 0) ## WT10 row

l3.mat <- matrix(data=l3.dist, nrow=4, ncol=4, byrow=TRUE, dimnames=metrics)
ggcorr(data=NULL, cor_matrix= l3.mat, label=TRUE)+labs(title='Correlation Matrix of Community Detection Algorithms')

# ---- C10 ----
# using walktrap10 and leading eigenvector communities for remainder
clust.wt10
clust.le

# ---- C11 ----
# identifying hashtag I'm interested in based on hashtag with highest degree
#V(twtr.mc)$label
# I will investigate the "#maga" hashtag

# ---- C12 ----
ht_loc <- which(V(twtr.mc)$label == "#maga")
ht_loc

# ---- C13 ----
# extracting subgraphs containing "#maga" from wt10 and leading eigenvector
wt10_vids <- clust.wt10[[membership(clust.wt10)[ht_loc]]]
wt10_comm <- induced_subgraph(twtr.mc, wt10_vids)

le_vids <- clust.le[[membership(clust.le)[ht_loc]]]
le_comm <- induced_subgraph(twtr.mc, le_vids)

# ---- C14 ----
# plotting the wt10 #maga community and the le #maga community
par(mfrow=c(1,2))
plot(wt10_comm, layout=layout_with_kk(wt10_comm), main="Walktrap 10 Community")
plot(le_comm, layout=layout_with_kk(le_comm), main="Leading Eigenvector Community")
par(mfrow=c(1,1))

# ---- C15 ----
# exporting the graphs for gephi work
write_graph(wt10_comm, "wt10_comm.graphml", format="graphml")
write_graph(le_comm, "le_comm.graphml", format="graphml")
