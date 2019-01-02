# Loading the libraries
# ---- C1 ----
library("ggplot2")
library("GGally")
library("sand")

# ---- C2 ----
# Loading the "Les Miserables" data
path <- "path"
setwd(path)
lesmis <- read.graph("les-mis-full.graphml", format="graphml")

# ---- C3 ----
# Identify the bipartite structure
is_bipartite(lesmis)
summary(lesmis)
lesmis <- set_vertex_attr(lesmis, "type", index=which(V(lesmis)$bipartite==FALSE), FALSE) 
lesmis <- set_vertex_attr(lesmis, "type", index=which(V(lesmis)$bipartite==TRUE), TRUE)

# ---- C4 ----
# Create section-to-section bipartite projection
lm_projs <- bipartite_projection(lesmis, which="both")
lm_sec <- lm_projs[[2]]

# ---- C5 ----
# Histogram of edge weights
sec_wts <- E(lm_sec)$weight
g <- ggplot(data.frame(x=sec_wts), aes(x=x))
g <- g + geom_histogram(position="dodge", binwidth=0.25)
g <- g + labs(x="Edge Weights", y="Count", title="Section Projection - Edge Weights")
g <- g + scale_x_continuous(breaks=seq(0,10,by=1))
g <- g + scale_y_continuous(breaks=seq(0,15000,by=1000))
print(g)

# ---- C6 ----
# Remove edges of weight 1
lm_secf <- delete_edges(lm_sec, E(lm_sec)[E(lm_sec)$weight == 1])

# ---- C7 ----
# Calculate the set of connected components using the "decompose" function
cmpnts <- decompose(lm_secf)

# ---- C8 ----
# Extract the subgraph containing just the giant component. Use this for rest of assignment
main_comp <- cmpnts[[which.max(sapply(cmpnts, vcount))]]

# ---- C9 ----
# Display a summary of the extracted subgraph
summary(main_comp)

# ---- C10 ----
# Save the extracted subgraph to a graphml file
write_graph(main_comp, "lesmes_sec2sec.graphml", format="graphml")
