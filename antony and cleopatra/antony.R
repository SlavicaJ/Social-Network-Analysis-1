# ---- C1 ----
library("ggplot2")
library("GGally")
library("sand")

# ---- C2 ----
path <- "path"
setwd(path)

antony <- read.graph("antony-c2c.graphml", format="graphml")

# ---- C3 -----
# Filter
antony_f <- delete_edges(antony, E(antony)[E(antony)$weight < 2])
antony_f <- delete_vertices(antony_f, V(antony_f)[degree(antony_f) < 1])

#generating a list of components in antony_f
components <- decompose(antony_f)

#number of nodes in each component
sapply(components, vcount)    

#saving the main component with most nodes    
main_comp <- components[[which.max(sapply(components, vcount))]]


# ---- C4 ----
lo <- layout_with_fr(main_comp)
#plot(main_comp, layout=lo)

# ---- C5 ----
main_comp.dc <- degree(main_comp, normalize=TRUE)
wdeg <- graph.strength(main_comp)
#main_comp.wdc <- (wdeg - min(wdeg)) / (max(wdeg)-min(wdeg))  #min-max scaling
main_comp.wdc <- wdeg / sqrt(sum(wdeg^2))   #unit scaling
main_comp.bet <- betweenness(main_comp, normalize=TRUE, weights=NA)
main_comp.clo <- closeness(main_comp, normalize=TRUE, weights=NA)
main_comp.pr <- page_rank(main_comp)
main_comp.eig <- eigen_centrality(main_comp, directed=FALSE)

main_comp.cent <- data.frame(degree=main_comp.dc,
                             wdegree=main_comp.wdc,
                             pr=main_comp.pr$vector,
                             eig=main_comp.eig$vector,
                             betw=main_comp.bet,
                             clo=main_comp.clo)

# ---- C6 ----
ggcorr(main_comp.cent, label=TRUE)

# ---- C7 ----
# Range 5 - 15
library(scales)
eig_rescale <- rescale(main_comp.eig$vector, to=c(5,15), from=range(main_comp.eig$vector))
bet_rescale <- rescale(main_comp.bet, to=c(5,15), from=range(main_comp.bet))

par(mfrow=c(1,2))
plot(main_comp, layout=lo, vertex.size=eig_rescale)
plot(main_comp, layout=lo, vertex.size=bet_rescale)
par(mfrow=c(1,1))

# ---- C8 ----
nationality.factor <- factor(V(main_comp)$Nationality)
bet.Roman <- main_comp.bet[nationality.factor == "Roman"]
bet.Egyptian <- main_comp.bet[nationality.factor == "Egyptian"]

main_comp.bet.df <- rbind(data.frame(Betweenness=bet.Roman, Nationality="Roman"),
                          data.frame(Betweenness=bet.Egyptian, Nationality="Egyptian"))

g <- ggplot(main_comp.bet.df, aes(x=Betweenness, fill=Nationality))
g <- g + geom_density(aes(alpha=0.5))
g <- g + guides(alpha=FALSE)
print(g)

# ---- C9 ----
eig.Roman <- main_comp.eig$vector[nationality.factor == "Roman"]
eig.Egyptian <- main_comp.eig$vector[nationality.factor == "Egyptian"]

main_comp.eig.df <- rbind(data.frame(EV_Centrality=eig.Roman, Nationality="Roman"),
                          data.frame(EV_Centrality=eig.Egyptian, Nationality="Egyptian"))
g <- ggplot(main_comp.eig.df, aes(y=EV_Centrality, x=Nationality))
g <- g + geom_violin() + geom_jitter(height=0, width=0.05)
print(g)

# ---- C10 ----
# Antony and Cleopatra subset
AC_cent.df <- main_comp.cent[V(main_comp)$label %in% c("cleopatra", "antony"),] 
AC_cent.df
 
# ---- C11 ----
percent_diff <- (AC_cent.df[1,] - AC_cent.df[2,]) / AC_cent.df[2,]
percent_diff

