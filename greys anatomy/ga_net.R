# ---- C1 ----
library(network)
library(ergm)
library(intergraph)
library(sand)

# ---- C2 ----
path <- "PATH"

setwd(path)
source("ga_net-utils.R")
set.seed(20180515)

ga.gr <- read.graph("ga.graphml", format="graphml")


# ---- C3 ----
gaplot(ga.gr, names=T)


# ---- C4 ----
ga.net <- asNetwork(ga.gr)


# ---- C5 ----
ga.m1 <- ergm(ga.net ~ edges + nodemix("sex", base=c(1,3)))
summary(ga.m1)
# AIC 324.5

# ---- C6 ----
gaplot(asIgraph(simulate(ga.m1)), names=F)


# ---- C7 ----
m1.gof <- gof(ga.m1)
m1.gof

plot(m1.gof)


# ---- C8 ----
invlogit(ga.m1$coef[1] + ga.m1$coef[2])


# ---- C9 ----
ga.m2 <- ergm(ga.net ~ edges + nodemix("sex", base=-2) + degree(1))
summary(ga.m2)
# AIC 302


# ---- C10 ----
mcmc.diagnostics(ga.m2, center=F)


# ---- C11 ----
m2.gof <- gof(ga.m2)
m2.gof

plot(m2.gof)


# ---- C12save ----
ga.m3 <- ergm(ga.net ~ edges + nodemix("sex", base=-2)+ degree(1),
              control=control.ergm(MCMC.burnin=50000,
                                   MCMC.interval=5000))
save(ga.m3, file="ga_net-m3.Rdata")


# ---- C12load ----
load("ga_net-m3.Rdata")
summary(ga.m3)
# AIC 302


# ---- C13 ----
mcmc.diagnostics(ga.m3, center=F)


# ---- C14 ----
m3.gof <- gof(ga.m3)
m3.gof
plot(m3.gof)


# ---- C15save ----
ga.m4 <- ergm(ga.net ~ edges + nodemix("sex", base=-2) + degree(1)
              + absdiff("birthyear"),
              control=control.ergm(MCMC.burnin=50000,
                                   MCMC.interval=5000))
save(ga.m4, file="ga_net-m4.Rdata")


# ---- C15load ----
load("ga_net-m4.Rdata")
summary(ga.m4)

# AIC 278


# ---- C16 ----
mcmc.diagnostics(ga.m4, center=F)


# ---- C17 ----
m4.gof <- gof(ga.m4)
plot(m4.gof)


# ---- C18save ----
# articulate the model monogamy by sex
ga.m5 <- ergm(ga.net ~ edges + nodemix("sex", base=-2)
              + degree(1, "sex") + absdiff("birthyear"),
              control=control.ergm(MCMC.burnin=100000,
                                   MCMC.interval=5000,
                                   MCMC.samplesize=2048))
save(ga.m5, file="ga_net-m5.Rdata")


# ---- C18load ----
load("ga_net-m5.Rdata")
summary(ga.m5)


# ---- C19 ----
mcmc.diagnostics(ga.m5, center=F)


# ---- C20 ----
m5.gof <- gof(ga.m5)
plot(m5.gof)


# ---- C21 ----
gaplot(asIgraph(simulate(ga.m5)), names=F)


# ---- C22 ----
ga.m5$coef


# ---- C23 ---
# Nine cases:
cases <- c("M0/F0", "M0/F1", "M0/F2+",
            "M1/F0", "M1/F1", "M1/F2+",
            "M2+/F0", "M2+/F1", "M2+/F2+")

# ignore birthyear
edges <- c(1)
mixFM <- c(1)
deg1F <- c(1, -1, 0, 1, -1, 0, 1, -1, 0)
deg1M <- c(1, 1, 1, -1, -1, -1, 0, 0, 0)

cases.df <- data.frame(case=cases, edges=edges, mixFM = mixFM,
                      deg1F = deg1F, deg1M = deg1M)

logodds <- cases.df$edges*ga.m5$coef[1] + cases.df$mixFM*ga.m5$coef[2] +
  cases.df$deg1F*ga.m5$coef[3] + cases.df$deg1M*ga.m5$coef[4]
logodds

cases.df$logodds <- logodds
cases.df

# ---- C24 ----
cases.df$conditional <- invlogit(cases.df$logodds)
cases.df[order(cases.df$conditional),]

