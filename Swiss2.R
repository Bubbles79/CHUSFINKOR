library(sna)
library(ergm)
seed <- 12345
set.seed (seed)

#Import networks
coopSwiss <- read.csv(file="/Users/paulwagner/Desktop/CHFinSK_US/Switzerland/CH_collab.csv",header=TRUE)
infSwiss <- read.csv(file= "/Users/paulwagner/Desktop/CHFinSK_US/Switzerland/CH_Inf.csv", header = TRUE)
OrgTypeSwiss <- read.csv(file="/Users/paulwagner/Desktop/CHFinSK_US/Switzerland/SwissOrgType.csv", header = TRUE)

coopSwiss <- as.matrix(coopSwiss)
infSwiss <- as.matrix(infSwiss)
diag (coopSwiss) <- 0
diag (infSwiss) <- 0

SwissOrgNames <- as.vector(colnames(coopSwiss))

OrgTypeSwiss  <-OrgTypeSwiss[,2 ] #When four actor types
OrgTypeSwiss  <-OrgTypeSwiss[,3 ] #When five actor types
OrgTypeSwiss  <- as.character(OrgTypeSwiss )
OrgTypeSwiss  <- as.vector(OrgTypeSwiss )

#import and calculate beliefs distance matrix. Convert to 3 point Likert scale 
PBsSwiss = read.csv("/users/paulwagner/Desktop/CHFinSK_US/Switzerland/CH_Bfs.csv", header = TRUE, stringsAsFactors=FALSE)
rownames(PBsSwiss) <- SwissOrgNames
colnames(PBsSwiss) <- SwissOrgNames
PBsSwiss <- PBsSwiss[,-c(1,2,4,7),]
Pol.dist.Swiss <- dist(PBsSwiss, method = "manhattan")
PrefSimMatSwiss <- max(Pol.dist.Swiss) - Pol.dist.Swiss
PBsSwiss <-as.matrix(PrefSimMatSwiss)



#order of actor types for the nodefactor terms 
f <- sort(unique(OrgTypeSwiss))
nodecov <- match(OrgTypeSwiss,f)
#"BUS" "CIV" "GOV" "NGO" "SCI"
#"BUS" "CIV" "GOV" "SCI"

#set attributes
nw.coopSwiss <- network(coopSwiss) # create network object
set.vertex.attribute (nw.coopSwiss, "OrgTypeSwiss", OrgTypeSwiss)
set.vertex.attribute (nw.coopSwiss, "Influence", degree (infSwiss, cmode = "indegree"))

#03 Sept 2018--------
Swiss1 <- ergm(nw.coopSwiss ~ edges,
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Swiss1)
plot(mcmc.diagnostics(Swiss1))
plot(gof(Swiss1))

Swiss2 <- ergm(nw.coopSwiss ~ edges + edgecov(PBsSwiss),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Swiss2)
plot(mcmc.diagnostics(Swiss2))
plot(gof(Swiss2))

Swiss3 <- ergm(nw.coopSwiss ~ edges + edgecov(PBsSwiss) + 
               nodeifactor ("OrgTypeSwiss", base=c(1,2,4,5)) + 
               nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4,5)),
               #nodeifactor ("OrgTypeSwiss", base=c(1,2,4)) + 
               #nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4)),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Swiss3)
plot(mcmc.diagnostics(Swiss3))
plot(gof(Swiss3))

Swiss4a <- ergm(nw.coopSwiss ~ edges + edgecov(PBsSwiss) + 
                nodeifactor ("OrgTypeSwiss", base=c(1,2,4,5)) + 
                nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4,5)) + 
                #nodeifactor ("OrgTypeSwiss", base=c(1,2,4)) + 
                #nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4)) +
                nodeicov("Influence"),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Swiss4a)
plot(mcmc.diagnostics(Swiss4a))
plot(gof(Swiss4a))

Swiss4b <- ergm(nw.coopSwiss ~ edges + edgecov(PBsSwiss) + 
                  nodeifactor ("OrgTypeSwiss", base=c(1,2,4,5)) + 
                  nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4,5)) + 
                  #nodeifactor ("OrgTypeSwiss", base=c(1,2,4)) + 
                  #nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4)) +
                  edgecov (infSwiss),
               eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Swiss4b)
plot(mcmc.diagnostics(Swiss4b))
plot(gof(Swiss4b))

Swiss5<- ergm(nw.coopSwiss ~ edges + edgecov(PBsSwiss) + 
                nodeifactor ("OrgTypeSwiss", base=c(1,2,4,5)) + 
                nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4,5)) + 
                #nodeifactor ("OrgTypeSwiss", base=c(1,2,4)) + 
                #nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4)) +
                edgecov (infSwiss) + nodeicov("Influence"),
                eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Swiss5)
plot(mcmc.diagnostics(Swiss5))
plot(gof(Swiss5))

Swiss6a <- ergm(nw.coopSwiss ~ edges + edgecov(PBsSwiss) + 
                  nodeifactor ("OrgTypeSwiss", base=c(1,2,4,5)) + 
                  nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4,5)) + 
                  #nodeifactor ("OrgTypeSwiss", base=c(1,2,4)) + 
                  #nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4)) +
                  nodeicov("Influence") +
                  mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
                eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Swiss6a)
plot(mcmc.diagnostics(Swiss6a))
plot(gof(Swiss6a))

Swiss6b <- ergm(nw.coopSwiss ~ edges + edgecov(PBsSwiss) + 
                  nodeifactor ("OrgTypeSwiss", base=c(1,2,4,5)) + 
                  nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4,5)) + 
                  #nodeifactor ("OrgTypeSwiss", base=c(1,2,4)) + 
                  #nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4)) +
                  edgecov (infSwiss) + 
                  mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
                eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Swiss6b)
plot(mcmc.diagnostics(Swiss6b))
plot(gof(Swiss6b))

Swiss7<- ergm(nw.coopSwiss ~ edges + edgecov(PBsSwiss) + 
                nodeifactor ("OrgTypeSwiss", base=c(1,2,4,5)) + 
                nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4,5)) + 
                #nodeifactor ("OrgTypeSwiss", base=c(1,2,4)) + 
                #nodematch ("OrgTypeSwiss", diff = T) + nodefactor("OrgTypeSwiss", base=-c(2,3,4)) +
                edgecov (infSwiss) + nodeicov("Influence") + 
              mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Swiss7)
plot(mcmc.diagnostics(Swiss7))
plot(gof(Swiss7))

par(mfrow=c(1,1))
par(mfrow=c(2,2))

texreg::screenreg(list(Swiss1,Swiss2,Swiss3,Swiss4a,Swiss4b,Swiss5,Swiss6a,Swiss6b,Swiss7),single.row = T)

#4 actor types
texreg::htmlreg(list(Swiss1,Swiss2,Swiss3,Swiss4a,Swiss4b,Swiss5,Swiss6a,Swiss6b,Swiss7),file="/Users/paulwagner/Desktop/CHFinSK_US/CHFinSKUS Code 01_09/Results/SwissResults4.html",single.row = T)
#5 actor types
texreg::htmlreg(list(Swiss1,Swiss2,Swiss3,Swiss4a,Swiss4b,Swiss5,Swiss6a,Swiss6b,Swiss7),file="/Users/paulwagner/Desktop/CHFinSK_US/CHFinSKUS Code 01_09/Results/SwissResults5.html",single.row = T)

