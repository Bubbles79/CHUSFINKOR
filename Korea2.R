library(ergm)
library(network)
library(statnet)
library (texreg)
library(latticeExtra)
library(RColorBrewer)
library (texreg) 
seed <- 12345
set.seed (seed)

#Import networks and convert to matrices
coopKOR <- read.csv(file="/Users/paulwagner/Desktop/CHFinSK_US/Korea/coopKOR.csv",header=TRUE)
SciKOR <- read.csv(file= "/Users/paulwagner/Desktop/CHFinSK_US/Korea/SciKOR.csv",header=TRUE)
infKOR <- read.csv(file= "/Users/paulwagner/Desktop/CHFinSK_US/Korea/infKOR.csv", header = TRUE)
OrgTypeKOR <- read.csv(file="/Users/paulwagner/Desktop/CHFinSK_US/Korea/OrgTypeKor.csv", header = FALSE)

#Convert to matrices
coopKOR=network(coopKOR,matrix.type="edgelist",directed=TRUE) 
SciKOR=network(SciKOR,matrix.type="edgelist",directed=TRUE) 
infKOR=network(infKOR,matrix.type="edgelist",directed=TRUE) 
coopKOR <- as.matrix(coopKOR)
SciKOR <- as.matrix(SciKOR)
infKOR <- as.matrix(infKOR)
OrgTypeKOR <- as.matrix(OrgTypeKOR)

#remove self ties
diag (coopKOR) <- 0
diag (SciKOR)  <- 0
diag (infKOR) <- 0

KorOrgNames <- as.vector(colnames(coopKOR))

#Remove non-respondents
coopKOR <- coopKOR[-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106),-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106)]
SciKOR <- SciKOR[-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106),-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106)]
infKOR <- infKOR[-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106),-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106)]

#4 Actors
OrgTypeKOR <- OrgTypeKOR[-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106),6]

#5 Actors
OrgTypeKOR <- OrgTypeKOR[-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106),5]

#prepare orgtype data, so that it can be set as an attribute
OrgTypeKOR <- as.character(OrgTypeKOR)
OrgTypeKOR <- as.vector(OrgTypeKOR)

#import and calculate beliefs distance matrix. Convert to 3 point Likert scale 
PBsKOR = read.csv("/Users/paulwagner/Desktop/CHFinSK_US/Korea/BeliefsKor.csv", header = TRUE)
rownames(PBsKOR) <- KorOrgNames
colnames(PBsKOR) <- KorOrgNames
#PBsKOR <- PBsKOR[-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106), -c(1:2) ]
PBsKOR <- PBsKOR[-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106), c(3,10,11) ]
PBsKOR[PBsKOR== 97] <- 3
PBsKOR [PBsKOR  == 2] <- 1
PBsKOR [PBsKOR  == 3] <- 2
PBsKOR [PBsKOR  == 4] <- 3
PBsKOR [PBsKOR  == 5] <- 3
Pol.dist.KOR <- dist(PBsKOR, method = "euclidean") #we might want to do Euclidean
PrefSimMatKOR <- max(Pol.dist.KOR) - Pol.dist.KOR
PBsKOR <-as.matrix(PrefSimMatKOR)

#order of actor types for the nodefactor terms 
k <- sort(unique(OrgTypeKOR))
nodecov <- match(OrgTypeKOR,k)
#"BUS" "CIV" "GOV" "NGO" "SCI"

#set attributes
nw.coopKOR <- network(coopKOR) # create network object
set.vertex.attribute (nw.coopKOR, "OrgTypeKOR", OrgTypeKOR)
set.vertex.attribute (nw.coopKOR, "Influence", degree (infKOR, cmode = "indegree"))

#02 Sept 2018
Kor1 <- ergm(nw.coopKOR ~ edges,
            eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor1)
plot(mcmc.diagnostics(Kor1))
plot(gof(Kor1))

Kor2 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR),
            eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor2)
plot(mcmc.diagnostics(Kor2))
plot(gof(Kor2))

Kor3 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
               nodeifactor ("OrgTypeKOR", base=c(1,2,4)) + 
               nodematch ("OrgTypeKOR", diff = T) + nodefactor("OrgTypeKOR", base=-c(2,3,4)),
            eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor3)
plot(mcmc.diagnostics(Kor3))
plot(gof(Kor3))

Kor4a <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
                nodeifactor ("OrgTypeKOR", base=c(1,2,4)) + 
                nodematch ("OrgTypeKOR", diff = T) + nodefactor("OrgTypeKOR", base=-c(2,3,4)) +
               nodeicov("Influence"),
            eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor4a)
plot(mcmc.diagnostics(Kor4a))
plot(gof(Kor4a))

Kor4b <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
                nodeifactor ("OrgTypeKOR", base=c(1,2,4)) + 
                nodematch ("OrgTypeKOR", diff = T) + nodefactor("OrgTypeKOR", base=-c(2,3,4)) +
               edgecov (infKOR),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor4b)
plot(mcmc.diagnostics(Kor4b))
plot(gof(Kor4b))

Kor5 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
               nodeifactor ("OrgTypeKOR", base=c(1,2,4)) + 
               nodematch ("OrgTypeKOR", diff = T) + nodefactor("OrgTypeKOR", base=-c(2,3,4)) +
               edgecov (infKOR) + nodeicov("Influence"),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor5)
plot(mcmc.diagnostics(Kor5))
plot(gof(Kor5))

Kor6a <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
                nodeifactor ("OrgTypeKOR", base=c(1,2,4)) + 
                nodematch ("OrgTypeKOR", diff = T) + nodefactor("OrgTypeKOR", base=-c(2,3,4)) +
               nodeicov("Influence") +  
              mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
            eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor6a)
plot(mcmc.diagnostics(Kor6a))
plot(gof(Kor6a))

Kor6b <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
                nodeifactor ("OrgTypeKOR", base=c(1,2,4)) + 
                nodematch ("OrgTypeKOR", diff = T) + nodefactor("OrgTypeKOR", base=-c(2,3,4)) +
                edgecov (infKOR) +  
                mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor6b)
plot(mcmc.diagnostics(Kor6b))
plot(gof(Kor6b))

Kor7 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
               nodeifactor ("OrgTypeKOR", base=c(1,2,4)) + 
               nodematch ("OrgTypeKOR", diff = T) + nodefactor("OrgTypeKOR", base=-c(2,3,4)) +
                edgecov (infKOR) + nodeicov("Influence") +  
                mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor7)
plot(mcmc.diagnostics(Kor7))
plot(gof(Kor7))

par(mfrow=c(1,1))
par(mfrow=c(2,2))

#4 actor types
texreg::screenreg(list(Kor1,Kor2,Kor3,Kor4a,Kor4b,Kor5,Kor6a,Kor6b,Kor7),single.row = T)
texreg::htmlreg(list(Kor1,Kor2,Kor3,Kor4a,Kor4b,Kor5,Kor6a,Kor6b,Kor7),file="/Users/paulwagner/Desktop/CHFinSK_US/CHFinSKUS Code 01_09/Results/KoreaResults4.html",single.row = T)



#-----------------no nodematch term

Kor3.1 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
               nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor3.1)
plot(mcmc.diagnostics(Kor3.1))
plot(gof(Kor3.1))

Kor4a.1 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
                nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + 
                nodeicov("Influence"),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor4a.1)
plot(mcmc.diagnostics(Kor4a.1))
plot(gof(Kor4a.1))

Kor4b.1 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
                nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + 
                edgecov (infKOR),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor4b.1)
plot(mcmc.diagnostics(Kor4b.1))
plot(gof(Kor4b.1))

Kor5.1 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
               nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + 
               edgecov (infKOR) + nodeicov("Influence"),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor5.1)
plot(mcmc.diagnostics(Kor5.1))
plot(gof(Kor5.1))

Kor6a.1 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
                nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + 
                nodeicov("Influence") +  
                mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor6a.1)
plot(mcmc.diagnostics(Kor6a.1))
plot(gof(Kor6a.1))

Kor6b.1 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
                nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + 
                edgecov (infKOR) +  
                mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor6b.1)
plot(mcmc.diagnostics(Kor6b.1))
plot(gof(Kor6b.1))

Kor7.1 <- ergm(nw.coopKOR ~ edges + edgecov(PBsKOR) + 
               nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + 
               edgecov (infKOR) + nodeicov("Influence") +  
               mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Kor7.1)
plot(mcmc.diagnostics(Kor7.1))
plot(gof(Kor7.1))

#5 actor types
texreg::htmlreg(list(Kor1,Kor2,Kor3.1,Kor4a.1,Kor4b.1,Kor5.1,Kor6a.1,Kor6b.1,Kor7.1),file="/Users/paulwagner/Desktop/CHFinSK_US/CHFinSKUS Code 01_09/Results/KoreaResults5.html",single.row = T)
texreg::screenreg(list(Kor4a.1,Kor4b.1,Kor5.1,Kor6a.1,Kor6b.1,Kor7.1),single.row = T)
#-------------------------------

















#when NGOs and CIV are the same category 14/03/18
m6.KOR1 <- ergm(nw.coopKOR ~ edges + mutual + edgecov(PBsKOR) + edgecov(KOR.bus.civ) +
                  nodeifactor ("OrgTypeKOR", base=c(1,2,4)) + 
                  nodeofactor ("OrgTypeKOR", base=c(1,2,3)) + nodematch ("OrgTypeKOR") + 
                  edgecov (infKOR) + nodeicov("Influence") + absdiff ("Influence") + 
                  gwodegree(1.5, fixed = TRUE) + 
                  twopath + gwesp(1 , fixed = TRUE ),
                eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 2500 , MCMC.interval = 2500))
summary(m6.KOR1)
plot(mcmc.diagnostics(m6.KOR1))
plot(gof(m6.KOR1a))


#when NGOs and CIV are not the same category 14/03/18
m6.KOR1 <- ergm(nw.coopKOR ~ edges + mutual + edgecov(PBsKOR) + edgecov(KOR.bus.ngo) +
                  nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + 
                  nodeofactor ("OrgTypeKOR", base=c(1,2,3,5)) + nodematch ("OrgTypeKOR") + 
                  edgecov (infKOR) + nodeicov("Influence") + absdiff ("Influence") + 
                  gwodegree(2.5, fixed = TRUE) + 
                  twopath + gwesp(1 , fixed = TRUE ),
                eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 2500 , MCMC.interval = 2500))
summary(m6.KOR1)
plot(mcmc.diagnostics(m6.KOR1))
plot(gof(m6.KOR1))

#Older models 02/18
#edgecov(SciKOR) +
#"BUS" "CIV" "GOV" "NGO" "SCI"  absdiff ("Influence") + gwidegree(2 , fixed = TRUE ) +  gwodegree(0.5 , fixed = TRUE ) + twopath + gwdsp(0.3 , fixed = TRUE ) + gwesp(0.3 , fixed = TRUE )
m1.KOR <- ergm(nw.coopKOR ~ edges + mutual + nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + 
                nodeofactor ("OrgTypeKOR", base=c(1,2,3,5)) + nodematch ("OrgTypeKOR") + 
                edgecov (KOR.bus.ngo) + edgecov (infKOR) + nodeicov("Influence")  + 
                edgecov(PBsKOR) + absdiff ("Influence") + twopath + gwodegree(2.5, fixed = TRUE ) +
                gwesp(1 , fixed = TRUE ) + gwidegree(1.5 , fixed = TRUE ),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m1.KOR)
plot(mcmc.diagnostics(m1.KOR))
plot(gof(m1.KOR))




#------models------
m1.KOR <- ergm(nw.coopKOR ~ edges,
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m1.KOR)
plot(mcmc.diagnostics(m1.KOR))
plot(gof(m1.KOR))

m2.KOR <- ergm(nw.coopKOR ~ edges + mutual,
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m2.KOR)
plot(mcmc.diagnostics(m2.KOR))
plot(gof(m2.KOR))

m3.KOR <- ergm(nw.coopKOR ~ edges + mutual + gwidegree(0.5 , fixed = TRUE ) + gwodegree(2, fixed = TRUE) +
                twopath + gwesp(0.1 , fixed = FALSE),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m3.KOR)
plot(mcmc.diagnostics(m3.KOR))
plot(gof(m3.KOR))

m4.KOR <- ergm(nw.coopKOR ~ edges + mutual + gwidegree(1.5 , fixed = TRUE ) + gwodegree(2.5, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ) + edgecov(PBsKOR),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m4.KOR)
plot(mcmc.diagnostics(m4.KOR))
plot(gof(m4.KOR))

m5.KOR <- ergm(nw.coopKOR ~ edges + mutual + gwidegree(1.5 , fixed = TRUE ) + gwodegree(2.5, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ) + edgecov(PBsKOR) + nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + 
                nodeofactor ("OrgTypeKOR", base=c(1,2,3,5)) + nodematch ("OrgTypeKOR"),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m5.KOR)
plot(mcmc.diagnostics(m5.KOR))
plot(gof(m5.KOR))

m6.KOR <- ergm(nw.coopKOR ~ edges + mutual + gwidegree(1.5 , fixed = TRUE ) + gwodegree(2.5, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ) + edgecov(PBsKOR) + edgecov(KOR.bus.ngo) +
                 nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + 
                nodeofactor ("OrgTypeKOR", base=c(1,2,3,5)) + nodematch ("OrgTypeKOR") + 
                edgecov (infKOR) + nodeicov("Influence") + absdiff ("Influence"),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m6.KOR)
plot(mcmc.diagnostics(m6.KOR))
plot(gof(m6.KOR))

texreg::htmlreg(list(m1.KOR,m2.KOR,m3.KOR,m4.KOR,m5.KOR,m6.KOR),file="/Users/paulwagner/Desktop/ERGMs/CHFinSK_US/Korea.html",single.row = T)


#when NGOs and CIV are the same category
m6.KOR1 <- ergm(nw.coopKOR ~ edges + mutual + edgecov(PBsKOR) + edgecov(KOR.bus.civ) +
                 nodeifactor ("OrgTypeKOR", base=c(1,2,4)) + 
                 nodeofactor ("OrgTypeKOR", base=c(1,3,4)) + nodematch ("OrgTypeKOR") + 
                 edgecov (infKOR) + nodeicov("Influence") + absdiff ("Influence") + 
                 gwodegree(1.5, fixed = TRUE) + 
                 twopath + gwesp(1 , fixed = TRUE ),
               eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 2500 , MCMC.interval = 2500))
summary(m6.KOR1)
plot(mcmc.diagnostics(m6.KOR1))
plot(gof(m6.KOR1))

#"BUS" "CIV" "GOV" "SCI"
palette(c("blue","green","red","yellow"))
plot(nw.coopKOR , displaylabels = FALSE , vertex.cex = degree(infKOR, cmode = 'indegree') / 20 , vertex.border = "black", edge.col = "black", vertex.col = "OrgTypeKOR",pad = 1)






#------ may 13th 
Kor.m1 <- ergm(nw.coopKOR ~ edges + mutual + edgecov(PBsKOR) + nodematch ("OrgTypeKOR") +
                nodeifactor ("OrgTypeKOR", base=c(1,2,4,5)) + edgecov(KOR.bus.ngo) +
                nodeicov("Influence") + absdiff ("Influence") +
                edgecov(LowHighKor) + edgecov(LowHighKor) +
                gwodegree(2.5, fixed = TRUE) +
                gwdsp(0.1 , fixed = TRUE ) + gwesp(0.1 , fixed = TRUE ),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m6.Kor1)



