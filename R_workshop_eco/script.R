install.packages("tidyverse")
install.packages("factoextra")
install.packages("vegan")
install.packages("pvclust")
install.packages("dendextend")
install.packages("mgcv")
install.packages("mvabund")
install.packages("devtools")

library(tidyverse)
library(factoextra)
library(vegan)
library(pvclust)
library(dendextend)
library(mgcv)
library(mvabund)
library(devtools)

#Introduction to multivariate analysis in R
#George Perry / Uni of Auckland

getwd()
setwd("J:/R_workshop_eco")
getwd()
devtools::install_github("gavinsimpson/ggvegan")
devtools::install_github("vqv/ggbiplot")

install.packages("ggvegan")
install.packages("ggbiplot")


devtools::install_github('gavinsimpson/ggvegan')
devtools::install_github('vqv/ggbiplot')


# Set the data up and select variables of interest
physical <- read.csv('J:/R_workshop_eco/data/physicochemical.csv', header = T, row.names = 1)
physical.lee17 <- select(physical, SRP, Ammonia, Velocity, ProportionMacrophyte,
                         Temperature, Conductivity, Pasture, Scrub, Forest, 
                         BankGradient, Elevation, DistanceToSea, CatchmentSize)

inverts <- read.csv(file = 'J:/R_workshop_eco/data/inverts.csv', header = T, row.names = 1)

# Scale using either base::scale or vegan::decostand  
physical.sc <- scale(physical.lee17)
physical.sc <- decostand(physical.lee17, method = "standardize")  # Vegan package


?scale
#magurran

#calculating distance

library(vegan)
physical.dist <- dist(physical.sc)   # In base R - default distance is Euclidean
inverts.bc <- vegdist(inverts, method = "bray")  # In vegan

factoextra::fviz_dist(physical.dist, lab_size = 8)


#Hierarchical agglomerative clustering

invert.hc <- hclust(inverts.bc, method = "average")  # stats library
plot(invert.hc, hang = -1, main = "", xlab = 'Sites', ylab = 'Distance')

#Need to choose distance when it comes to interpretations of the dendrogram

#Using ggplot
library(dendextend)
invert.hc.gg <- as.ggdend(as.dendrogram(invert.hc))
ggplot(invert.hc.gg, horiz = TRUE) + theme_bw()
## Warning: Removed 57 rows containing missing values (geom_point).


#Bootstrapping to assess the classificiation
# this analysis is very computationally expensive (control with nboot)
physical.pc <- pvclust(t(physical.sc), method.dist="euclidean", 
                       method.hclust="average", nboot=1000, quiet = TRUE)
plot(physical.pc, main = "Bootstrapped classification")
pvrect(physical.pc, alpha = 0.95) #draw red index box in the plot

seplot(physical.pc, main = 'P-value vs std (sampling) error')
abline(v = 0.95, col = 'red')


#k-means
# Various options also available in NbClust package
fviz_nbclust(physical.sc, kmeans, method='silhouette')
#statistically 2 might work, 10 could work, but the number of sites are only 30, so it might not work


# Do the kmeans analysis
site.km <- kmeans(x = physical.sc, centers = 2)           # centres = groups (K)
physical.sc.df <- data.frame(physical.sc)
physical.sc.df$km.cluster <- as.factor(site.km$cluster)   # add cluster id as a column

# Plot it via ggplot...
site.km.plot <- ggplot(data = physical.sc.df) +
  geom_point(aes(x = Pasture, y = Temperature, col = km.cluster), size = 2) +
  labs(colour = 'Cluster id (k)') + coord_equal()


#In theory, more robust option: k-medioids (pam())

library(cluster)
site.pam <- pam(x = physical.sc, k = 2)
clusplot(site.pam, main = 'Clusters from PAM')


## comparsions among k means and k medioids using differnet optimal k (k=2 / k=4)
# Do the kmeans analysis
site.km4 <- kmeans(x = physical.sc, centers = 4)
physical.sc.df <- data.frame(physical.sc)
physical.sc.df$km.cluster4 <- as.factor(site.km4$cluster)



ggpubr::ggarrange()

##Ordination
##PCA

library(tidyverse)
f4a.dat <- select(physical.lee17, SRP, Ammonia, Velocity, ProportionMacrophyte,
                  Temperature, Conductivity)
f4b.dat <- select(physical.lee17, Pasture, Scrub, Forest, BankGradient,
                  Elevation, DistanceToSea, CatchmentSize)
# for convenience, one for water quality variables, and the other one for other variables

# prcomp preferred to princomp (?princomp)
chem.pca <- prcomp(f4a.dat, scale = TRUE) # scale if data on different scales
lu.pca <- prcomp(f4b.dat, scale = TRUE)
#chem: water chem, lu: land use
biplot(chem.pca, main = "Water quality PCA- Lee et al. 2017")

#ggbiplot can't load the package here
library(ggbiplot)
ggbiplot(lu.pca, obs.scale = 1, var.scale = 1) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')


#loadings
summary(chem.pca)

#scores
scores(chem.pca)

#screeplots
par(mfrow = c(1,2))
screeplot(chem.pca, main = "Water quality")
screeplot(lu.pca, main = "Catchment LU")

#Unconstrained - nMDS
# Set up data as per Lee et al. 2018
sites18 <- c('F01', 'F03', 'F04', 'F07', 'F08', 'F10', 'F11', 'F12', 'F13', 'F17', 'F18')
inverts.lee18 <- inverts[rownames(inverts) %in% sites18, ]
physical.lee18 <- physical.lee17[rownames(physical.lee17) %in% sites18, ]

invert.mds <- metaMDS(inverts.lee18, distance = "jaccard", 
                      autotransform = TRUE, wascores = TRUE)

plot(invert.mds, type = 't', display = 'sites', cex = 2)


invert.mds$stress
## [1] 0.1605531
stressplot(invert.mds)


#using ggvegan package
#autoplot(invert.mds, geom = "text", layers = 'sites', size = 2)
#install.packages("ggvegan")



#Vector fitting
phys.fit <- envfit(invert.mds, physical.lee18, perm = 9999, p.max = 0.10)
plot(invert.mds)
plot(phys.fit)


#counterpart: surface fitting (ordisurf)
#contour and size of circle are important in interpretation

phys.surf <- ordisurf(invert.mds ~ SRP, physical.lee18, bubble = 5)
plot(invert.mds, main = '')
plot(phys.surf)

##Post-hoc assessment of dissimilarity matrices
#Load Grampians vegetation data set:
  
grampians <- read.csv(file = 'J:/R_workshop_eco/data/gramps99.csv', header = T, row.names = 1)
gr.dist <- vegdist(grampians)
yr <- as.factor(substr(rownames(grampians),1,2))  # fire year

# Do the nMDS analysis
gramps.mds <- metaMDS(gr.dist, wascores = FALSE, autotransform = FALSE, trace = 0) 
# trace = 0 supresses messages about stress
gramps.mds$stress
## [1] 0.2009278

grp.mds <- data.frame(scores(gramps.mds), yr  = yr)
ggplot(data = (grp.mds)) +
  geom_point(aes(x = NMDS1, y = NMDS2, col = factor(yr)), size = 2) +
  labs(colour='Year') + 
  coord_equal()


plot(gramps.mds)
## species scores not available
grp.hull <- ordihull(ord = gramps.mds, groups = yr, label = TRUE)

plot(gramps.mds)
## species scores not available
grp.spider <- ordispider(ord = gramps.mds, groups = yr, label = TRUE)

#Post hoc test: ANOSIM
gramps.ano <- anosim(x = grampians, grouping = yr)
gramps.ano

#autoplot(gramps.ano, notch = FALSE)
plot(gramps.ano)

#Post-hoc tests on dissimilarity matrices - PERMANOVA
#Read the "by" argument carefully; the test is seqeuntial
gramps.ad <- adonis2(gr.dist ~ yr)
gramps.ad

#Post-hoc tests on dissimilarity matrices - MHV
gramps.mod <- betadisper(gr.dist, yr)
plot(gramps.mod, main = "")

mod.HSD <- TukeyHSD(gramps.mod)
mod.HSD
plot(mod.HSD)


#Constrained ordination

#R package mvabund
#quite strong with all types of data: not only count but also other types
#Go youtube and check the video

library(mvabund)
meanvar.plot(grampians, xlab = 'Mean', ylab = 'Variance')

# Convert from cover to pres-abs and use a binomial model (Poisson for count)
grampians.pa <- decostand(grampians, method = "pa")
grampians.pa <- mvabund(grampians.pa)
gramps.mod <- manyglm(grampians.pa ~ factor(yr), family="binomial")
plot(gramps.mod)

#ANOVA to asess the effect of year
anova(gramps.mod)


#ANOVA to asess the effect of year per species
anova(gramps.mod, p.uni="adjusted")





