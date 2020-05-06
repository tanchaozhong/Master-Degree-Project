# Part V - CCA analysis
# Data needed:
# ben; env
  # species names of ben is named based on order
  # such as "Insecta1", 'Olig1'

library(vegan)

#---------------Data standardization -------------------
env <- env[rowSums(spe)>0,]
spe <- spe[rowSums(spe)>0,]

ben.lg=log(ben+1)#log transform
#spe.hel<-decostand(spe,"hellinger")#Hellinger transform

#spe<-spe.hel # choose Hellinger transdorm this time
decorana(ben.lg) # DCA analysis (<3 use RDA; 4 use CCA)

# core code for RDA analysis - use variables in data(env) to explain variations of spe
spe.cca <- cca(ben.lg~.,data=env); title <- "CCA"
#spe.rda<- rda(ben.lg~.,data=env); title <- "RDA"

 results.analysis <- spe.cca
# results.analysis <- spe.rda

summary(results.analysis) # summary of RDA analysis
vif.cca(results.analysis) # co-linear coeffecience (the small the better, need < 10, other wise remove some varaibles)
coef(results.analysis)    # the slope of every variables to each axies

# R2 and Adj R2
R2 <- RsquareAdj(results.analysis)$r.squared;R2 #R2
R2adj<-RsquareAdj(results.analysis)$adj.r.squared;R2adj #Adj R2

# significant test
anova.cca(results.analysis, step=1000)            # Permutation test of result of RDA
anova.cca(results.analysis, by="axis", step=1000) # The significance of every axies
anova.cca(results.analysis, by="term", step=1000) # The sig. of every each variable
envfit(results.analysis,env,choices=1)            # which variables is sig. correlated with axie 1 (choices parameter)

# plot of CCA
 # We have 3 ways to plot CCA

#1. Default CCAA plot (Basic)
plot(results.analysis,scaling = 2,main=title,type="p")

#2. A little bite better version
plot(results.analysis
     ,scaling = 3     # scale follow both site and specie
     ,main=title      # Title of plot
     ,type="p"        # p = point
     ,display=c("bp"  # bp=axies
               ,"cn"  # cn=variables and arrow
               ,"wa" # wa=biotic data (site)
     ))

#3. Draw the plot step by step from blank
  # draw a blank
  pl <- plot(spe.cca, type="n", scaling=3, correlation=TRUE)
  # plot "site" 
  points(pl, "site", pch=2, col=1,cex=1.2)
  # intrude the family grp to saperate the species by grp (ben.family.grp)
  species.name <-rownames(pl$species)
  ben.family.grp <- substring(species.name,1,4)# taking the first four character of speceis name
  ben.family.grp <- as.factor(ben.family.grp)  # translate string into factors
  
  with(ben.c                       # use ben.c
       , points(pl, "species"      # species in pl  
                ,pch=21,cex=0.8
                , col=NULL,bg=ben.family.grp 
                # background based on 'ben.family.grp'
                # and no outline color because 'col=NULL'
                ))
  
  legend("bottomright"
         , levels(ben.family.grp)
         , pch=21, pt.bg=1:10
         , bty="n")             # legend has no border
  
  # draw the arrows of environmetal factors
  text(spe.cca, display ="cn", scaling=3)# cn=variables and arrow