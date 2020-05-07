# Part II - Non-metric multidimensional scaling and analysis of similarity among groups

# data needed:
  # ben; grp; env

grp <- season.river
ben <- ben.river

library(vegan)

level <- levels(grp)
ben.lg <- log(ben+1)
Jac <- vegdist(ben.lg, method = "jaccard") # use 'Jaccard distance'

nmds <- metaMDS(Jac)                       # core code of nMDS
nmds$stress                                # take out the stress

score <- scores(nmds)               # take out the coordinates of each sites

nmds.coord <- as.data.frame(score)  # convert coordinates to data frame

# There are two ways to plot nMDS:

plot(nmds,type="t") # default plot

plot(nmds.coord     # use the coordinate take out from nMDS 
     ,col=grp       # separate the sites use different color based on grp
     ,pch=16)       # 'pch' is the shape of the site


# plot correlations between env and ben data - be very similar with CCA
ef<-envfit(nmds,env.river,na.rm=TRUE)# conections to environmental variable
ef                                   # see which variabels in env is significant
plot(ef)                             # plot all the variables  (CCA)
plot(ef
     ,p.max=0.05                     # plot variables p < 0.05
     ,col="black")                   # with black

# draw the legend
legend("topleft"
       ,level                  # label you want to show in the nMDS
       ,pch=16                 # 'pch' is the shape of the site
       ,col=c('XX1','XX2',...) # using color exactly in the plot
       ,bty = "n"              # box type - 'n' if default, 'o' for no box line for legend
       ,bg="white"             # color of the legend background 
       ,box.col = "black"      # Color of the legend border
       ,cex=0.5)               # size of the labels and sites

text(x=X1,y=Y1       # write a text at corrdinate (x,y)
     ,"stress=0.16") # plot stress at corrdinate (x,y)