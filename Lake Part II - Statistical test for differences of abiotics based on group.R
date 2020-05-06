# Part II - Statistical test for differences of abiotics based on group

# Data needed:
# env; grp

mean <- aggregate(env,by=list(grp),FUN=mean)# variables'sd based on group
mean

sd <- aggregate(env,by=list(grp),FUN=sd)# variables' sd based on group
sd
# Operate in the Excel to get:
  # table 3-1 Mean ¡À SD values of hydrological and water chemistry ...

ylim= XX # fill in the XX after you see how boxplot goes
title=c('TN','TP','pH','XX') # fill in the title follow the colname of env
par(mforw=c(3,2)) # can plot 3x2=9 pannel in one picture, 3 row, 2 column
i=0
repeat{
  i=i+1
  
  # plot the variables based on group factor
  boxplot(env.river[,i]~grp
          ,main=title[i]        # title of the plot
          ,ylim=c(0,ylim)       # limit of the y-axis
          ,ylab = "",xlab = "") # label of the x- and y-axis
  
  # significant test among groups by TueyHSD test (>= 3 gorups)
  mod <- aov(env[,i]~grp)
  tuk <- TukeyHSD(mod)
  plot(tuk)
  
  if(i == length(colnames(env)) )break 
  # if i equal to number of the variables(TP,TN...) in env, then quit this loop
}
par(mforw=c(1,1)) # get back to normal
i=NULL # set i free, in case disturbance the following codes