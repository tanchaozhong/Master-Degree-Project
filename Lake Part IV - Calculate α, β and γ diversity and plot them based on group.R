# Part IV - Calculate ¦Á, ¦Â and ¦Ã diversity and plot them based on group

# Data needed:
# ben; grp
library(vegan)

ben.binary <- ben
ben.binary[ben.binary>0]=1 # values exceed 0, set them to 1

# ¦Á diversity
alpha <- rowSums(ben.binary)

# ¦Ã diversity
  # Create a function to calculate gamma diversity
gamma <- function(data,grp){
  gamma.middle1 <- aggregate(data,by=list(grp),FUN=sum) # Calculate rowsum based on grp
  rownames(gamma.middle1) <- gamma.middle1[,1]
  gamma.middle2 <- gamma.middle1[,-1] # remove the first column
  gamma.middle2[gamma.middle2>0]=1    # values exceed 0, set them to 1
  gamma <- rowSums(gamma.middle2)     # Calculate the gamma diversity
  return(gamma) 
  # without return(), there will be no results saved 
    # after running this function
}
gamma <- gamma(data=ben,grp=grp)

# ¦Â diversity
  # ¦Â diversity calculation is a little tricky
  
level <- levels(grp) 
level # see what grps are they
  
i=0
beta.grp<-NULL;beta<-NULL # we need 'beta.grp' and 'beta' later, so define it here
repeat{
  i=i+1
  # take out one dataset at a time to calculate
  data.o <- data[grp==level[o]]
  
  # standardize the data, log transform
  ben.lg <- log(data.0+1)
  
  # calculate beta diversity
  commu.dis <- vegdist(ben.lg, method='chao') # I like chao dissimilarity
  beta.vector <- as.vector(commu.dis)         # translate matrix into vector
  
  # document the results of beta and the grp
  title <- level[o]
  beta.grp <- c(beta.grp,rep(title,length(beta.vector)))
  beta <- c(beta,beta.vector)
  
  if(o==length(level))break # when calculate all the grps, end the loop
}
i = NULL # set i free

# Visualize the diversity pattern among groups
par(mfrow=c(1,2)) # Put two plot into one picture
boxplot(alpha~grp)
boxplot(beta~beta.grp)

# statistical analysis of differences of ¦Á, ¦Â diversity between groups
mod <- aov(alpha~grp);tuk <- TukeyHSD(mod);plot(tuk)
mod <- aov(beta~beta.grp);tuk <- TukeyHSD(mod);plot(tuk)

par(mfrow=c(1,1))# Give back everything is normal