# Part VI - Generalized linear model for environmental distance and geological distance against community dissimilarity

# Data needed:
  # ben; env; grp
  # grp.env - grp.env is to separate the env into 
    # habitat variables (water chemistry and physical vairable)
    # hydrological variables (Current velocity and water depth)
    # geologicla distnace
      # For example:
        # if the variables names in 'env' is : 'TN', 'TP', 'COD', 'WD', 'CV', 'Lat', 'Lot'
        # grp.env <- c('HAB','HAB','HAB','HYD','HYD','GEO','GEO')

level <- levels(grp)

i=0; result.F <- NULL
repeat{
  i=i+1
  
  # take out dataset at a time  
  ben.i <- ben[grp==level[i],]
  env.i <- env[grp==level[i],]
  title <- level[i]
  
  # remove the empty line
    # Why we are removing the empty line?
    # Becuase, Community dissimilarity between one empty sites and a non-empty sites are 1
      # However, this is not true.
  ben.i <- ben.i[rowSums(ben.i)>0,]
  env.i <- ben.i[rowSums(ben.i)>0,]

  # Standardization of the data, log transfer and sd=1, mean=0
  ben.lg <- log(data.i+1)
  
  hab.scale <- scale(env.i[,grp.env=="HAB"]
                     ,scale = TRUE          # sd=1
                     ,center = TRUE)        # mean=0
  hyd.scale <- scale(env.i[,grp.env=="HYD"]
                     ,scale = TRUE          # sd=1
                     ,center = TRUE)        # mean=0
  
  # Calculate Commu.dis and distance matrics
  commu.dis <- vegdist(ben.lg, method='chao') # I like chao dissimilarity
  commu.dis <- as.vector(commu.dis)
  
  habitat.dis <- dist(hab.scale,method = 'euclidean',diag = FALSE,upper = FALSE);habitat.dis<-as.vector(habitat.dis)
  hydro.dis <- dist(hyd.scale,method = 'euclidean',diag = FALSE,upper = FALSE);hydro.dis<-as.vector(hydro.dis)
  geo.dis <- dist(env.i[,grp.env=="GEO"],method = 'euclidean',diag = FALSE,upper = FALSE);geo.dis<-as.vector(geo.dis)
  
  # 3.2.2 lm --------------------
  lm <- glm(Commu.dis~hydro.dis+habitat.dis+geo.dis) # fit GLM
  result <- list(title,summary(lm))              # Document the result
  result.F <- list(result.F,result)              # Document the result
  
  if(i==length(level)) break
}