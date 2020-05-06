# Part III - Species accumulation curves among groups

# Data needed:
  # ben; grp
library(vegan)

# 'season1' is a factor in grp
ben.season1 <- ben[grp == "season1",] # take out the season based on grp factor

rarefaction.season1 <- specaccum(ben.season1, method = "rarefaction"
                            , permutations = 1000,conditioned =TRUE
                            , gamma = "chao1",  w = NULL)

plot(rarefaction.season1
     , add = FALSE, random = FALSE
     ,col = "Blue", lty = 1       # the color of the accmulation curve
     ,ci = 2, ci.type = c("line") # setting of the ci (confidence interval)
     ,ci.col = "red", ci.lty = 2  # setting of the ci
     , ci.length = 10             # setting of the ci
     ,xlim=c(0,300), ylim=c(0,35),# limits of x- and y- axis
     xvar = c("individuals"),     # based on "individual"
     main = "XX"                  # Title of this plot - usual function
     ) # ci - confidence interval)
