# Part III - Density of each Class and Order among groups

# data needed:
# ben, grp and ben.initial
# '...' means you need to keep writing the code like you wrote just now
# 'XX' means you need to write yourself based on your wish

# Done 'ben.initial' in Excel:
  # classify the species into classes and orders one by one
  # save them in 'ben.initial' as group factors
    # such as c('Diptera','Diptera','Ephemeroptera','Ephemeroptera'...)
    # Attention: 'ben.initial' has to against to 'rownames(ben)'

level <- levels(grp)
initial <- ben.initil    # save the initial

# Category abundance of one Order/Class into one matrix
{
  Gastropoda      <- ben[initial=="Gast",]  # Pay attention to the '[,]'
  Lamellibranchia <- ben[initial=="Lame",]
  Crustacea       <- ben[initial=="Crustacea",]
  Coleoptera      <- ben[initial=="Coleoptera",]
  # ...
}

data.list <- list(Gastropoda,Lamellibranchia,Crustacea,Coleoptera,...)  # put them into the list
data.list  # see the list looks like

# Give one name to each list in 'data.list' 
title.list <- c("¸¹×ã¸ÙGastropoda","°êÈù¸ÙLamellibranchia",
                "Èí¼×¸ÙCrustacea","ÇÊ³áÄ¿Coleoptera",...)

sample.area <- XX # sampling area at each sites (assuming each sites have the same area)
# 'abundance/sample.are' to calculate the density

# Tricky move:
  # set ylim after the barplot
ylim=c(XX,XX,...)

par(mfrow=c(3,4)) # put 3 x 4 = 12 panel in one picture

# define vairables - median, minus and maximum
results.Mean.sd.Min.Max.Oct <- NULL;results.Mean.sd.Min.Max.Jan <- NULL;
results.Mean.sd.Min.Max.Apr <- NULL
i=0;
repeat{
  if(i==length(title.list))break     # jump out of the loop
  i=i+1
  data <- as.data.frame(data.list[i])# Tabke out one dataset per time and translate into data frame
  # have to transferm into data.frame
  
  # For each Order/Class, separate them into seasons based on 'grp'
    # convert them to the linear
    # and calculate the density
  data.season1 <- as.matrix(data[,grp==level[1]])[TRUE]/sample.area
  data.season2 <- as.matrix(data[,grp==level[2]])[TRUE]/sample.area
  data.season3 <- as.matrix(data[,grp==level[3]])[TRUE]/sample.area
  data.seasonX <- as.matrix(data[,grp==level[X]])[TRUE]/sample.area
  ... # if you have other seasons, keep writing or write a loop
  
  data <- c(data.season1,data.season2,data.season3,...)
  grp.data <- factor(c(rep("season1",length(data.season1)),
                       rep("season2",length(data.season2)),
                       rep("season3",length(data.season3)),
                       rep("seasonX",length(data.seasonX)),
                       ...),
                     levels = c("season1","season2","season3",...))
  
  #plot(data ~ grp.data,main=title.list[i],xlab="",ylab = "")
  
  # Calculate standard deviation (SD)
  sd.season1 <- round(sd(data.season1),2) # Control the friction up to 2
  sd.season2 <- round(sd(data.season2),2)
  sd.season3 <- round(sd(data.season3),2)
  ...

  # Calculate standard error (SE): SE = SD/( n^(1/2) )
  se.season1 <- sd.Oct/(length(data.season1))**(1/2)
  se.season2 <- sd.Jan/(length(data.season2))**(1/2)
  se.season3 <- sd.Apr/(length(data.season3))**(1/2)
  ...
  
  # Calculate mean, min and max of the dataset based on season
  Mean.season1 <- round(mean(data.season1),2);Min.season1 <- min(data.season1);Max.season1 <- max(data.season1)
  Mean.season2 <- round(mean(data.season2),2);Min.season2 <- min(data.season2);Max.season2 <- max(data.season2)
  Mean.season3<- round(mean(data.season3),2);Min.season3 <- min(data.season3);Max.season3 <- max(data.season3)
  ...
  
  # boxplot and errorbar (mean+-SE)
    # use arrow to draw SE: SE = SD/[n^(1/2)]
  barplot.Mean.SE <- data.frame(
    mean=c(Mean.season1,Mean.season2,Mean.season3), # a column names Mean
    se=c(se.season1,se.season2,se.season3)          # a column names SE
  )
  rownames(barplot.Mean.SE) <- c("season1","season2","season3") # define the row name
  
  # barplot and add error bar (errror bars were drawn by arrow() )
  { # barplot
    b <- barplot(barplot.Mean.SE$mean                   # use the mean to draw barplot
                 ,names.arg = rownames(barplot.Mean.SE) # xlab of the each bar
                 ,main=title.list[i]                    # Order/Class name of the plot
                 ,ylim=c(0,ylim[i])                     # ylim is the tricky move at line 34-36
                 ,ylab='ÃÜ¶ÈDensity (ind/m2)')
    # error bar
    arrows(b[1],barplot.Mean.SE$mean[1] # from (x1,y1); x1=b[1],y1 = barplot.Mean.SE$mean[1]
           ,b[1] ,barplot.Mean.SE$mean[1]+barplot.Mean.SE$se[1]
                                        # to (x2,y2); 
                                        # x2=b[1],y2 = y1 + barplot.Mean.SE$se[1]
           ,angle = 90,cex=0.4)

    arrows(b[2],barplot.Mean.SE$mean[2]
           ,b[2],barplot.Mean.SE$mean[2]+barplot.Mean.SE$se[2]
           ,angle = 90,cex=0.4)

    arrows(b[3],barplot.Mean.SE$mean[3]
           ,b[3],barplot.Mean.SE$mean[3]+barplot.Mean.SE$se[3]
           ,angle = 90,cex=0.4)
    ...
  }
  
# Significant test
  
  # ready for wilcox test
  condition1="less";if(Mean.Oct >= Mean.Jan){condition1="greater"} # 'greater' to which is great
  condition2="less";if(Mean.Oct >= Mean.Apr){condition2="greater"}
  condition3="less";if(Mean.Jan >= Mean.Apr){condition3="greater"}
  ...
  
  # wilcox among seeasons and P-values
  O.J <- wilcox.test(data.Oct,data.Jan,alternative = condition1)
  O.J.Pvalue <- round(O.J$p.value,4)
  O.A <- wilcox.test(data.Oct,data.Apr,alternative = condition2)
  O.A.Pvalue <- round(O.A$p.value,4)
  J.A <- wilcox.test(data.Jan,data.Apr,alternative = condition3)
  J.A.Pvalue <- round(J.A$p.value,4)
  ...
  
  # document all the P-value
  wilcox <- c(O.J.Pvalue,O.A.Pvalue,J.A.Pvalue)
  
  # document the (1) order/class name; (2) mean, sd, min and max of the density of each order/class
  result.Oct <- c(title.list[i]
                  ,Mean.Oct
                  ,sd.Oct
                  ,Min.Oct
                  ,Max.Oct)
  result.Jan <- c(title.list[i],Mean.Jan,sd.Jan,Min.Jan,Max.Jan)
  result.Apr <- c(title.list[i],Mean.Apr,sd.Apr,Min.Apr,Max.Apr,wilcox)
  ...
  
  # document the final results
  results.Mean.sd.Min.Max.Oct <- rbind(results.Mean.sd.Min.Max.Oct
                                       ,result.Oct)
  results.Mean.sd.Min.Max.Jan <- rbind(results.Mean.sd.Min.Max.Jan,result.Jan)
  results.Mean.sd.Min.Max.Apr <- rbind(results.Mean.sd.Min.Max.Apr,result.Apr)
  ...
}
par(mfrow=c(1,1))
# Finish the loop

# Document the final results
result.F <- cbind(results.Mean.sd.Min.Max.Oct,results.Mean.sd.Min.Max.Jan,results.Mean.sd.Min.Max.Apr)

# column names of each results
colnames(result.F) <- c("1","season1.Mean",'sd',"Min","Max",
                        "6","season2.Mean",'sd',"Min","Max",
                        "11","season3.Mean",'sd',"Min","Max",
                        ...
                        ,"season1-season2","season1-season3","season2-season3"
                        ,...)

rownames(result.F) <- result.F[,1]
result.F[,-c(1,6,11,...)]           # simplyfied the final results

# write down the results into .csv file in working direction
write.csv(result.F[,-c(1,6,11,...)],"Compare abund of taxa among seasons.csv")