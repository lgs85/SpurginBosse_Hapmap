

# Structure bar plot ------------------------------------------------------

structureplot <- function(str_out,pops,k,xaxis = T)
{
  #Check the number of k specified in the input matches the data
  dataK <- ncol(str_out)
  if(dataK != k) stop('The specified number of clusters does not match the data')
  
  #Sort columns by prevalence
  str_out <- str_out[,order(apply(str_out,2,sum),decreasing = T)]
  
  #Add a population column
  str_out$pop <- pops[,1]
  
  #Add a column saying which is the largest cluster for each ind and pop.
  #This is useful for sorting
  str_out$maxK <- apply(str_out[,1:k],1,which.max)
  
  maxKmode <- tapply(str_out$maxK,str_out$pop,function(x) as.numeric(names(table(x))[which.max(table(x))]))[str_out$pop]
  
  str_out$maxKpop <- NA
  
  for(i in 1:length(maxKmode))
  {
    str_out$maxKpop[i] <- str_out[i,maxKmode[i]]
  }
  
  #Order by population, then by maxK
  str_out <- str_out[order(str_out$pop,str_out$maxKpop),]
  
  #Specify some nice colours
  mycols <- c("#a6cee3",
              "#1f78b4",
              "#b2df8a",
              "#33a02c",
              "#fb9a99",
              "#e31a1c",
              "#fdbf6f",
              "#ff7f00",
              "#cab2d6",
              "#6a3d9a")[1:k]
  
  #Now draw the barplot
  
  barplot(t(str_out[,1:k]),col = mycols,
          border = mycols,
          space = 0,
          axes=F, 
          axisnames = F,
          xpd=F)
  
  
  #Add some lines to seperate the populations
  pop_pos <- cumsum(tapply(1:nrow(str_out),str_out$pop,length))
  abline(v=c(0,pop_pos),lwd=1.2)
  
  #X axis labels are a bit more complicated
  #as we wan't them in the middle of each population
  #Calculate that using a loop
  
  labpos <- pop_pos
  
  for(i in 1:length(pop_pos))
  {
    if(i == 1)
    {
      labpos[i] <- pop_pos[i]/2
    } else
    {
      labpos[i] <- pop_pos[i-1] + (pop_pos[i]-pop_pos[i-1])/2
    }
  }
  
  #Add the axis and the text.
  #Tick marks can go at the end of each pop
  
  if(xaxis == T)
  {
    
    text(labpos, par("usr")[3]-0.03, 
         srt = 60, adj= 1, xpd = TRUE,
         labels = levels(str_out$pop),
         cex = 0.7)
  }
}





# Structure bar plot 2 ------------------------------------------------------

structureplot2 <- function(str_out,pops,k,xaxis = T)
{
  #Check the number of k specified in the input matches the data
  dataK <- ncol(str_out)
  if(dataK != k) stop('The specified number of clusters does not match the data')
  
  #Sort columns by prevalence
  str_out <- str_out[,order(apply(str_out,2,sum),decreasing = T)]
  
  x <- melt(as.matrix(str_out)) %>%
    transform(X1 = factor(X1, rownames(str_out)),
              X2 = factor(X2, colnames(str_out)),
              value = as.numeric(paste(value)))
  
  x$pop <- rep(pops[,1],k)
  str_out$pop <- pops[,1]
  pop_pos <- cumsum(tapply(1:nrow(str_out),str_out$pop,length))
  labpos <- pop_pos
  
  for(i in 1:length(pop_pos))
  {
    if(i == 1)
    {
      labpos[i] <- pop_pos[i]/2
    } else
    {
      labpos[i] <- pop_pos[i-1] + (pop_pos[i]-pop_pos[i-1])/2
    }
  }
  
  labpos <- round(labpos,0)
  
  ggplot(x,
         aes(x = X1,y = value, fill = X2))+
    geom_bar(stat = "identity",width = 1)+
    theme_bw()+
    scale_x_discrete(breaks = labpos,labels = names(pop_pos))+
    scale_y_continuous(expand = c(0,0))+
    geom_vline(xintercept = pop_pos)+
    theme(axis.text.x = element_text(angle = 90),
          axis.ticks = element_blank(),
          legend.position = "none")
}
