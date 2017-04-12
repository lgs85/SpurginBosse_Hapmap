

# Structure bar plot------------------------------------------------------

structureplot <- function(str_out,pops,k)
{
  #Sort columns by prevalence
  str_out <- str_out[,order(apply(str_out,2,sum),decreasing = T)]
  
  x <- melt(as.matrix(str_out)) %>%
    transform(X1 = factor(X1, rownames(str_out)),
              X2 = factor(X2, colnames(str_out)),
              value = as.numeric(paste(value)))
  
  x$pop <- rep(pops[,1],k)
  str_out$pop <- pops[,1]
  x$X3 <- factor(paste(letters[as.numeric(x$pop)],x$X1,sep = "_"))
  x <- x[order(x$X3),]
  
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
  
  output <- ggplot(x,
         aes(x = as.numeric(X3),y = value, fill = X2))+
    geom_bar(stat = "identity",width = 1)+
    theme_bw()+
    scale_x_continuous(breaks = labpos,labels = names(pop_pos),expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    geom_vline(xintercept = pop_pos)+
    xlab("")+
    ylab("")+
    theme(axis.text.x = element_text(vjust = 0.1,hjust = 1,angle = 90),
          axis.ticks = element_blank(),
          legend.position = "none")+
    scale_fill_manual(values = mycols)
  
  return(output)
}

