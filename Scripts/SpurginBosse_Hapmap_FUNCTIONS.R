

# Structure bar plot------------------------------------------------------

structureplot <- function(str_out,pops,k,xlab = T)
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
  
  if(xlab ==T)
  {
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
  } else
  {
    output <- ggplot(x,
                     aes(x = as.numeric(X3),y = value, fill = X2))+
      geom_bar(stat = "identity",width = 1)+
      theme_bw()+
      scale_x_continuous(breaks = labpos,labels = rep("",length(pop_pos)),expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      geom_vline(xintercept = pop_pos)+
      xlab("")+
      ylab("")+
      theme(axis.text.x = element_text(vjust = 0.1,hjust = 1,angle = 90),
            axis.ticks = element_blank(),
            legend.position = "none")+
      scale_fill_manual(values = mycols)
  }
  

  
  return(output)
}

# Standard error ----------------------------------------------------------

se <- function(x) sd(x)/sqrt(length(x))




# Rounding ----------------------------------------------------------------


#Round to 2 or 3 dp and keep trailing zeros
round2 <- function(x,lessthan=F)
{
  if(lessthan == T && x < 0.01)
  {
    return('< 0.01')
  } else
  {
    sprintf(round(x,2), fmt="%.2f")
  }
}

round3 <- function(x,lessthan = T)
{
  if(lessthan == T && x < 0.001)
  {
    return('< 0.001')
  } else
  {
    sprintf(round(x,3), fmt="%.3f")
  }
}


# Treemix -----------------------------------------------------------------

calcVarExplain <- function(stem){
  # read in two covariance matrices
  WHat = read.table(gzfile(paste(stem, ".cov.gz", sep = "")), as.is = T, 
                    head = T, quote = "", comment.char = "")
  W = read.table(gzfile(paste(stem, ".modelcov.gz", sep = "")), as.is = T, 
                 head = T, quote = "", comment.char = "")
  
  # set row and column names and order properly
  names(WHat) = rownames(WHat)
  names(W) = rownames(W)
  WHat = WHat[order(names(WHat)), order(names(WHat))]
  W = W[order(names(W)), order(names(W))]
  
  # calculate standard error of covariance matrix
  se = read.table(gzfile(paste(stem, ".covse.gz", sep = "")), as.is = T, 
                  head = T, quote = "", comment.char = "")
  seBar = apply(se, 1, mean)
  seBar = mean(seBar)
  
  # calculate residuals
  R = WHat - W
  
  # set residual lower matrix triangle to NA
  R[lower.tri(R, diag = TRUE)] <- NA
  
  # calculate mean residual across upper matrix triangle
  RBar = mean(as.matrix(R), na.rm = TRUE)
  
  # set data-estimated W(hat) matrix lower matrix triangle to NA
  WHat[lower.tri(WHat, diag = TRUE)] <- NA
  
  # calculate mean W(hat)
  WHatBar = mean(as.matrix(WHat), na.rm = TRUE)
  
  # calculate numerator of equation 30
  fNum = sum((R - RBar)^2, na.rm = TRUE)
  
  # calculate denominator of equation 30
  fDen = sum((WHat - WHatBar)^2, na.rm = TRUE)
  
  # calculate f in equation 30
  f = 1 - (fNum/fDen)
  
  outList = list("StdErr" = seBar, "VarExplain" = f)
  return(outList)
}

