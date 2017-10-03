CalculateGroupPath4 <- function(df) {
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1)) # find increment
  xx<-c(rbind(t(plot.data.offset[,-1])*sin(angles[-ncol(df)]),t(plot.data.offset[,2])*sin(angles[1])))
  yy<-c(rbind(t(plot.data.offset[,-1])*cos(angles[-ncol(df)]),t(plot.data.offset[,2])*cos(angles[1])))
  graphData<-data.frame(group=rep(df[,1],each=ncol(df)),x=(xx),y=(yy))
  return(graphData)
}

CalculateAxisPath2 <- function(var.names,min,max) {
n<-length(var.names)
#Cacluate required number of angles (in radians)
angles <- seq(from=0, to=2*pi, by=(2*pi)/n)
#calculate vectors of min and max x+y coords
min.x <- min*sin(angles)
min.y <- min*cos(angles)
max.x <- max*sin(angles)
max.y <- max*cos(angles)
tmp<-lapply(1:n,function(i) matrix(c(i,i,min.x[i],max.x[i],min.y[i],max.y[i]),2,3))
res<-as.data.frame(do.call(rbind,tmp))
colnames(res) <- c("axis.no","x","y")
return(res)
}

funcCircleCoords <- function(center = centre.y, r = 1, npoints = ncol(plot.data)){
  #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  tt <- seq(0,2*pi,length.out = npoints)
  yy <- center + r * cos(tt)
  xx <- center + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
