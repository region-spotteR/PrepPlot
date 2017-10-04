#' Calculate the x and y circle coordinates of any input data
#' 
#' This function calculates x and y circle coordinates of any input data for a spider/radar plot. It needs a dataframe as input. It is inspired by the ggradar-package, but much faster. For more info see ...
#' @param df A data.frame where the first column are the id-names and any column after that is either numeric or an integer.
#' @export
#' @examples
#' CalcGroupPath()

CalcGroupPath <- function(df) {
  ## 1. Stop rules
  if(!is.data.frame(df)) stop("df must be a data.frame")
  mycols<-unlist(lapply(df[,-1],class))%in%c('numeric','integer')
  if(sum(mycols)!=(ncol(df)-1)) stop("Not all columns after the second columns are of classes integer or numeric.")
  ## 2. Function
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1)) # find increment
  xx<-c(rbind(t(df[,-1])*sin(angles[-ncol(df)]),t(df[,2])*sin(angles[1]))) # The first observation needs to be repeated to allow the lines to close
  yy<-c(rbind(t(df[,-1])*cos(angles[-ncol(df)]),t(df[,2])*cos(angles[1])))
  graphData<-data.frame(group=rep(as.character(df[,1]),each=ncol(df)),x=(xx),y=(yy))
  return(graphData)
}

#' Calculate the axis path
#' 
#' This function calculates the axis lines for a spider/radar plot. It is inspired by the ggradar-package, but a bit faster. For more info see ...
#' @param var.names A character vector or factor of variable names
#' @param min A single numeric. The minimum value for the axis.lines. Should correspond to `center` in CalcCircleCoords
#' @param max A single numeric. The maximum value for the axis.lines. Should be maximum value of your desired grid
#' @export
#' @examples
#' CalcAxisPath()

CalcAxisPath <- function(var.names,min,max) {
  ### 1. Stop rules
  if(length(min)!=1 | !is.numeric(min)) stop("min must be a single numeric")
  if(length(max)!=1 | !is.numeric(max)) stop("max must be a single numeric")
  if(!is.character(var.names) & !is.factor(var.names)) stop("var.names must be a character or factor vector")
  
  ### 2. The function 
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

#' Create circle coordinates 
#' 
#' This function returns a dataframe with x, y coordinates of a circle depending on the chosen center, r and number of points. It is the same function as in the ggradar-package, except for the documentation.
#' @param center A single numeric. Giving the center of the circle
#' @param r A single numeric. Describing the radius of the circle
#' @param npoints A single numeric. The number of points the circle consists of
#' @export 
#' @examples
#' CalcCircleCoords()

CalcCircleCoords <- function(center = centre.y, r = 1, npoints = ncol(plot.data)){
  ## 1. Stop rules
  if(length(center)!=1 | !is.numeric(center)) stop("center must be a single numeric")
  if(length(r)!=1 | !is.numeric(r)) stop("r must be a single numeric")
  if(length(npoints)!=1 | !is.numeric(npoints)) stop("center must be a single numeric")
  
  ## 2. The function
  #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  tt <- seq(0,2*pi,length.out = npoints)
  yy <- center + r * cos(tt)
  xx <- center + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


#' Create line data to plot multiple boxplots on the x-line
#' 
#' This function returns a list with a dataframe containing the names of the box-lines and another dataframe containing the outlier points
#' @param df A data.frame or a matrix
#' @param y_range A numeric or integer vector with a length equaling the amount of df columns
#' @param start_id A single whole numeric allowing to set the first id-name
#' @param q_type A integer between 1 and 9 allowing you to choose the type of quantile algorithm used. See ?quantile for more info. Default is 5.
#' @param q_width A single numeric giving the width of the box
#' @param whisker_width The width of the whisker. Default is NULL. It will then take half the box's width. 
#' @export 
#' @examples
#' 
#' ## Create sample data
#' tt=data.frame(v1=rbeta(62,4.4,4.1),v2=rbeta(62,4.4,4.4),v3=rbeta(62,4,4.4),v4=rbeta(62,4.4,4.4),
#'               v5=rbeta(62,4.4,4.1),v6=rbeta(62,4.4,4.4),v7=rbeta(62,4,4.4),v8=rbeta(62,4.4,4.4))
#' 
#' create_box_data(tt,y_range=0:7,start_id=11,q_type=5,q_width=0.3,whisker_width=0.22)              

create_box_data<-function(df,y_range,start_id,q_type=5,q_width,whisker_width=NULL){
  ### 1.0 Create some stop rules to avoid breakdown
  if(length(start_id)!=1 | start_id%%2%in%c(0,1)==FALSE) stop("start_id must be a single whole number")
  if(length(q_width)!=1 | !is.numeric(q_width)) stop("q_width must be a single numeric")
  if(!is.null(whisker_width)){
    if(length(whisker_width)!=1 | !is.numeric(whisker_width)) stop("whisker_width must be a single number")  
  }
  if(length(q_type)!=1 | !q_type%in%c(1:9)) stop("q_width must be a single whole number between 1 and 9")
  if(!is.data.frame(df) & !is.matrix(df)) stop("df must be a data.frame or a matrix")
  if(!is.integer(y_range) & !is.numeric(y_range)) stop("y_range must be a numeric or integer vector")
  if(length(y_range)!=ncol(df)) stop("y_range is too long or too short. Anyways it is not equal to the amount of df-columns")
  
  ### 1.1 Create short handles for all the inputs
  y<-y_range
  n<-length(y) # length of the id parameters minus one
  s<-start_id
  qw<-q_width/2
  if(!is.null(whisker_width)){ww<-whisker_width/2} else {ww=qw/2}
  
  ### 1.2 Calculate the quantiles of the dataframe
  btt<-t(sapply(1:ncol(df),function(j) boxplot.stats(df[,j])$stats))
  
  ### 1.3 Calculate the vertical quantile lines inside the box as well as the box
  iqr_df<-data.frame(x=rep(c(btt[,c(2,4)]),2),y=c(rep(y-qw,2),rep(y+qw,2)),
                     id=paste('Boxplots',c(rep(s:(s+n-1),2),rep((s+n):(s+2*n-1),2)),sep='')) # box lines for boxplot
  q_lines<-data.frame(x=rep(c(btt[,-c(1,5)]),2),y=c(rep(y-qw,3),rep(y+qw,3)),
                      id=paste('Boxplots',rep((s+2*n):(s+5*n-1),2),sep='')) # quantile lines for boxplot
  
  ### 1.4 Calculate the lines to the whiskers and the whisker lines
  lines_to_whisker<-data.frame(x=c(btt[,c(2,4,1,5)]),y=rep(y,4),
                               id=paste('Boxplots',rep((s+5*n):(s+7*n-1),2),sep='')) # lines to the whiskers
  whiskers<-data.frame(x=rep(c(btt[,c(1,5)]),2),y=c(rep(y-ww,2),rep(y+ww,2)),
                       id=paste('Boxplots',rep(c((s+7*n):(s+9*n-2),''),2),sep='')) # only whiskers up to here
  
  ### 1.5 Merge the lines together into one data.frame; create the outputlist `box`
  box<-NULL
  box$lines<-rbind(iqr_df,q_lines,lines_to_whisker,whiskers)
  
  ### 1.6 Calculate the outlier points
  listres<-lapply(1:ncol(df),function(j) boxplot.stats(df[,j])$out)
  myrows<-unlist(lapply(listres,function(x) length(x)))
  if(sum(myrows)>0){
    box$points<-data.frame(x=unlist(listres),y=rep(y,times=myrows),
                           key=rep(colnames(df),times=myrows))
  } else {(box$points<-NULL)}
  return(box)
}
