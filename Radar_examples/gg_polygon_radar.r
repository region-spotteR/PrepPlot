### Combine with all the code in the first chunk of shiny_example.Rmd to make it work
### 3.1. Create an empty theme for ggplot
theme_clear <- theme_bw(base_size=20) + 
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.key=element_rect(linetype="blank"))


### 3.2. Set the extent of the plot and the colors of the grid
plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
bg_colors<-c(RColorBrewer::brewer.pal(9,'Purples')[2],'#DFDFED') # Set the grid colors

### 3.3. Set up a basic empty gg-object
basething <- ggplot() + xlab(NULL) + ylab(NULL) + coord_fixed() +
  scale_x_continuous(limits=c(-plot.extent.x,plot.extent.x)) + 
  scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y)) + 
  theme_clear 

### 3.4 For the polygon-radar (spider-chart): Add the gridlines
n<-nrow(grid$all_lines)/length(grid$lines)        # n 
base_grid<-basething+geom_polygon(data=grid$all_lines[nrow(grid$all_lines):1,],aes(x,y,group=rev(q)),
                                  fill=c(rep(rep(rev(bg_colors),2),each=n),rep('white',n)))+
                     geom_polygon(data=grid$lines$q0,aes(x,y),fill="white")+
                     geom_path(data=grid$axis_path,aes(x=x,y=y,group=axis.no),
                                colour=axis.line.colour,alpha=0.4)

### 3.5 Add text to gg-object
font_size=2
base_text<-base_grid+geom_text(data=grid$all_lines,aes(x,y,label=values),size=font_size)+
                     geom_text(data=grid$axis_label,aes(x,y,label=text))

### 3.6 Add observation lines to gg-object
gg<-base_text+geom_path(data=xy_lines,aes(x=x,y=y,group=group,color=group),alpha=0.8,size=1)+
              theme(legend.position="bottom") # leave this line out if you want the legend right
