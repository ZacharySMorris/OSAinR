###Generating useful OSA plots###

## Sequence diagram
plot.OSA <- function(X,Sm_num,Ev_num){
        # X is an OSA plotting values object
        temp_X <- X$PlottingValues$X
        temp_Y <- X$PlottingValues$Y
        temp_bg <- X$colors
        temp_cex <- X$size
        temp_Sm_num <- Sm_num
        temp_Ev_num <- Ev_num

        tmp_Xlim<-c(0:(temp_Sm_num+1) )
        tmp_Ylim<-c(0:temp_Ev_num)

        plot(0, 0, type = "n",
             xlim = c(0,max(tmp_Xlim)),
             ylim = c(0,max(tmp_Ylim)+0.5),
             xlab = "",
             ylab = "Maturity Score",
             axes = FALSE,
             frame.plot = FALSE,
             asp=F)

        clip(0,max(tmp_Xlim),0,max(tmp_Ylim+0.5))

        y_bottom <- c(seq(1, temp_Ev_num, 2) - 0.5)
        y_top <- c(seq(1, temp_Ev_num, 2) + 0.5)

        rect(xleft = min(tmp_Xlim),
             ybottom = y_bottom,
             xright = max(tmp_Xlim),
             ytop = y_top,
             density = NULL,
             angle = 45,
             col = "light grey",
             border = NA
        )

        #axis(2, Ylim, pos=0, lwd.ticks = 0, las = 1)
        clip(0,max(tmp_Xlim),-0.5,max(tmp_Ylim+0.5))
        abline(v=0, lty="solid",lwd=1)
        axis(2, tmp_Ylim, pos=0, tick = FALSE, lwd.ticks = 0, las = 1)

        points(temp_X, temp_Y, pch = 21, bg = temp_bg, cex = temp_cex)

}
##

## real vs hypothetical hypercube plot

##

## event variability plot

plot.evar <- function(X){
        #X is an OSA event matrix
        plot(

        )
}

##



#Plot PCA alone#
plot.new()
par_default <- par()

Xlim<-c(0:(Sm_num+1) )
Ylim<-c(0:Ev_num)

par(par_default)
#pdf(file="PC1vPC2_DorsalSkull.pdf", width = 11, height = 8.5)

plot(0, 0, type = "n",
     xlim = c(0,max(Xlim)),
     ylim = c(0,max(Ylim)+0.5),
     xlab = "",
     ylab = "Maturity Score",
     axes = FALSE,
     frame.plot = FALSE,
     asp=F)


clip(0,max(Xlim),0,max(Ylim+0.5))

y_bottom <- c(seq(1, length(Color_Ontogeny.df$Events), 2) - 0.5)
y_top <- c(seq(1, length(Color_Ontogeny.df$Events), 2) + 0.5)

rect(xleft = min(Xlim),
     ybottom = y_bottom,
     xright = max(Xlim),
     ytop = y_top,
     density = NULL,
     angle = 45,
     col = "light blue",
     border = NA
     )

#axis(2, Ylim, pos=0, lwd.ticks = 0, las = 1)
clip(0,max(Xlim),-0.5,max(Ylim+0.5))
abline(v=0, lty="solid",lwd=1)
axis(2, Ylim, pos=0, tick = FALSE, lwd.ticks = 0, las = 1)

#axis(1, Xlim, pos=0, tick = FALSE, lwd.ticks = 0, las = 1)

points(X_position, Color_Ontogeny.df$MaturityScore, pch = 21, bg = Color_Ontogeny.df$colors, cex = Color_Ontogeny.df$size)

points(All_Semaphoronts.df$PlottingValues$X, All_Semaphoronts.df$PlottingValues$Y, pch = 21, bg = All_Semaphoronts.df$colors, cex = All_Semaphoronts.df$size)

points(All_Semaphoronts.df$PlottingValues$X[9:17], All_Semaphoronts.df$PlottingValues$Y[9:17], pch = 21, bg = All_Semaphoronts.df$colors[9:17], cex = All_Semaphoronts.df$size[9:17])



#testing code for plotting lines on sequence graph
names(All_Semaphoronts.df$PlottingValues$X)[order(All_Semaphoronts.df$MaturityScore)]

lines(All_Semaphoronts.df$PlottingValues$X[c(1,6)],
      All_Semaphoronts.df$PlottingValues$Y[c(1,6)],
      lty = 1,
      lwd = 16,
      col = "black")

lines(All_Semaphoronts.df$PlottingValues$X[c(1,6)],
      All_Semaphoronts.df$PlottingValues$Y[c(1,6)],
      lty = 1,
      lwd = 15,
      col = "light blue")


lines(All_Semaphoronts.df$PlottingValues$X,
      All_Semaphoronts.df$PlottingValues$Y,
      lty = 1,
      lwd = 5,
      col = "black")
