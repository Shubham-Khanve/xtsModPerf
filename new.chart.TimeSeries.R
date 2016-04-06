new.chart.TimeSeries <- function( R, 
                                  auto.grid = TRUE,
								  xaxis = TRUE, 
								  yaxis = TRUE,
                                  yaxis.right = FALSE, 
                                  type = "l", 
                                  lty = 1, 
                                  lwd = 2, 
                                  las = par("las"),
                                  main = NULL,
                                  ylab=NULL,
								  xlab="",								  
                                  date.format.in="%Y-%m-%d",
                                  date.format = NULL,
                                  xlim = NULL,
                                  ylim = NULL, 
                                  element.color="darkgray",
                                  event.lines = NULL, 
								  event.labels = NULL, 
								  period.areas = NULL, 
								  event.color = "darkgray", 
								  period.color = "aliceblue", 
								  colorset = (1:12), 
                                  pch = (1:12),
                                  legend.loc = NULL,
                                  ylog = FALSE, 
                                  cex.axis=0.8, 
								  cex.legend = 0.8,
								  cex.lab = 1, 
								  cex.labels = 0.8, 
								  cex.main = 1,
                                  major.ticks = "auto",
								  minor.ticks=TRUE,
                                  grid.color="lightgray",
                                  grid.lty = "dotted",
                                  xaxis.labels = NULL, 
                                  ...){
								  
     y <- checkData(R) 
    
	 ######################################################################################################################
	 
	 plot.xts(y, 
              yaxis.right = yaxis.right, 
              type = type, 
              lty = lty, 
              lwd = lwd,
              xaxis.las = las,
              main = colnames(y)[1],
              ylim = ylim,
              col = colorset,
              legend.loc = legend.loc,
              cex.axis = cex.axis,
              grid.col = grid.color,
			  grid.ticks.lwd = 1,
              ...)
			  
	
	 ########################################################################################################################
	
 }