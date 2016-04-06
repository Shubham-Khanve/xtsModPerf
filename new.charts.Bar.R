new.charts.Bar <-
function (R, main = "Returns", cex.legend = 0.8, cex.main=1, ...)
{ 
    R = checkData(R)
	
#even function introduced
  even <- function (x) {
		x%%2 == 0
	    }
############################################################################

    columns = NCOL(R)
    columnnames = colnames(R)

    ymax = max(R, na.rm=TRUE)
    ymin = min(R, na.rm=TRUE)

    layout(matrix(c(1:columns), ncol = 1, byrow = TRUE), widths=1)
    op <- par(oma = c(5,0,4,0), mar=c(0,4,0,4))
    xaxis=FALSE
    yaxis=TRUE
    for(i in 1:columns){
         if(even(i))
            yaxis.right=TRUE
         else
             yaxis.right=FALSE
        positives = R[,i,drop=FALSE]
        for(row in 1:length(R[,i,drop=FALSE])){ 
            positives[row,]=max(0,R[row,i])
        }
        negatives = R[,i,drop=FALSE]
        for(row in 1:length(R[,i,drop=FALSE])){ 
            negatives[row,]=min(0,R[row,i])
        }
        if(i==columns)
            xaxis = TRUE
        
		
		############################################################################
		#chart.TimeSeries replaced by new.chart.TimeSeries
		
		print(new.chart.TimeSeries(positives, type = "h", lend="butt", xaxis=xaxis, main="", ylab="", ylim = c(ymin,ymax), yaxis=yaxis, yaxis.right=yaxis.right, colorset="darkgreen", lwd=2, ...))
		
		############################################################################
		
        lines(1:length(R[,1]), negatives, type="h", lend="butt", colorset="darkred", lwd=2)
        text(1, ymax, adj=c(0,1.2), cex = 0.8, labels = columnnames[i])

        if(i==1)
            yaxis=FALSE
    }

    mtext(main,
        side = 3, outer = TRUE, 
        font = 2, cex = cex.main, line=1)
    par(op)
    
}

