new.charts.TimeSeries <-
function (R,  space = 0, main = "Returns", ...)
{
    R = checkData(R)
	
    columns = NCOL(R)
    columnnames = colnames(R)

    ymax = max(R, na.rm=TRUE)
    ymin = min(R, na.rm=TRUE)

    op <- par(oma = c(2,0,4,0), mar=c(0,4,0,4))
    layout(matrix(c(1:columns), ncol = 1, byrow = TRUE), widths=1)
    xaxis=FALSE
    yaxis=TRUE
	
#even function introduced	
	
	even <- function (x) {
		x%%2 == 0
	    }
		
##############################################################################	
	
    for(i in 1:columns){
         if(even(i))
            yaxis.right=TRUE
         else
             yaxis.right=FALSE
        if(i==columns)
            xaxis = TRUE
		
		#chart.TimeSeries replaced by new.chart.TimeSeries		
			
        print(new.chart.TimeSeries(R[,i,drop=FALSE],  xaxis=xaxis, main="", ylab=colnames(R)[i], ylim = c(ymin,ymax), yaxis=yaxis, yaxis.right=yaxis.right, ...))
		
		########################################################################
		
        if(i==1)
            yaxis=FALSE
    }

    mtext(main,
        side = 3, outer = TRUE, 
        font = 2, cex = 1.2, line=1)
    par(op)
    

}

