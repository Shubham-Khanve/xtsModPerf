new.chart.Drawdown <-
    function (R, geometric = TRUE, legend.loc = NULL, colorset = (1:12), ...)
    { 
        Drawdowns <-
            function (R, geometric = TRUE, ...)
            {   x = checkData(R)
            
            columns = ncol(x)
            columnnames = colnames(x)
            
            colDrawdown <- function(x, geometric) {
                if(geometric)
                    Return.cumulative = cumprod(1+x)
                else
                    Return.cumulative = 1+cumsum(x)
                maxCumulativeReturn = cummax(c(1,Return.cumulative))[-1]
                column.drawdown = Return.cumulative/maxCumulativeReturn - 1
            }
            
            for(column in 1:columns) {
                column.drawdown <- na.skip(x[,column],FUN=colDrawdown, geometric = geometric)
                
                if(column == 1)
                    drawdown = column.drawdown
                else
                    drawdown = merge(drawdown,column.drawdown)
            }
            
            colnames(drawdown) = columnnames
            drawdown = reclass(drawdown, x)
            return(drawdown)
            }
        
        #source= https://github.com/cran/PerformanceAnalytics/blob/master/R/Drawdowns.R
        
        na.skip <- function (x, FUN=NULL, ...) 
        {   
            nx <- na.omit(x)
            fx <- FUN(nx, ... = ...)
            if (is.vector(fx)) {
                result <- .xts(fx, .index(x), .indexCLASS = indexClass(x))
            }
            else {
                result <- merge(fx, .xts(, .index(x)))
            }
            return(result)
        }
        
        #source= https://r-forge.r-project.org/scm/viewvc.php/pkg/PerformanceAnalytics/R/na.skip.R?view=markup&root=returnanalytics
        
        y = checkData(R)
        drawdown = Drawdowns(y, geometric)
        
        if(NCOL(y)==1)
        {
            drawdown<-as.xts(drawdown)
            colnames(drawdown)<-colnames(y)
        }
     
##########################################################################
	 
        # Here new.chart.TimeSeries() function is used which is based on plot.xts rather than base graphics
        new.chart.TimeSeries(drawdown, colorset = colorset, legend.loc = legend.loc, ...)
		
###########################################################################
        
    }

