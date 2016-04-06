new.chart.RelativePerformance <-
function (Ra, Rb, main = "Relative Performance", xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ylog = FALSE, elementcolor = "darkgray", lty = 1, cex.legend=.7, ...)
{ 
    Ra = checkData(Ra)
    Rb = checkData(Rb)

    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    for(column.a in 1:columns.a) { 
        for(column.b in 1:columns.b) { 
            merged.columns = merge(Ra[, column.a, drop = FALSE], Rb[, column.b, drop = FALSE])
            cumulative = cumprod(1+na.omit(merged.columns))
            column.calc = cumulative[,1,drop=FALSE]/cumulative[,2,drop=FALSE]
            colnames(column.calc) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = "/")
            if(column.a == 1 & column.b == 1)
                Result.calc = column.calc
            else
                Result.calc = merge(Result.calc,column.calc)
        }
    }
columnnames = colnames(Result.calc)

############################################################################
#chart.TimeSeries replaced by new.chart.TimeSeries

    print(new.chart.TimeSeries(Result.calc, xaxis = xaxis, main = main, colorset = colorset, ylog = ylog, lty = lty, ...)) #print
	
############################################################################
	
    abline(h=1,col=elementcolor)
    if(!is.null(legend.loc)){
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = cex.legend, border.col = elementcolor, lty = lty, lwd = 2, bg = "white", legend = columnnames)
    }
}