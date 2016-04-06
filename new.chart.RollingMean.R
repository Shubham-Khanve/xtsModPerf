new.chart.RollingMean <-
function (R, width = 12, xaxis = TRUE, ylim = NULL, lwd=c(2,1,1), ..., fill = NA)
{
	x = checkData(R)

    columnnames = colnames(x)
	
#sd.xts is introduce
	
	sd.xts <- xts:::sd.xts
	
############################################################################


    x.mean = rollapply(x[,1,drop=FALSE], width = width, FUN = "mean", fill=fill, align = "right")
    x.stdev = rollapply(x[,1,drop=FALSE], width = width, FUN = "sd.xts", fill=fill, align = "right")
	
    lower.band = x.mean - 2 * x.stdev/sqrt(width)
    upper.band = x.mean + 2 * x.stdev/sqrt(width)

    result = merge(x.mean,lower.band,upper.band)

    if(is.null(ylim[1]))
        ylim = range(result, na.rm=TRUE)

    freq = periodicity(R)

    switch(freq$scale,
        minute = {freq.lab = "minute"},
        hourly = {freq.lab = "hour"},
        daily = {freq.lab = "day"},
        weekly = {freq.lab = "week"},
        monthly = {freq.lab = "month"},
        quarterly = {freq.lab = "quarter"},
        yearly = {freq.lab = "year"}
    )

    main = paste(columnnames[1], " Rolling ",width,"-",freq.lab," Performance",sep="")

############################################################################
 
#chart.TimeSeries replaced by new.chart.TimeSeries

 new.chart.TimeSeries(result, ylim = ylim, xaxis = xaxis, ylab = "Return", lty = c(1,2,2), colorset = c("black","darkgray","darkgray"), main=main, ... = ...)
 
############################################################################

}

