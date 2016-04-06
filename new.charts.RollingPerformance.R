new.charts.RollingPerformance <-
function (R, width = 12, Rf = 0, main = NULL, event.labels = NULL, legend.loc=NULL, ...)
{ 
    x = checkData(R)
    colnames = colnames(x)
    ncols = ncol(x)

    if(is.null(main)){
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

      main = paste("Rolling",width,freq.lab, "Performance", sep=" ")
    }
    
    op <- par(no.readonly=TRUE)

    layout(matrix(c(1,2,3)),heights=c(1,0.75,1),widths=1)

    par(mar=c(1,4,4,2))
	
	############################################################################

	#charts.RollingPerformance replaced by new.chart.RollingPerformance
	
    print(new.chart.RollingPerformance(R, width = width, main = main, xaxis = FALSE, ylab = "Annualized Return", FUN = "Return.annualized", legend.loc = legend.loc, event.labels = event.labels, ...))#print

    par(mar=c(1,4,0,2))
    print(new.chart.RollingPerformance(R, width = width, main = "", xaxis = FALSE, ylab = "Annualized Standard Deviation", FUN = "StdDev.annualized", event.labels= NULL, ...))#print

    par(mar=c(5,4,0,2))
    print(new.chart.RollingPerformance(R, width = width, main = "", ylab = "Annualized Sharpe Ratio", Rf = Rf, FUN = "SharpeRatio.annualized", event.labels= NULL, ...))#print

	############################################################################
	
    par(op)
}

