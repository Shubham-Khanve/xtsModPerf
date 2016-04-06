new.charts.RollingRegression = function (Ra, Rb, width = 12, Rf = 0, main = NULL, legend.loc = NULL, event.labels=NULL, ...)
{ 
	columns.a = ncol(Ra)
    columns.b = ncol(Rb)
	
	op <- par(no.readonly=TRUE)

    layout(matrix(c(1,2,3)),heights=c(1.3,1,1.3),widths=1)

    par(mar=c(1,4,4,2))
    if(is.null(main)){
      freq = periodicity(Ra)

      switch(freq$scale,
          minute = {freq.lab = "minute"},
          hourly = {freq.lab = "hour"},
          daily = {freq.lab = "day"},
          weekly = {freq.lab = "week"},
          monthly = {freq.lab = "month"},
          quarterly = {freq.lab = "quarter"},
          yearly = {freq.lab = "year"}
      )

      main = paste("Rolling ",width,"-",freq.lab," Regressions", sep="")
    }

	############################################################################
	#print has been added to every new.charts.RollingRegression
	#chart.RollingRegression replaced by new.chart.RollingRegression
	
    print(new.chart.RollingRegression(Ra, Rb, width = width, Rf = Rf, attribute = "Alpha", xaxis = FALSE, main = main, ylab = "Alpha", legend.loc=legend.loc, event.labels = event.labels, ...))

    par(mar=c(1,4,0,2))

    print(new.chart.RollingRegression(Ra, Rb, width = width, Rf = Rf, attribute = "Beta", main = "", ylab = "Beta", xaxis = FALSE, event.labels = NULL, ...))

    par(mar=c(5,4,0,2))

    print(new.chart.RollingRegression(Ra, Rb, width = width, Rf = Rf, attribute = "R-Squared", main = "", ylab = "R-Squared", event.labels = NULL, ...))

    par(op)
	
	############################################################################
}