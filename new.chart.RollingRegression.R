new.chart.RollingRegression <-
function (Ra, Rb, width = 12, Rf = 0, attribute = c("Beta", "Alpha", "R-Squared"), main=NULL, na.pad = TRUE, ...)
{ 
	Ra = checkData(Ra)
    Rb = checkData(Rb)
    
    attribute=attribute[1]

    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)
	
############################################################################
	# Return.excess was taken from PerformanceAnalytics package and introduced here	
	
	Return.excess <-
	function (R, Rf = 0)
	{ # Source: https://github.com/cran/PerformanceAnalytics/blob/master/R/Return.excess.R
    R = checkData(R)

    if(!is.null(dim(Rf))){
        Rf = checkData(Rf)
        coln.Rf=colnames(Rf)
        if(is.null(coln.Rf)){
          colnames(Rf) = "Rf"
          coln.Rf = colnames(Rf)
        }
        Rft=cbind(R,Rf)
        Rft=na.locf(Rft[,make.names(coln.Rf)])
        Rf=Rft[which(index(R) %in% index(Rft))]
    }
    else {
        coln.Rf='Rf'
        Rf = reclass(rep(Rf,length(index(R))),R) 
    }

    result = do.call(merge, lapply(1:NCOL(R), function(nc) R[,nc] - coredata(Rf))) 
    
    if(!is.null(dim(result))) colnames(result) = paste(colnames(R), ">", coln.Rf)
   
    return(result)
	}
############################################################################

    Ra.excess = Return.excess(Ra, Rf)
    Rb.excess = Return.excess(Rb, Rf)

    for(column.a in 1:columns.a) { 
        for(column.b in 1:columns.b) { 
            merged.assets = merge(Ra.excess[,column.a,drop=FALSE], Rb.excess[,column.b,drop=FALSE])
            if(attribute == "Alpha")
                column.result = rollapply(na.omit(merged.assets), width = width, FUN= function(x) lm(x[,1,drop=FALSE]~x[,2,drop=FALSE])$coefficients[1], by = 1, by.column = FALSE, fill = na.pad, align = "right")
            if(attribute == "Beta")
                column.result = rollapply(na.omit(merged.assets), width = width, FUN= function(x) lm(x[,1,drop=FALSE]~x[,2,drop=FALSE])$coefficients[2], by = 1, by.column = FALSE, fill = na.pad, align = "right")
            if(attribute == "R-Squared")
                column.result = rollapply(na.omit(merged.assets), width = width, FUN= function(x) summary(lm(x[,1,drop=FALSE]~x[,2,drop=FALSE]))$r.squared, by = 1, by.column = FALSE, align = "right")

            column.result.tmp = xts(column.result)
            colnames(column.result.tmp) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
            column.result = xts(column.result.tmp, order.by = time(column.result))

            if(column.a == 1 & column.b == 1)
                Result.calc = column.result
            else
                Result.calc = merge(Result.calc, column.result)
        }
    }

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

      main = paste("Rolling ",width,"-",freq.lab," ", attribute, sep="")
    }
    
	############################################################################
	#chart.TimeSeries replaced by new.chart.TimeSeries
	
	new.chart.TimeSeries(Result.calc, main = main, ...)
	
	############################################################################

}