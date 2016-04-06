new.chart.Events <-
function (R, dates, prior=12, post=12, main = NULL, xlab=NULL, ...)
{ 
  R = checkData(R[,1,drop=FALSE]) 
  if(is.null(main)) 
    main = paste(colnames(R), "Event Study")
  for(i in 1:length(dates)){
    date = dates[i]
    origin = grep(date,index(R))
    if(length(origin)==0) 
        stop("Date not found or dates don't match the index of the data.")
    x1 = matrix(R[(max(0,origin-prior)):origin, ],ncol=1) 
    if(origin-prior<0)
        x1 = rbind(matrix(rep(NA,abs(origin-prior)+1),ncol=1),x1) 
    x2 = matrix(R[(origin+1):min(length(R),origin+post) ],ncol=1) 
    if(origin+post>length(R))
        x2 = rbind(x2,matrix(rep(NA,((origin+post)-length(R))),ncol=1)) 
    x = rbind(x1,x2) 

    if(date == dates[1])
      y = x
    else
      y = cbind(y,x) 
  }

  colnames(y) = as.character(dates)
  x.xts=xts(y, order.by=time(R)[1:nrow(y)]) 
  event.line = format(time(x.xts)[prior+1], "%m/%y")
  if(is.null(xlab))
    xlab = "Periods to Event" 

############################################################################
#chart.TimeSeries replaced by new.chart.TimeSeries
	
  new.chart.TimeSeries(x.xts, xlab = xlab, xaxis.labels = seq(-prior, post, by=1), event.lines = event.line, main = main, ...)
  
############################################################################

}

