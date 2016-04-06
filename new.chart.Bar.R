new.chart.Bar <- function (R, legend.loc = NULL, colorset = (1:12), ...)
{ 
    x = checkData(R)

	#chart.TimeSeries replaced by new.chart.TimeSeries (modified only for Univariate Series)
	
    new.chart.TimeSeries(x, type = "h", colorset = colorset, legend.loc = legend.loc, lend="butt",...)
	
	##########################################################################

}