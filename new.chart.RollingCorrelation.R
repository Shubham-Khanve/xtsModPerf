new.chart.RollingCorrelation <-
function (Ra, Rb, width = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), ..., fill=NA)
{ 
    Ra = checkData(Ra)
    Rb = checkData(Rb)

    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    for(column.a in 1:columns.a) { 
        for(column.b in 1:columns.b) { 
            merged.assets = merge(Ra[,column.a,drop=FALSE], Rb[,column.b,drop=FALSE])
            column.calc = rollapply(na.omit(merged.assets[,,drop=FALSE]), width = width, FUN= function(x) cor(x[,1,drop=FALSE], x[,2,drop=FALSE]), by = 1, by.column = FALSE, fill = fill, align = "right")

            column.calc.tmp = xts(column.calc)
            colnames(column.calc.tmp) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
            column.calc = xts(column.calc.tmp, order.by = time(column.calc))

            if(column.a == 1 & column.b == 1)
                Result.calc = column.calc
            else
                Result.calc = merge(Result.calc, column.calc)
        }
    }

############################################################################
#chart.TimeSeries replaced by new.chart.TimeSeries 

 new.chart.TimeSeries(Result.calc, xaxis = xaxis, colorset = colorset, legend.loc = legend.loc, ylim = c(-1,1), ...)
 
############################################################################

}

