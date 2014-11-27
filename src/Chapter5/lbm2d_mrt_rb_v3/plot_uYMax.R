library(ncdf)
rb.output = open.ncdf("./out_test.nc")

print(rb.output)

uYMax = get.var.ncdf( rb.output, "uYMax" )
time  = get.var.ncdf( rb.output, "time" )

nTimes = dim(uYMax)

#selTimeRange = (nTimes %/% 5):nTimes # discards initial adjustment-period
selTimeRange = 1:nTimes # complete history

plot(time[selTimeRange], uYMax[selTimeRange], log="y", type="l", lty=1)

