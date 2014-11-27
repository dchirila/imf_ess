# File: plotTempFieldFromAscii.R
#
# Purpose: Visualize ASCII-output for the LBM-MRT code which simulates the
# Rayleigh-Benard problem in 2D.
#
# Required R-packages: "fields", "animation"; if not present on your system,
# install them with:
# > install.packages(package_name_with_quotes)

# clear workspace
rm( list=ls() );

# TODO:
# - accept "prefix" as script-parameter

library(animation)
library(fields)

# utility-function
getFloatVector <- function( fName, atLine ) {
  result <- scan(file=fName, what=numeric(0.0), comment.char="#"
                 , skip=atLine-1, nlines=1, quiet=TRUE)
  return(result)
}

outPrefix = "results"

summaryFileName = paste( outPrefix, "summary.dat", sep="_" )

filePattern = paste("(", paste(outPrefix, "temp", "diff", sep="_")
                    , ").*\\.dat$", sep=""
                    )

# Parse summary-file
simParams = scan(file=summaryFileName
                 , what=list(Nx=integer(0)
                             , Ny=integer(0)
                             , Nt=integer(0)
                             , Ra=numeric(0.0)
                             , Pr=numeric(0.0)
                             , maxMach=numeric(0.0)
                             , XUnits=character(0)
                             , YUnits=character(0)
                             , TUnits=character(0)
                             , UyMaxUnits=character(0)
                             )
                 , comment.char="#"
                 , nmax=1, quiet=TRUE
                 )

XVals = getFloatVector(summaryFileName, 11)
YVals = getFloatVector(summaryFileName, 12)
TVals = getFloatVector(summaryFileName, 13)
UyMaxVals = getFloatVector(summaryFileName, 14)

# Parse & plot field-files

#saveGIF(
        #{
          #ani.options(interval=0.2, nmax=simParams$Nt)
          #layout(matrix(1:2, 2, 1))
          #for( currFile in list.files(".", pattern=filePattern) ) {
            #fieldInfo = scan(file=currFile
                                 #, what=list(thisTime=numeric(0.0), tempUnits=character(0))
                                 #, comment.char="#"
                                 #, nmax=1, quiet=TRUE
                                 #)
            #currTempField = matrix( scan(file=currFile
                                         #, n=simParams$Nx*simParams$Ny
                                         #, skip=3, quiet=TRUE 
                                         #)
            #, simParams$Nx, simParams$Ny, byrow=FALSE
            #)
            #image.plot(XVals, YVals, currTempField, asp=1
                       #, xlab=paste("x [", simParams$XUnits, "]", sep="")
                       #, ylab=paste("y [", simParams$YUnits, "]", sep="")
                       #, main=paste("T [", fieldInfo$tempUnits, "]", sep="") 
                       #)
            #contour(XVals, YVals, currTempField, add=TRUE)
            #plot(TVals, UyMaxVals, type="o"
                 #, col=ifelse(TVals==fieldInfo$thisTime, "red", "black")
                 #, pch=ifelse(TVals==fieldInfo$thisTime, "O", ".")
                 #, xlab=paste("time [", simParams$TUnits, "]", sep="")
                 #, ylab=paste("max Uy [", simParams$UyMaxUnits, "]", sep="")
                 #)
          #}
        #}
        ##, movie.name = outPrefix
        #, ani.width = 800 
        #, ani.height = 600 
        #)

saveHTML(
        {
          ani.options(interval=0.2, nmax=simParams$Nt)
          layout(matrix(1:2, 2, 1))
          for( currFile in list.files(".", pattern=filePattern) ) {
            fieldInfo = scan(file=currFile
                                 , what=list(thisTime=numeric(0.0), tempUnits=character(0))
                                 , comment.char="#"
                                 , nmax=1, quiet=TRUE
                                 )
            currTempField = matrix( scan(file=currFile
                                         , n=simParams$Nx*simParams$Ny
                                         , skip=3, quiet=TRUE 
                                         )
            , simParams$Nx, simParams$Ny, byrow=FALSE
            )
            image.plot(XVals, YVals, currTempField, asp=1
                       , xlab=paste("x [", simParams$XUnits, "]", sep="")
                       , ylab=paste("y [", simParams$YUnits, "]", sep="")
                       , main=paste("T [", fieldInfo$tempUnits, "]", sep="") 
                       )
            contour(XVals, YVals, currTempField, add=TRUE)
            plot(TVals, UyMaxVals, type="o"
                 , col=ifelse(TVals==fieldInfo$thisTime, "red", "black")
                 , pch=ifelse(TVals==fieldInfo$thisTime, "O", ".")
                 , xlab=paste("time [", simParams$TUnits, "]", sep="")
                 , ylab=paste("max Uy [", simParams$UyMaxUnits, "]", sep="")
                 )
          }
        }
        #, img.name = outPrefix
        , ani.width = 800 
        , ani.height = 600 
        )

