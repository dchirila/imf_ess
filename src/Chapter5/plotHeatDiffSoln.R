# File: plotHeatDiffSoln.R
#
# Purpose: Visualize ASCII-output for the FD code which simulates the unsteady
# heat-diffusion problem in 2D.
#
# Required R-packages: "fields", "animation"; if not present on your system,
# install them with:
# > install.packages(package_name_with_quotes)

# clear workspace
rm( list=ls() );

# TODO:
# - accept "prefix" as script-parameter

library(fields)

# utility-function, for parsing ASCII-output summary file
getFloatVector <- function( fName, atLine ) {
  result <- scan(file=fName, what=numeric(0.0), comment.char="#"
                 , skip=atLine-1, nlines=1, quiet=TRUE)
  return(result)
}

outFileName = "simulation_final_temp_field.dat"

# Parse metadata in output-file
simMetadata = scan(file=outFileName
                   , what=list(timeUnit=character(0)
                               , currTime=numeric(0.0)
                               , xUnit=character(0)
                               , nX=numeric(0)
                               , yUnit=character(0)
                               , nY=numeric(0)
                               , tempUnit=character(0)
                               )
                   , comment.char="#"
                   , nmax=1, quiet=TRUE
                   )
xVals = getFloatVector(outFileName, 9)
yVals = getFloatVector(outFileName, 10)

tempFieldVals = matrix(
                       scan(file=outFileName
                            , n=(simMetadata$nX+1)*(simMetadata$nY+1)
                            , skip=11, quiet=TRUE
                            )
                       , simMetadata$nX+1, simMetadata$nY+1, byrow=FALSE
                       )

plotTitle <- bquote(theta*.("[")*degree~C*.("]"))

pdf('plotHeatDiffSoln.pdf')
image.plot(xVals, yVals, tempFieldVals
           , asp=1
           , xlim=c(min(xVals), max(xVals))
           , ylim=c(min(yVals), max(yVals))
           , xlab=paste("x [", simMetadata$xUnit, "]", sep="")
           , ylab=paste("y [", simMetadata$yUnit, "]", sep="")
           , main=plotTitle
           )
contour(xVals, yVals, tempFieldVals
        , asp=1
        , xlim=c(min(xVals), max(xVals))
        , ylim=c(min(yVals), max(yVals))
        , nlevels=15
        , labcex=1.0 # adjusts size of isocontour-labels
        , add=TRUE
        )
dev.off()
