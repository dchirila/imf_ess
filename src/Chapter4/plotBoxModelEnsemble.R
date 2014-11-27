# File: plotBoxModelEnsemble.R
#
# Purpose: Visualize output of the climate box-model.

# clear workspace
rm( list=ls() );

library(ggplot2)
library(reshape2)

ensembleData <- list()

filePattern = "(box_model_ensemble_member_).*\\.dat"

memberIdx = 1

ensembleData <- NULL

for( currFile in list.files(".", pattern=filePattern) ) {
  print( paste("reading file: ", currFile ) )
  ensembleData[[memberIdx]] <- read.csv(currFile)
  memberIdx = memberIdx + 1
}

#str(ensembleData)

# Determine data-bounds for setting plot axes
minX = Inf; maxX = -Inf
minY = Inf; maxY = -Inf
for( member in ensembleData ) {
  minX = min(minX, min(member$year))
  maxX = max(maxX, max(member$year))

  minY = min(minY, min(member$TocS))
  maxY = max(maxY, max(member$TocS))
}

colSpace = palette(rainbow(length(ensembleData)))

plot(
     seq(minX,maxX,length.out=100),
     seq(minY,maxY,length.out=100),
     type="n",
     xlab="time [yr]",
     ylab="TocS [Celsius]",
     main="Ensemble plot"
    ) # initialize plot

legend("center", "groups", c(1:length(ensembleData)), pch=20, cex=0.5, col=colSpace,
       ncol=2)
memberIdx = 1
for( member in ensembleData ) {
  points( member$year, member$TocS, col=colSpace[[memberIdx]], pch=20, cex=.1 )

  memberIdx = memberIdx + 1
}

