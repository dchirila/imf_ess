# Filename: quickPlotBoxModel.plt
# Purpose: Plot ASCII-file containing output of the box-model.

# Usage:
#       gnuplot -e "datafile='<path_to_file>'" quickPlotBoxModel.plt
#
#   For example, assuming there is an output-file named './box_model_rk4.out' in
#   the current directory, it can be visualized with the command:
#       gnuplot -e "datafile='./box_model_rk4.out'" quickPlotBoxModel.plt

reset

if(exists("datafile")) {
  # Configure axes-tics
  ticsFont = "Times-Roman, 4"
  set xtics font ticsFont
  set ytics font ticsFont
  set key font ticsFont

  # Global properties for LABELS
  labelX = 0.32
  labelY = 1.08
  labelFont = ',6'

  # Tweaks to make graphs have same width
  set lmargin 2.5
  set rmargin 1.2
  set bmargin 2.5

  # start multiplot (3x2)
  set multiplot layout 3,2 rowsfirst

  currLabel = 'Global Temperature'
  set label 1 currLabel at graph labelX, labelY font labelFont
  plot datafile using 1:2 with lines ls 1 title 'tempGlobal'

  currLabel = 'Overturning circulation'
  set label 1 currLabel at graph labelX, labelY font labelFont
  plot datafile using 1:14 with lines ls 1 title 'Phi'

  currLabel = 'Ocean Temperature'
  set label 1 currLabel at graph labelX, labelY font labelFont
  plot datafile using 1:3 with lines ls 1 title 'TocS', \
       datafile using 1:4 with lines ls 2 title 'TocM', \
       datafile using 1:5 with lines ls 3 title 'TocN', \
       datafile using 1:6 with lines ls 4 title 'TocD'

  currLabel = 'Ocean Salinity'
  set label 1 currLabel at graph labelX, labelY font labelFont
  plot datafile using 1:7 with lines ls 1 title 'SocS', \
       datafile using 1:8 with lines ls 2 title 'SocM', \
       datafile using 1:9 with lines ls 3 title 'SocN', \
       datafile using 1:10 with lines ls 4 title 'SocD'

  currLabel = 'Atmosphere Temperature'
  set label 1 currLabel at graph labelX, labelY font labelFont
  plot datafile using 1:7 with lines ls 1 title 'TatS', \
       datafile using 1:8 with lines ls 2 title 'TatM', \
       datafile using 1:9 with lines ls 3 title 'TatN', \
       datafile using 1:10 with lines ls 4 title 'TatD'

  unset multiplot

  pause -1
}
