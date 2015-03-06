## ----plot-loop-----------------------------------------------------------

### This section loops through plot functions, first loading them from 'PlotFunctions'
### and then running them with the appropriate data.
for (np in 1:2) {
  if (testPlot(np)) {
    print(paste('Plot',np))
    eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
    eval(parse(text=sprintf("RPlot%d(Data, Flight)", np)))
  }
}
for (np in 3:nps) {
  if (file.exists (sprintf ("./PlotFunctions/RPlot%d.R", np))) {
    if (testPlot(np)) {
      print(paste('Plot',np))
      eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
      eval(parse(text=sprintf("RPlot%d(DataV)", np)))
    }
  }
}
print('done plotting')
