## ----configuration, include=TRUE-----------------------------------------

SavePlotsToFiles <- 1 # if != 0 plots are saved to a file
                      # 2 => display via evince when finished.
if (length (run.args) > 2) {
  SavePlotsToFiles <- as.numeric(run.args[3])
  if (SavePlotsToFiles == 0) {
    x11 ()
  }
}
print (sprintf (" length of run.args is %d, SavePlotsToFiles is %d",
                length (run.args), SavePlotsToFiles))
nps <- 30
Flight <- "rf01"
# x <- readline(sprintf("Project is %s; CR to accept or enter new project name: ", Project))
# if (nchar(x) > 1) {Project <- x}
print(sprintf("Project is %s", Project))
## find max rf in data directory, use as default if none supplied via command line:
Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), Project),
                        sprintf ("%srf...nc", Project)), decreasing = TRUE)[1]
Flight <- sub (Project, '',  sub (".nc", '', Fl))

if (length (run.args) > 0) {
  Flight <- run.args[1]
} else {
  x <- readline(sprintf("Flight is %s; CR to accept or enter new flight name (rfxx format): ",
                        Flight))
  if (nchar(x) > 1) {Flight <- x}
}
print(sprintf("Flight is %s", Flight))
nplots=c(1, 3:17, 19:nps)    # project default
if (length (run.args) > 1) {
  if (run.args[2] != "-1") {
    nplots <- eval (parse (text=paste('c(', run.args[2],')', sep='')))
  }  # else leave unchanged, using project default
  if (run.args[2] == "0") {
    nplots <- 1:nps
  }
} else {
  print ("Plots desired: CR to accept, or enter new spec")
  x <- readline ("new spec: (0 => all, -1 => project set, n => plot n, can be a sequence): ")
  if (nchar(x) > 0) {nplots <- eval(parse(text=paste('c(',x,')', sep='')))}
}
print (nplots)
StartTime <- 0; EndTime <- 0 ## can set start and end times for plot
if (length(run.args) > 3) {
  StartTime <- as.numeric (run.args[4])
  if (length (run.args) > 4) {
    EndTime <- as.numeric (run.args[5])
  }
}


