## ----get-data, include=TRUE----------------------------------------------

### load data
fname = sprintf("%s%s/%s%s.nc", DataDirectory (), Project, Project, Flight)
print (sprintf ("processing %s", fname))
# VarList must include all variable names that are used in this routine
## VarList is a file that contains all the variables needed. It loads
## 'VarList' as a vector of those names. Add new names to that file.
source("./VarList")
Data <- getNetCDF (fname, VarList)

# data: select only points where TASX > 60, and optionally limit time range
DataV <- Data[setRange(Data$Time, StartTime, EndTime), ]
DataV <- DataV[(!is.na (DataV$TASX)) & (DataV$TASX > 60), ]


