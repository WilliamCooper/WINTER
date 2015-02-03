simple run command from a terminal window: will ask for flight and plots wanted
  cd ~/RStudio/WINTER
  R --no-save <Review.R

alternate:
  R
   at prompt >:
      source ("~/RStudio/WINTER/Review.R"))

better: (suitable for batch file)  
   Rscript ~/RStudio/WINTER/Review.R rf01 
   (sets flight to be rf01, plots project-default set, displays it)
for specific plots (here 1,3, and 7 through 17):
   Rscript ~/RStudio/WINTER/Review.R rf01 1,3,7:17
for example, to just see the SkewT plot:
   Rscript ~/RStudio/WINTER/Review.R rf01 17

