simple run command from a terminal window: will ask for flight and plots wanted
  cd ~/RStudio/WINTER
  R --no-save <Review.R

alternate:
  R
   at prompt >:
      source ("~/RStudio/WINTER/Review.R"))
      quit ()

better: (suitable for batch file)  
   Rscript ~/RStudio/WINTER/Review.R rf01 
   (sets flight to be rf01, plots project-default set, displays it)
for specific plots (here 1,3, and 7 through 17):
   Rscript ~/RStudio/WINTER/Review.R rf01 1,3,7:17
for example, to just see the SkewT plot:
   Rscript ~/RStudio/WINTER/Review.R rf01 17

To stay up-to-date:
  cd ~/RStudio/WINTER
  git pull

For 'Ranadu' changes, can do the same in the ~/RStudio/Ranadu directory,
but then you have to rebuild the package. Better is to retrieve the
highest-numbered package (currently Ranadu_2.0-15-1-28.tar.gz) from tikal.
I don't put this in github because it's so big (~25 MB) a result of including
all the information needed to produce the skew-T diagram. Download, then 
install using (RStudio packages install) or via a terminal window using:
   R
   at prompt >:
     install.packages ("Ranadu_2.0-15-1-28.tar.gz")
     quit()
