# Initialize the virtual environment
library(renv)
renv::init()

# All package installation and loading goes here
install.packages("dcmodify", repos='https://cran.uib.no')
library(dcmodify)
install.packages("magrittr", repos='https://cran.uib.no') 
library(magrittr)
install.packages("simputation", repos='https://cran.uib.no') 
library(simputation)
install.packages("lumberjack", repos='https://cran.uib.no') 
library(lumberjack)
install.packages("dplyr", repos='https://cran.uib.no') 
library(dplyr)

# Save the dependencies above into renv.lock
renv::snapshot()