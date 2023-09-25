# 25 Sept 2023
# Install R packages needed to run the Shiny app. 
install.packages("reticulate")
install.packages("shiny")
install.packages("shinyjs")
install.packages("tidyverse")
install.packages("tuneR")

# Optionally, attempt to download and install Miniconda from RStudio using the 
# "reticulate" package. Not working when tested late Sept '23. 
reticulate::install_miniconda()

# If the above doesn't work, download and run the installer from 
# https://docs.conda.io/en/latest/miniconda.html 
# and set the install location to C:\Users\username\AppData\Local\r-miniconda

# If you are getting Python errors when trying to run the app you may need to
# install the development version of reticulate.
# First download and install the appropriate version of Rtools from
# https://cran.r-project.org/bin/windows/Rtools/
# (Use the Rtools version that matches the version of R you are running.)
# Then run the following:
install.packages("pak")
pak::pak("rstudio/reticulate")