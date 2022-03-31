#Install RGEE package

#30-03-2022


if (!("reticulate" %in% installed.packages()[,1])) {
     print("Installing package `reticulate`...")
     install.packages("reticulate")
 } else { 
     print("Package `reticulate` already installed") }
"Package `reticulate` already installed"
library(reticulate)
Sys.which("python")   # system default
 
Sys.which("python3") 

use_python(Sys.which("python3")) 
# use the standard Python numeric library
np <- reticulate::import("numpy", convert = FALSE)
# do some array manipulations with NumPy
a <- np$array(c(1:4))
print(a)  # this should be a Python array
(sum <- a$cumsum())

print(py_to_r(sum))

 
# install -- one time
## development version -- use with caution
# remotes::install_github("r-spatial/rgee")
## stable version on CRAN
if (!("rgee" %in% installed.packages()[,1])) {
     print("Installing package `rgee`...")
     install.packages("rgee")
 } else
 { print("Package `rgee` already installed") }

library(rgee)
rgee::ee_install() # Install required Python packages for GEE

library(rgee) 
ee_check() # Check non-R dependencies
