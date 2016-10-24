# pacNamesAddin
RStudio Addin for insertion of package names.
### Installing and loading the `microarrayTools` package:
Installing through GitHub requires the `devtools` package, so instructions for installing and loading that package are provided below.
```r
## Install and load devtools for loading packages from GitHub
install.packages("devtools") # to allow us to install packages from GitHub
library(devtools)
```
Since GitHub packaged are compiled on your machine to run, you may be prompted to "install build tools" or something similar.  Follow the instructions, which will install the tools needed to compile this package.
  
Now you should be ready to install and then load the `pacNamesAddin` package
```r
## Install pacNamesAddin package to get the RStudio addins for automatic insertion of package names
install_github("bakuhatsu/pacNamesAddin") # syntax for installing from GitHub: username/library
library(pacNamesAddin) # To load the package
```
