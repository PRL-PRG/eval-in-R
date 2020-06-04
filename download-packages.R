# Download source code of most dowloaded R packages
# or from a fst file
# (excluding some packages optionally)

library(tidyverse)
library(cranlogs)
library(fst)


download_packages <- function(packages, excluded_packages=NULL) 
{
  destdir = paste0("packages-", Sys.Date())
  dir.create(destdir)
  
  selectedPackages <- packages %>% filter(!package %in% excluded_packages)
  
  downloadedPackages <- download.packages(pkgs = pull(selectedPackages, package),  destdir = destdir, type = "source")
  
  for (package in downloadedPackages[,2])
  {
    untar(package, exdir=destdir)
  }
}

full_tidyverse_packages <- function() 
{
  return(c(tidyverse_packages(include_self = TRUE), "tidyselect"))
}

# Without tidyverse packages
crantidy_packages <- function()
{
  # Will change depending on the day...
  mostDownloadedPackages <- cran_top_downloads(count = 100)
  download_packages(mostDownloadedPackages, full_tidyverse_packages())
}


packages_from_fst <- function(fstfile, n = 100)
{
  packages <-   as_tibble(read.fst(fstfile)) %>% 
    rename( package = `Package Name`) %>%
    head(n)
    
  download_packages(packages)
}

