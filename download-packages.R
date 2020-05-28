# Download source code of most dowloaded R packages
# (excluding tidyverse optionally)

library(tidyverse)
library(cranlogs)


excluded_packages = c(tidyverse_packages(include_self = TRUE), "tidyselect")

# Will change depending on the day...
mostDownloadedPackages <- cran_top_downloads(count = 100) %>% 
    filter(!package %in% excluded_packages ) 

destdir = paste0("packages-", Sys.Date())
dir.create(destdir)

downloadedPackages <- download.packages(pkgs = pull(mostDownloadedPackages, package),  destdir = destdir, type = "source")

for (package in downloadedPackages[,2])
{
  untar(package, exdir=destdir)
}
