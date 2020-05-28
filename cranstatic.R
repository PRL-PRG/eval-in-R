# Prase Cranstatic and return the ordered list of popular packages
# Save it as an fst file

library(rvest)
library(selectr)
library(dplyr)
library(stringr)
library(fst)

url <- "https://crantastic.org/packages?page="

page <- read_html(str_c(url, "1"))

nb_pages <- page %>% 
    html_nodes("div .pagination > a") %>% 
    tail(n=2) %>%
    first() %>%
    html_text() %>%
    as.integer()

nb_packages <- page %>%   #Just get the text inside, not the children nodes
  html_nodes(xpath=paste(css_to_xpath("div #packages_list"), "/text()")) %>%
  html_text() %>%
  .[[1]] %>%
  str_extract("\\d+") %>%
  as.integer()

packages_per_page <- vector("list", nb_pages)

print(str_c("Pages: ", nb_pages, " and packages: ", nb_packages))

for(i in 1:nb_pages) 
{
  complete_url <- str_c(url, i)
  print(strc_c("Processing ", complete_url))
  page <- read_html(complete_url)
  packages_per_page[[i]] <- page %>% html_nodes("table") %>% .[[1]] %>% html_table()
}

packages <- bind_rows(packages_per_page)

# Ranekd by popularity (number of users)
ranked_packages <- packages %>% arrange(desc(Users))


write.fst(ranked_packages, "cranstatic_packages.fst")
