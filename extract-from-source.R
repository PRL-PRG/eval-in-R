# Extract usages of eval in the source of packages
# Generate the data

library(tidyverse)
library(stringr)
library(rlang)
library(fst)

# A "beautiful" function to parse out the lines with eval
# We assume that eval is on the first line
# TODO: handle \\\" 
parse_eval <- function(strs) 
{
  #1st paren after eval
  pos <- str_locate(strs[[1]], "eval\\s*\\(")
  start_eval <- pos[,1]
  end_eval <- start_eval# Will be updated later
  start_paren <- pos[,2]
  paren_counter <- 0# the first ( will be counted later
  line_counter <- 0
  in_string1 <- FALSE # Detect if we're in a string \\" 
  in_string2 <- FALSE # Detect if we're in a string '
  for(line in strs)
  {
    line_counter <- line_counter + 1
    characters <- str_split(line, "")
    if(length(characters[[1]]) == 0 )
    {
      next # So elegant
    }
    for(i in start_paren:length(characters[[1]]))
    {
      c <- characters[[1]][[i]]
      if (c == "\"") 
      {
        in_string1 <- !in_string1
      }
      else if (c == "'" && !in_string1) # '
      {
        in_string2 <- !in_string2
      }
      else if (!in_string1 && !in_string2)
      {
        if (c == "#") # Skip the remaining of the line
        {
          break
        }
        else if(c == "(")
        {
          paren_counter = paren_counter + 1
        }
        else if( c == ")")
        {
          paren_counter = paren_counter - 1
        }
      }
      # We found the closing parenthesis of the eval
      # We need to leave both loops
      if(paren_counter <= 0 ) 
      {
        end_eval <- i
        break
      }
    }
    if(paren_counter <= 0 ) 
    {
      break
    }
    start_paren <- 1
  }
  # Remove before "eval()
  start_str <- str_sub(strs[[1]], start_eval, str_length(strs[[1]]))
  eval_str <- if(line_counter == 1) 
  {
    str_sub(start_str, 1, end_eval - start_eval + 1)
  }
  else
  {
    end_str <- str_sub(strs[[line_counter]], 1, end_eval)
    middle_str <- ""
    if(line_counter > 2)
    {
      middle_str <- str_c(strs[c(2:(line_counter-1))], collapse="\n")
    }
    str_c(start_str, middle_str, end_str, sep="\n")
  }
  return(eval_str)
}

# May return empty string
remove_comments <- function(line) 
{
  sharp_pos <- str_locate(line, "#")[,1]
  if (is.na(sharp_pos)) # If there is actually no comment
  {
    return(line)
  }
  else
  {
    return(str_sub(line, 1, sharp_pos - 1))
  }
}

analyse_packages <- function(packagedir) 
{
  packages <- list.dirs(packagedir,  recursive=FALSE)
  #packages <- sapply(packages, function(package) {paste0(package, "/R")})
  excluded_packages <- c("assertthat")
  
  packages <- packages[!str_detect(packages, str_c(excluded_packages, collapse="|"))]
  
  
  eval_results = list()
  
  for (package in packages)
  {
    # Get all the R files
    rFiles <- list.files(paste0(package, "/R"), full.names = TRUE, pattern = ".*\\.R")
    
    eval_package = list()
    
    for (file in rFiles)
    {
      # Read the file
      filelines = readLines(file)
      nbLines = length(filelines)
      # Search for eval, line by line
      # This is regex so we just detect the line
      # To capture all the expression, we will need to do some balanced
      # parenthesis matching
      eval_lines <- tibble(lines = filelines) %>%
        mutate(line_number = row_number()) %>%
        mutate(lines_with_context = str_c(lag(lines), lines, lead(lines), sep="\n")) %>%
        filter(str_detect(lines, fixed("eval"))) %>% # Coarse but quick
        # Here, for performance reasons
        mutate(lines = sapply(lines, remove_comments)) %>% # there could be evals only in the comments
        filter(str_detect(lines, "\\beval\\s*\\("))  # filter  the remaining ones
      #filter(str_detect(lines, fixed("eval("))) # 
      
      if(count(eval_lines) > 0)
      {
        eval_lines <- eval_lines %>% 
          mutate(eval_call = sapply(line_number, function(ln) { parse_eval(filelines[ln:nbLines])}))
        eval_package[[basename(file)]] <-  eval_lines
      }
    }
    if(length(eval_package) > 0) 
    {
      eval_results[[basename(package)]] <- bind_rows(eval_package, .id = "file")
    }
  }
  
  ev_results <- if(length(eval_results) > 0)
  {
    bind_rows(eval_results, .id = "package")
  } else 
  {
    print("Np packages using eval")
    tibble()
  }
  return(ev_results)
}

packagedir = "packages-2020-06-03"

ev_results <- analyse_packages(packagedir)

eval_calls <- "source_eval_calls.fst"
write.fst(ev_results, eval_calls)
