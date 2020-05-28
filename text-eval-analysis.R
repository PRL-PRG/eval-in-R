# Analyze usage of eval textually (not on traces of execution)
# Generate data frame:
# package_name, file, line_number, line 
# Add surrounding function?

library(tidyverse)
library(stringr)
library(xfun) # To be able to find arguments of parse_only 
library(rlang)

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

packagedir = "packages-2020-05-13"

# R source is in the R folder of the packages

packages <- list.dirs("packages-2020-05-13",  recursive=FALSE)
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
      filter(str_detect(lines, "eval\\s*\\("))  # filter  the remaining ones
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


# Packages using eval
packagesPlot <- ev_results %>% count(package) %>% arrange(desc(n)) %>% ggplot() + geom_col(aes(x=fct_reorder(package, n), y=n))  + 
  labs(y="number", x="package", title = "packahes using eval")
  coord_flip()
ggsave("packagesEval.pdf", plot = packagesPlot)

call_evals <- ev_results %>% count(eval_call) %>% arrange(desc(n))

mostUsedEvalCallPlot <- call_evals %>% filter(n > 1) %>% mutate(prop = 100 * n / nrow(ev_results)) %>% ggplot() + 
    geom_col(aes(x=fct_reorder(eval_call, prop), y=prop))  + 
    labs(y="%", x = "eval call", title = "Most used eval calls") + 
    coord_flip() 
ggsave("propEvals.pdf", plot = mostUsedEvalCallPlot)
  


extract_args <- function(ev_call) 
{
  #print(str_c("CALL:", ev_call, "\n"))
  # Get an expression from the string
  exp <- parse(text = ev_call)[[1]]
  # Check if it is actually a call and an eval call
  #if(is.call(exp) && exp[[1]] == "eval") #eval_call(exp, name="eval") with rlang
  if(is.call(exp))
  {
    nb_args <- length(exp) - 1
    # Have all the named arguments
    # It requires to have the function in exp in the global environment
    # that's why I added library(xfun) to be able to parse parse_only
    exp <- call_standardise(exp)
    # List of arguments (remove the call name)
    args <- call_args(exp)#as.list(exp[-1])
    return(map(args, function(x) {str_c(deparse(x), collapse = "\n")}))
  }
  else
  {
    return(list())
  }
}

extract_call_name <- function(ev_call)
{
  # Get an expression from the string
  exp <- parse(text = ev_call)[[1]]
  # Check if it is actually a call
  if(is.call(exp))
  {
    return(call_name(exp))
  }
  else
  {
    return(NA)
  }
}

arg_eval <- ev_results %>% 
  mutate(arguments = sapply(eval_call, extract_args), nb_args = sapply(arguments, length)) %>%
  unnest_wider(arguments) # Unnest list of arguments into columns

# How many use envir? and enclos?
arg_eval %>% filter(!is.na(envir)) %>% count()
# parent.frame() is the most used then env
arg_eval %>% count(envir) %>% arrange(desc(n)) 

arg_eval %>% filter(!is.na(enclos)) %>% count()
# parent.frame() then parent.frame(2)
arg_eval %>% count(enclos) %>% arrange(desc(n)) 

# Most used exprs
expr_evals <- arg_eval %>% count(expr) %>% arrange(desc(n))

mostUsedEvalExprPlot <- expr_evals %>% filter(n > 1) %>% mutate(prop = 100 * n / nrow(arg_eval)) %>% ggplot() + 
  geom_col(aes(x=fct_reorder(expr, prop), y=prop))  + 
  labs(y="%", x = "expression", title = "Most used expressions in eval calls") + 
  coord_flip() 
ggsave("propExprEvals.pdf", plot = mostUsedEvalExprPlot)

# most used functions in an expr
call_expr_evals <- arg_eval %>% mutate(call_in_expr = map_chr(expr, extract_call_name)) %>%
    count(call_in_expr) %>% arrange(desc(n))

mostUsedEvalCallExprPlot <- call_expr_evals  %>% mutate(prop = 100 * n / nrow(arg_eval)) %>% ggplot() + 
  geom_col(aes(x=fct_reorder(call_in_expr, prop), y=prop))  + 
  labs(y="%", x = "function", title = "Most used functions in exps eval calls",
       subtitle = "NA corresponds to a variable") + 
  coord_flip() 
ggsave("propCallExprEvals.pdf", plot = mostUsedEvalCallExprPlot)

# Eval that use parse (and parse a string or a file)
text_eval <- arg_eval %>% filter(str_detect(expr, fixed("parse")))

# Rename code arg of parse_only into text arg of parse
sanitize_parse_args <- function(args) 
{
  if("code" %in% names(args))
  {
    args$text <- args$code
    args$code <- NULL
  }
  return(args)
}

arg_text_eval <- arg_eval %>% filter(str_detect(expr, fixed("parse"))) %>% 
  mutate(parse_arg = map(expr, ~ sanitize_parse_args(extract_args(.)))) %>% 
           unnest_wider(parse_arg)

# How many use parameter keep.source?
arg_text_eval %>% count(keep.source) # Only 4(and FALSE) and 52 do no use this arg


# Find out if some textual args are shared by several packages, are patterns
arg_text_eval %>% group_by(text) %>% summarize(nb_packages = n_distinct(package)) %>% arrange(desc(nb_packages))
                            

        