# Analyze usage of eval textually (not on traces of execution)
# Generate data frame:
# package_name, file, line_number, line 
# Add surrounding function?

library(tidyverse)
library(stringr)
library(xfun) # To be able to find arguments of parse_only 
library(rlang)
library(fst)

eval_calls <- "source_eval_calls.fst"
ev_results <- read.fst(eval_calls)

# Packages using eval
packagesPlot <- ev_results %>% count(package) %>% filter(n > 1) %>% 
    arrange(desc(n)) %>% head(50) %>%
    ggplot() + geom_col(aes(x=fct_reorder(package, n), y=n))  + 
    labs(y="number", x="package", title = "packages using eval") +
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
ggsave("packagesEval.pdf", plot = packagesPlot)

call_evals <- ev_results %>% count(eval_call) %>% arrange(desc(n))

mostUsedEvalCallPlot <- call_evals %>% filter(n > 1) %>% 
    mutate(prop = 100 * n / nrow(ev_results)) %>%  head(15) %>%
    ggplot() + 
    geom_col(aes(x=fct_reorder(eval_call, prop), y=prop))  + 
    labs(y="%", x = "eval call", title = "Most used eval calls") + 
    coord_flip() 
ggsave("propEvals.pdf", plot = mostUsedEvalCallPlot)
  


extract_args_eval <- function(ev_call) 
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
    # sot it would require to add all the libraries we are
    # parsing in the environmen 
    # for instance to be able to parse parse_only
    # Is eval called with other arguments than the normal ones?
    # Then filter it out.
    # Also remove evals with ... because we cannot standardize them
    if(nb_args > 0 && (!all(call_args_names(exp) %in% c("", "expr",  "envir", "enclos")) || exp[[2]] == "..." || nb_args > 3))
    {
      print(str_c("Unknown args for ", ev_call))
      return(list())
    }
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
    #return(call_name(exp)) # Would return NULL if the function is anonymous
    return(deparse(exp[[1]]))
  }
  else
  {
    return(NA)
  }
}

# Filter out some packages that have problems because of \"
# earth does a printf("eval(%s, %s))". We should also exclude in a string out of an eval
ev_results <- ev_results %>% filter(!package %in% c("AICcmodavg", "FactoClass", "ajv", "earth", "shinymaterial", "caret", "copula"))

arg_eval <- ev_results %>% 
  mutate(arguments = sapply(eval_call, extract_args_eval), nb_args = sapply(arguments, length)) %>%
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

mostUsedEvalExprPlot <- expr_evals %>% filter(n > 1) %>%
  mutate(prop = 100 * n / nrow(arg_eval)) %>% 
  head(25) %>%
  ggplot() + 
  geom_col(aes(x=fct_reorder(expr, prop), y=prop))  + 
  labs(y="%", x = "expression", title = "Most used expressions in eval calls") + 
  coord_flip() 
ggsave("propExprEvals.pdf", plot = mostUsedEvalExprPlot)

# most used functions in an expr
call_expr_evals <- arg_eval %>% mutate(call_in_expr = map_chr(expr, extract_call_name)) %>%
    count(call_in_expr) %>% arrange(desc(n))

mostUsedEvalCallExprPlot <- call_expr_evals  %>% 
  mutate(prop = 100 * n / nrow(arg_eval)) %>% head(25) %>%
  ggplot() + 
  geom_col(aes(x=fct_reorder(call_in_expr, prop), y=prop))  + 
  labs(y="%", x = "function", title = "Most used functions in exps eval calls",
       subtitle = "NA corresponds to a variable") + 
  coord_flip() 
ggsave("propCallExprEvals.pdf", plot = mostUsedEvalCallExprPlot)

# Eval that use parse (and parse a string or a file)
text_eval <- arg_eval %>% filter(str_detect(expr, fixed("parse")))

extract_args_parse <- function(ev_call) 
{
  #print(str_c("CALL:", ev_call, "\n"))
  exp <- parse(text = ev_call)[[1]]
  
  # Check if it is actually a call
  if(is.call(exp) && exp[[1]] == "parse")
  {
    nb_args <- length(exp) - 1
    # Have all the named arguments
    # It requires to have the function in exp in the global environment
    # sot it would require to add all the libraries we are
    # parsing in the environment 
    # for instance to be able to parse parse_only
    if(nb_args > 0 && (!all(call_args_names(exp) %in% c("", "code", "text", "file", "n", "keep.source")) || exp[[2]] == "..." ))
    {
      print(str_c("Unknown args for ", ev_call))
      return(list())
    }
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

# Rename code arg of parse_only into text arg of parse
sanitize_parse_args <- function(args) 
{
  if("code" %in% names(args))
  {
    args$text <- args$code
    args$code <- NULL
  }
  
  if("file" %in% names(args))
  {
    args$fileParse <- args$file
    args$file <- NULL
  }
  return(args)
}

arg_text_eval <- arg_eval %>% filter(str_detect(expr, fixed("parse"))) %>% 
  filter(str_detect(expr, fixed("deparse"), negate=TRUE)) %>%
  mutate(parse_arg = map(expr, ~ sanitize_parse_args(extract_args_parse(.)))) %>% 
           unnest_wider(parse_arg)

# How many use parameter keep.source?
arg_text_eval %>% filter(!is.na(keep.source)) %>% nrow() 


# Find out if some textual args are shared by several packages, are patterns
arg_text_eval %>% group_by(text) %>% summarize(nb_packages = n_distinct(package)) %>% arrange(desc(nb_packages))
                            

        