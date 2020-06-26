## Functions to get various informations about R expressions


get_expr <- function(eval_call)
{
  # Special case for expressions starting with _ (such as _inherit in ggproto)
  escaped_eval_call <- if(startsWith(eval_call, "_"))
  {
    paste0("`", eval_call, "`")
  }
  else
  {
    eval_call
  }
  
  exp <- NA
  # Would fail for instance for 
  # "`$<-`(new(\"C++Field\", .xData = <environment>), \"read_only\", TRUE)" (classInt package)
  try(exp <- parse(text = escaped_eval_call)[[1]], silent=TRUE)
  
  return(exp)
}

function_name <- function(eval_call)
{
  exp <- get_expr(eval_call)
  if(is.call(exp))
  {
    return(paste(deparse(exp[[1]]), collapse = "\n"))
  }
  return(NA)
}


function_arguments <- function(eval_call)
{
  exp <- get_expr(eval_call)
  if(is.call(exp))
  {
    return(map_chr(as.list(exp[-1]), function(chr) { paste(deparse(chr), collapse = "\n")}))
  }
  return(NA)
}

# See extract_inner_exp which takes a str
# This one takes an expression
# We assume that eval_expr_call is a call
extract_inner_exp_aux <- function(eval_expr_call)
{
  args <- as.list(eval_expr_call[-1])
  leaves <- list()
  no_calls <- TRUE
  for(arg in args)
  {
    if(!missing(arg)) # we cannot have the two conditions in the same if
    {
      if(is.call(arg))
      {
        leaves <-  c(leaves, extract_inner_exp_aux(arg))
        no_calls <- FALSE
      }
    }
  }
  if(no_calls)
  {
    return(c(leaves, deparse(eval_expr_call)))
  }
  return(leaves)
}

# Extract the inner calls and arguments
# For instance, in f(g(h(e))), we would get h(e)
# For f(g(e), t), we will get g(e)
extract_inner_exp <- function(eval_call)
{
  #cat(eval_call, "\n")
  exp <- get_expr(eval_call)
  if(is.call(exp))
  {
    return(extract_inner_exp_aux(exp))
  }
  return(NA)
}

constant_leaves_expr <- function(eval_exp_call)
{
  if(is.call(eval_exp_call))
  {
    args <- as.list(eval_exp_call[-1])
    return(every(args, constant_leaves_expr))
  }
  else
  {
    return(!is.language(eval_exp_call))
  }
}

# Test if all the expression in the call graphs are not symbols but constant
# It would imply that there's no need for the eval!
constant_leaves <- function(eval_call)
{
  exp <- get_expr(eval_call)
  return(constant_leaves_expr(exp))
}

