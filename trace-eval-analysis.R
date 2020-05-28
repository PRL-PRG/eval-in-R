library(tidyverse)
library(forcats)
library(fst)
library(stringi)
library(stringr)
library(viridis)

# "blob-eval-calls.fst"      "bmp-eval-calls.fst"       "bootstrap-eval-calls.fst"
#   "dplyr-eval-calls.fst"     "egg-eval-calls.fst"      
# "msm-eval-calls.fst"      
package_name <- "egg-eval-calls.fst"
# package_name (always base here), function_name, expression, stack_frame, 
eval_calls <- as_tibble(read.fst(package_name))

exprs <- eval_calls %>% count(expression) %>% arrange(desc(n))

mostUsedExprsPlot <- exprs %>% filter(n > 100) %>% ggplot() + geom_col(aes(x=fct_reorder(expression, n), y=n))  +  coord_flip()

ggsave(str_c(package_name, "mostUsedExprs.pdf"), plot = mostUsedExprsPlot)

# cat(as.character(eval_calls[1, "stack_frame"][[1]]))

# Access the line before the last line of the stack frame to get the last function
# Won't work to get the lastcaller
callingFunctions <- eval_calls %>% mutate(calls = stri_split_lines(as.character(stack_frame)), last_caller = sapply(calls, function(call) str_trim(call[length(call) - 1]))) %>% select(-calls)
callers <- callingFunctions %>% count(last_caller) %>% arrange(desc(n))
mostFrequentCallers <- callers %>% filter(n > 100) %>% ggplot() + geom_col(aes(x=fct_reorder(last_caller, n), y=n))  +  coord_flip()
ggsave(str_c(package_name, "-mostFrequentCallers.pdf"), plot = mostFrequentCallers)

exprCaller <- callingFunctions %>% count(expression, last_caller) %>% filter(n >= 100) %>% 
    ggplot(aes(y = expression, x = last_caller)) + geom_tile(aes(fill=n)) + 
    scale_fill_viridis() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave(str_c(package_name, "-exprCaller.pdf"), plot = exprCaller)
