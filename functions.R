library(reticulate)
library(tidyverse)
use_python('/Users/hui.wan/miniconda/bin/python')
library(SigProfilerAssignmentR)
library(SigProfilerPlottingR)

## ---------------------- functions: plot_sbs -------------------------  ##


col.cosmic = structure(c('#5ABCEBFF', '#050708FF', '#D33C32FF', '#CBCACBFF', '#ABCD72FF', '#E7C9C6FF'),
                       names = c("C>A", "C>G", "C>T", "T>A", "T>C" ,"T>G"))

order.cosmic = df_sig_feat  %>% mutate(MutType6 = str_sub(MutationsType, 3,5 )) %>% arrange(MutType6, MutationsType) %>% pull(MutationsType)

plot_sbs <- function(df_sig_feat, ncol = 1) {
  
  ggbarplot(df_sig_feat %>% gather(SBS, value, -MutationsType) %>% mutate(MutType6 = str_sub(MutationsType, 3,5 )),
            x= 'MutationsType', y = 'value', order = order.cosmic, 
            color = NA, fill = 'MutType6', palette = col.cosmic, 
            facet.by = 'SBS', ncol = ncol,  scale = 'free',
            x.text.angle = 90) + 
    #rremove('xlab') +  rremove('x.text') + rremove('x.ticks') + 
    #rremove('ylab') + rremove('y.text') + rremove('y.ticks') + 
    rremove('legend') 
  
}

plot_sbs(df_sig_feat, ncol = 4)

## ------------------- functions: plot dbs ------------------- ##

