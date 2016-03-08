library(ggplot2)
library(reshape2)
source('./functions.R')
source('/Users/duccioa/Documents/02_DataScience/02_Functions/add_alpha.R')
scenarios = dir('./data', full.names = F, no..=T)
n = 600 # Specify number of rows
all_iter = all.raw.data(scenarios, n)
all_Avg = all.Avg.data(scenarios, n)
cases = c('a', 'b', 'c', 'd', 'f', 'g', 'h', 'i', 'l', 'm')
plot_list = plot.scenarios(all_iter, all_Avg, cases)
for(i in 1:length(cases)){
    plot_name = paste('infected_', toupper(cases[i]), '.png')
        ggsave(plot = plot_list[[i]], filename = plot_name, 
               dpi = 240, scale = 2, path = './Figures')
}




