library(ggplot2)
library(reshape2)
source('./functions.R')
scenarios = dir('./data', full.names = F, no..=T)
all_iter = all.raw.data(scenarios)
all_Avg = all.Avg.data(scenarios)

s = scenarios[2]
by_scenario = all.Avg.data(s)
g1 = ggplot(by_scenario, aes(x = time))
g1 + geom_line(aes(y = iter_avg_infected, colour = 'red')) + 
    geom_line(aes(y = iter_avg_healthy, colour = 'green'))
