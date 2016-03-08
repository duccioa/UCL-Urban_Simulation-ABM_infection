library(ggplot2)
library(reshape2)
source('./functions.R')
source('/Users/duccioa/Documents/02_DataScience/02_Functions/add_alpha.R')
scenarios = dir('./data', full.names = F, no..=T)
all_iter = all.raw.data(scenarios, 150)
all_Avg = all.Avg.data(scenarios, 150)

cases = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'l', 'm')
col1 = add.alpha('grey8', alpha = 0.2)
selection = cases[3]
by_scenario = all_iter[grepl(selection, all_iter$iter),]
by_scenario_avg = all_Avg[grepl(selection, all_Avg$iter),]
scenario_mean = mean(by_scenario_avg$iter_avg_infected)
scenario_sd = sd(by_scenario_avg$iter_avg_infected)
conf_Int = 1.96*(scenario_sd/sqrt(length(by_scenario_avg$iter_avg_infected)))
lb = scenario_mean - conf_Int
ub = scenario_mean + conf_Int

g = ggplot(by_scenario, aes(x = time, y = infected, group=iter))
g = g + geom_line(colour = col1) + ylim(0, 200) +
    geom_hline(aes(yintercept = 100),colour = 'black')
#g = g + xlim(100,150)
g = g + geom_line(data = by_scenario_avg, aes(y = iter_avg_infected))
g = g + geom_hline(aes(yintercept = scenario_mean),colour = 'red')
    geom_hline(aes(yintercept = lb),colour = 'red', linetype = 'dashed') +
    geom_hline(aes(yintercept = ub),colour = 'red', linetype = 'dashed')
g = g + theme(legend.position="none") + ggtitle('Scenario A - RecoveryRate = 3')
g
