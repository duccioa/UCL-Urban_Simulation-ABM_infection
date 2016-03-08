library(ggplot2)
library(reshape2)
source('./functions.R')
source('/Users/duccioa/Documents/02_DataScience/02_Functions/add_alpha.R')
scenarios = dir('./data', full.names = F, no..=T)
n = 150 # Specify number of rows
all_iter = all.raw.data(scenarios, n)
all_Avg = all.Avg.data(scenarios, n)
i = 3
cases = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'l', 'm')
col1 = add.alpha('grey8', alpha = 0.2)
selection = cases[i]
by_scenario = all_iter[grepl(selection, all_iter$iter),]
recR = by_scenario$recoveryRate[1]
immR = by_scenario$immuneRate[1]
pop = by_scenario$population
by_scenario_avg = all_Avg[grepl(selection, all_Avg$iter),]
scenario_mean = mean(by_scenario_avg$iter_avg_infected)
scenario_sd = sd(by_scenario_avg$iter_avg_infected)
conf_Int = 1.96*(scenario_sd/sqrt(length(by_scenario_avg$iter_avg_infected)))
lb = scenario_mean - conf_Int
ub = scenario_mean + conf_Int



g = ggplot(by_scenario, aes(x = time, y = infected, group=iter))
g = g + geom_line(colour = col1) + ylim(0, 200) +
    geom_hline(aes(yintercept = 100),colour = 'blue')
g = g + geom_hline(aes(yintercept = lb),colour = 'forestgreen') +
    geom_hline(aes(yintercept = ub),colour = 'forestgreen')
g = g + geom_line(data = by_scenario_avg, aes(y = iter_avg_infected))
g = g + geom_hline(aes(yintercept = scenario_mean),colour = 'red')
g = g + theme(legend.position="none") 
g = g + ggtitle(paste('Scenario', toupper(selection)))
g = g + annotate('text', label = paste('mean =', round(scenario_mean,1)), 
                 x = n - n*0.2, y = 200, size = 3.5, colour = 'red', hjust = 0) +
    annotate('text', label = paste('conf Int = +-',round(conf_Int,1)), 
             x = n - n*0.2, y = 192, size = 3.5, colour = 'forestgreen', hjust = 0) +
    annotate('text', label = paste('sd =', round(scenario_sd,1)),
             x = n - n*0.2, y = 184, size = 3.5, colour = 'grey6', hjust = 0)
g = g + annotate('text', label = paste('Recovery rate =', recR), 
                 x = 0, y = 200, size = 3.5, colour = 'black', hjust = 0) 
  #  annotate('text', label = paste('Immunity ratio =', round(immR)), 
   #          x = 0, y = 192, size = 3.5, colour = 'black', hjust = 0) +
    #annotate('text', label = paste('Population =', round(pop)), 
     #        x = 0, y = 184, size = 3.5, colour = 'black', hjust = 0)



