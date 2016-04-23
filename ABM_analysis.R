library(ggplot2)
library(reshape2)
source('./ABM_functions.R')
source('/Users/duccioa/Documents/02_DataScience/02_Functions/add_alpha.R')
scenarios = dir('./data', full.names = F, no..=T)
n = 5000 # Specify number of rows
all_iter = all.raw.data(scenarios, n)
all_Avg = all.Avg.data(scenarios, n)
#### STEADY STATE ####
TT = calc.Mode.df(all_iter, all_Avg, cases)

SS = find.steady(all_iter, cases, TT, window_size = 10)

#### PLOT LINE GRAPHS ####
#### 5000 iterations ####
num_iter_plotted = 1000
cases = c('a', 'b', 'c', 'd', 'f', 'g', 'h', 'i', 'l', 'p', 'q', 'r')
debug(plot.scenarios)
plot_list = plot.scenarios(all_iter, all_Avg, 
                           cases, n_iter = num_iter_plotted, 
                           confIntDf = TT, timeDf = SS)
for(i in 1:length(cases)){
    plot_name = paste0('infected_t',num_iter_plotted,'_', toupper(cases[i]), '.png')
        ggsave(plot = plot_list[[i]], filename = plot_name, 
               dpi = 240, scale = 2, path = './Figures')
}

#### 200 iterations ####
n = 200
case_studies = all_iter[grepl('p', all_iter$iter) | 
                            grepl('q', all_iter$iter) |
                            grepl('r', all_iter$iter), ]
case_avg = all_Avg[grepl('p', all_Avg$iter) | 
                        grepl('q', all_Avg$iter) |
                        grepl('r', all_Avg$iter), ]
case_studies = case_studies[case_studies$time <= n,]
case_avg = case_avg[case_avg$time <= n,]

plot_list = plot.scenarios(case_studies, case_avg, c('p', 'q', 'r'), n, confIntDf = TT[10:12,])
for(i in 1:length(c('p', 'q', 'r'))){
    plot_name = paste0('infected_t',n,'_', toupper(c('p', 'q', 'r')[i]), '.png')
    ggsave(plot = plot_list[[i]], filename = plot_name, 
           dpi = 240, scale = 2, path = './Figures')
}






#### PLOT DISTRIBUTIONS ####
cases = c('a', 'b', 'c', 'd', 'f', 'g', 'h', 'i', 'l', 'p', 'q', 'r')
for(i in 1:length(cases)){
    scenario = cases[i]
    subs = all_iter$infected[grepl(scenario, all_iter$iter)]
    m = round(TT$Mean[i],1)
    Col = 'red'
    png(paste0('./Figures/dist_', toupper(scenario), '.png'), width = 800, height = 600)
    par(cex = 1.2, lwd = 0.5)
    hist(subs, breaks = 100, xlab = paste('Infected Population'),  
         main = '', 
         ylim = c(0,3000),
         xlim = c(0,210), 
         col = 'grey90')
    abline(v = m, col = Col, lwd=2)
    title(main = paste('Scenario',toupper(scenario), '\nInfected Population Frequency'))
    text(x = 20, y = 2500, paste('Mode:\n', m), col = Col)
    dev.off()
}




