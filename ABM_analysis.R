library(ggplot2)
library(reshape2)
source('./ABM_functions.R')
source('/Users/duccioa/Documents/02_DataScience/02_Functions/add_alpha.R')
scenarios = dir('./data', full.names = F, no..=T)
n = 5000 # Specify number of rows
all_iter = all.raw.data(scenarios, n)
all_Avg = all.Avg.data(scenarios, n)

num_iter_plotted = 5000
cases = c('a', 'b', 'c', 'd', 'f', 'g', 'h', 'i', 'l', 'p', 'q', 'r')
plot_list = plot.scenarios(all_iter, all_Avg, cases, n_iter = num_iter_plotted)
for(i in 1:length(cases)){
    plot_name = paste0('infected_t',num_iter_plotted,'_', toupper(cases[i]), '.png')
        ggsave(plot = plot_list[[i]], filename = plot_name, 
               dpi = 240, scale = 2, path = './Figures')
}


n = 200
case_studies = all_iter[grepl('p', all_iter$iter) | 
                            grepl('q', all_iter$iter) |
                            grepl('r', all_iter$iter), ]
case_avg = all_Avg[grepl('p', all_Avg$iter) | 
                        grepl('q', all_Avg$iter) |
                        grepl('r', all_Avg$iter), ]
case_studies = case_studies[case_studies$time <= 200,]
case_avg = case_avg[case_avg$time <= 200,]

plot_list = plot.scenarios(case_studies, case_avg, c('p', 'q', 'r'), n)
for(i in 1:length(c('p', 'q', 'r'))){
    plot_name = paste0('infected_t',n,'_', toupper(cases[i]), '.png')
    ggsave(plot = plot_list[[i]], filename = plot_name, 
           dpi = 240, scale = 2, path = './Figures')
}


### Analysis
n = 5000 # Specify number of rows
all_iter2 = all.raw.data(scenarios, n)
all_Avg2 = all.Avg.data(scenarios, n)

# steady state of scenario A
ar = all_iter2[grepl('a', all_iter2$iter), ]
aa = all_Avg2[grepl('a', all_Avg2$iter), ]
st_a = find.steady(ar, aa, .5)
# steady state of scenario B
br = all_iter2[grepl('b', all_iter2$iter), ]
bb = all_Avg2[grepl('b', all_Avg2$iter), ]
st_b = find.steady(br, bb, 0.5)
# steady state of scenario C
cr = all_iter2[grepl('c', all_iter2$iter), ]
cc = all_Avg2[grepl('c', all_Avg2$iter), ]
st_c = find.steady(cr, cc, .5)
# steady state of scenario D
dr = all_iter2[grepl('d', all_iter2$iter), ]
dd = all_Avg2[grepl('d', all_Avg2$iter), ]
st_d = find.steady(dr, dd, .5)



# Plot distribution
plot_dist_list = list()

for(i in seq_along(cases)){
    sel = cases[i]
    x = all_Avg[grepl(sel, all_Avg$iter),]
    p = ggplot(x, aes(x=iter_avg_infected)) + 
        geom_histogram(binwidth=.5, colour="black", fill="white")+
        ggtitle(paste0('PDF scenario ', toupper(sel)))
    if(sel != 'd' & sel != 'h' & sel != 'r'){
        n = length(x$iter_avg_infected)
        mu = mean(x$iter_avg_infected)
        sigma = sd(x$iter_avg_infected)
        conf_int = 1.96*sigma/sqrt(n)
        lwr = mu - conf_int
        upr = mu + conf_int
        p = p + geom_vline(aes_q(xintercept = lwr), colour = 'blue') +
            geom_vline(aes_q(xintercept = upr), colour = 'blue')+
            geom_vline(aes_q(xintercept = mu), colour = 'red')
            
    }
    plot_dist_list[[sel]] = p
}
plot_dist_list
