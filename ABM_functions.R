################################################################################
##################### AUXILARY FUNCTIONS #######################################
################################################################################

## Data manipulation
# remove .csv from a file name
rm.csv = function(x, n){
    substr(x, 1, nchar(x) - n)
}
# Load a single csv from a file name
raw.data = function(scenario, n_rows){
    data_folder = paste0('./data/', scenario, '/')
    file_list = list.files(data_folder)
    df = read.csv(paste0(data_folder, file_list[2]), nrows = 1, skip = 21, sep = ',')
    df = df[-1,]
    for(i in 1: length(file_list)){
        csv = paste0(data_folder, file_list[i])
        temp = read.csv(csv, nrows = n_rows, skip = 21, sep = ',')
        temp$iter = rm.csv(file_list[i], 4) 
        df = rbind(df, temp)
    }
    return(df)
}
# Remove unused columns
clean.data = function(raw_df){
    raw_df = raw_df[,c(1,2,6,10,14,18, ncol(raw_df))]
    names(raw_df) = c('time', 'infected', 
                  'healthy', 'recoveryRate', 
                  'immuneRate', 'population', 'iter')
    return(raw_df)
}
# Calculate average and confidence interval, return a dataframe with avg and ConfInt  
calc.Avg = function(dtf){
    df_wide = as.matrix(dcast(dtf[,c(1,2,3,7)], time ~ iter, value.var = 'infected'))
    df_wide = df_wide[,-1]
    iter_avg = apply(df_wide, MARGIN = 1, mean)
    iter_sd = apply(df_wide, MARGIN = 1, sd)
    iter_lower_confInt = iter_avg - 1.96*(iter_sd/sqrt(ncol(df_wide)-1))
    iter_upper_confInt = iter_avg + 1.96*(iter_sd/sqrt(ncol(df_wide)-1))
    temp = cbind(time = seq(1:length(iter_avg)), iter_avg_infected = iter_avg,
                 iter_avg_healthy = dtf$population[1] - iter_avg,
                            iter_sd, iter_lower_confInt, 
                            iter_upper_confInt)
    temp = data.frame(temp, iter = dtf[,'iter'][1])
    return(temp)
}

################################################################################
##################### MAIN FUNCTIONS ###########################################
################################################################################

## Read and return a dataframe with raw data
# For each iteration in each scenario
all.raw.data = function(scenario_list, n_rows){
    df = data.frame()
    for(i in 1:length(scenario_list)){
        raw_df0 = raw.data(scenario_list[i], n_rows) 
        temp = clean.data(raw_df0)
        df = rbind(df, temp)
    }
    return(df)
}

## Return a dataframe of all the scenarios with the average and confidence interval
all.Avg.data = function(scenario_list, n_rows){
    df = data.frame()
    for(i in 1:length(scenario_list)){
        raw_df0 = raw.data(scenario_list[i], n_rows) 
        temp = clean.data(raw_df0)
        temp = calc.Avg(temp)
        df = rbind(df, temp)
    }
    return(df)
}

## Plot scenarios
# Input the df from all.raw.data() and from all.Avg.data() and a vector with the 
# letter codes of the different scenarios
# returns a list of plots
plot.scenarios = function(df_raw, df_avg, cases, n_iter = 1000){
    plot_list = list()
    col1 = add.alpha('grey8', alpha = 0.2)
    for(i in 1:length(cases)){
        selection = cases[i]
        by_scenario = df_raw[grepl(selection, df_raw$iter),]
        recR = by_scenario$recoveryRate[1]
        immR = by_scenario$immuneRate[1]
        pop = by_scenario$population[1]
        by_scenario_avg = df_avg[grepl(selection, df_avg$iter),]
        by_scenario_avg = by_scenario_avg[!is.na(by_scenario_avg$iter_avg_infected),]
        nn = round(length(by_scenario_avg$iter_avg_infected)*.5,0)
        end_value = tail(by_scenario_avg$iter_avg_infected, 1)
        scenario_mean = ifelse(end_value == 0 | end_value == 200, 
                    ifelse(end_value == 0, 0, 200), 
                    mean(tail(by_scenario_avg$iter_avg_infected, nn)))
        
        scenario_sd = sd(tail(by_scenario_avg$iter_avg_infected),nn)
        conf_Int = 1.96*(scenario_sd/sqrt(nn))
        lwr_b = scenario_mean - conf_Int
        upr_b = scenario_mean + conf_Int
        num_inf = ifelse(selection == 'i' | selection == 'l', ifelse(selection == 'i', 2, 100), 50)
        
        font_size = 10
        title_size = 30
        
        g = ggplot(by_scenario, aes(x = time, y = infected, group=iter))
        g = g + geom_line(colour = col1) + ylim(0, 200) + xlim(0,n_iter) +
            geom_hline(aes(yintercept = 100),colour = 'blue')
        g = g + geom_hline(aes_q(yintercept = lwr_b),colour = 'forestgreen') +
            geom_hline(aes_q(yintercept = upr_b),colour = 'forestgreen')
        g = g + geom_line(data = by_scenario_avg, aes(y = iter_avg_infected))
        g = g + geom_hline(aes_q(yintercept = scenario_mean),colour = 'red')
        g = g + theme(legend.position="none") 
        g = g + ggtitle(paste('Scenario', toupper(selection)))
        
        g = g + annotate('text', label = paste('Avg at equilibrium =', round(scenario_mean,1)), 
                         x = n_iter-n_iter*0.3, y = 200, size = font_size, colour = 'red', hjust = 0) +
            annotate('text', label = paste('conf Int = +-',round(conf_Int,4)), 
                     x = n_iter-n_iter*0.3, y = 192, size = font_size, colour = 'forestgreen', hjust = 0) +
            annotate('text', label = paste('sd =', round(scenario_sd,1)),
                     x = n_iter-n_iter*0.3, y = 184, size = font_size, colour = 'grey6', hjust = 0)
        g = g + annotate('text', label = paste('Recovery rate =', recR), 
                         x = 0, y = 200, size = font_size, colour = 'black', hjust = 0) + 
            annotate('text', label = paste('Immunity ratio =', immR), 
                     x = 0, y = 192, size = font_size, colour = 'black', hjust = 0) +
            annotate('text', label = paste('Num infected =', num_inf), 
                     x = 0, y = 184, size = font_size, colour = 'black', hjust = 0) +
            annotate('text', label = paste('Population =', pop), 
                     x = 0, y = 176, size = font_size, colour = 'black', hjust = 0)
        g = g + theme(text = element_text(size=title_size))
        plot_list[[toupper(selection)]] = g
    }
    return(plot_list)
}

######
# Find the average time t when the system hits the average of a sample of 
# the tail of the time series

find.steady = function(a_raw, a_avg, tail_len = 0.3){
    iters = unique(a_raw$iter)
    output = data.frame()
    a_avg = a_avg[!is.na(a_avg$iter_avg_infected),]
    n = round(length(a_avg$iter_avg_infected)*tail_len,0)# sample the last 30% of the series
    end_value = tail(a_avg$iter_avg_infected, 1)
    mu = ifelse(end_value == 0 | end_value == 200, 
                ifelse(end_value == 0, 0, 200), 
                mean(tail(a_avg$iter_avg_infected, n)))
    for(i in 1:length(iters)){
        df = a_raw[a_raw$iter == iters[i],]
        t = 1
        if(df$infected[t+1] < mu){
        while(df$infected[t] < mu){t = t + 1}
        output = rbind(output, df[t,])
        }
        else{
            t = 2
            while(df$infected[t] > mu){t = t + 1}
            output = rbind(output, df[t,])
        }
    }
    mu_t = round(mean(output$time),2)
    sd_t = sd(output$time)
    n_t = length(output$time)
    conf_int = round(1.96*sd_t/sqrt(n_t),1)
    lwr_b = round(mu_t - conf_int, 2)
    upr_b = round(mu_t + conf_int, 2)
    out_v = c(mu_t, conf_int, upr_b, lwr_b, mu)
    names(out_v) = c('Average time to the steady state', 
                     '95% confidence interval',
                     'Upper bound',
                     'Lower bound', 
                     'Average of the steady state')
    print(out_v)
    return(out_v)
}
