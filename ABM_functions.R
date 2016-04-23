################################################################################
##################### AUXILARY FUNCTIONS #######################################
################################################################################
# Calculate the mode of a distribution
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}


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

## Calclulate mode and ConfInt
calc.Mode.df = function(df_raw, df_avg, cases){
    source('./ABM_functions.R')
    M = matrix(NA, nrow = 15, ncol = length(cases))
    colnames(M) = cases
    rownames(M) = 1:15
    for(j in 1:length(cases)){
        sel_scen = cases[j]
        by_scenario = df_raw[grepl(sel_scen, df_raw$iter),]
        sel_iter = paste0(cases[j], 
                          formatC(as.numeric(rownames(M)), width = 2, flag = '0')
        )
        for(i in 1:length(sel_iter)){
            sub_data = by_scenario$infected[grepl(sel_iter[i], by_scenario$iter)]
            mm = Mode(sub_data)
            M[i,j] = ifelse(!is.na(mm), mm, mean(M[1:(i-1),j]))
        }
    }
    TT = data.frame(mConfInt = NULL, Mean = NULL, pConfInt = NULL, pVal = NULL)
    for(i in 1:length(cases)){
        if(var(M[,i])!=0){
            tt = t.test(M[,i], alternative = 'two.sided')
            TT[i,1] = tt$conf.int[1]
            TT[i,2] = tt$estimate
            TT[i,3] = tt$conf.int[2]
            TT[i,4] = tt$p.value
        }
        else{
            TT[i,1] = NA
            TT[i,2] = mean(M[,i])
            TT[i,3] = NA
            TT[i,4] = NA
        }
    }
    names(TT) = c('mConfInt', 'Mode', 'pConfInt', 'pVal')
    rownames(TT) = cases
    return(TT)
}


## Plot scenarios
# Input the df from all.raw.data() and from all.Avg.data() and a vector with the 
# letter codes of the different scenarios
# returns a list of plots
plot.scenarios = function(df_raw, df_avg, cases, n_iter = 1000, confIntDf){
    require(ggplot2)
    plot_list = list()
    col1 = add.alpha('grey8', alpha = 0.2)
    for(i in 1:length(cases)){
        selection = cases[i]
        # Subset data
        by_scenario = df_raw[grepl(selection, df_raw$iter),]
        # Extract parameters
        recR = by_scenario$recoveryRate[1]
        immR = by_scenario$immuneRate[1]
        pop = by_scenario$population[1]
        by_scenario_avg = df_avg[grepl(selection, df_avg$iter),]
        by_scenario_avg = by_scenario_avg[!is.na(by_scenario_avg$iter_avg_infected),]
        #nn = round(length(by_scenario_avg$iter_avg_infected)*.5,0)
        #end_value = tail(by_scenario_avg$iter_avg_infected, 1)
        #scenario_mean = ifelse(end_value == 0 | end_value == 200, 
         #                      ifelse(end_value == 0, 0, 200), 
        #                       mean(tail(by_scenario_avg$iter_avg_infected, nn)))
        
        #scenario_sd = sd(tail(by_scenario_avg$iter_avg_infected),nn)
        #conf_Int = 1.96*(scenario_sd/sqrt(nn))
        #lwr_b = scenario_mean - conf_Int
        #upr_b = scenario_mean + conf_Int
        num_inf = ifelse(selection == 'i' | selection == 'l', ifelse(selection == 'i', 2, 100), 50)
        
        
        
        
        font_size = 10
        title_size = 30
        
        g = ggplot(by_scenario, aes(x = time, y = infected, group=iter)) + theme_bw()
        g = g + geom_line(colour = col1) + ylim(0, 200) + xlim(0,n_iter) +
            geom_hline(aes(yintercept = 100),colour = 'grey40')# blue line at half of the population
        g = g + geom_hline(aes_q(yintercept = confIntDf$mConfInt[i]),colour = 'forestgreen') +
            geom_hline(aes_q(yintercept = confIntDf$pConfInt[i]),colour = 'forestgreen')
        g = g + geom_line(data = by_scenario_avg, aes(y = iter_avg_infected))
        g = g + geom_hline(aes_q(yintercept = confIntDf$Mean[i]),colour = 'red')
        g = g + theme(legend.position="none") 
        g = g + ggtitle(paste('Scenario', toupper(selection)))
        if(!is.na(confIntDf$pVal[i])){
            g = g + annotate('text', label = paste('Mode =', round(confIntDf$Mode[i],1)), 
                             x = n_iter-n_iter*0.3, y = 200, size = font_size, colour = 'red', hjust = 0) +
                annotate('text', label = '95% Conf Int', 
                         x = n_iter-n_iter*0.3, y = 190, size = font_size, colour = 'forestgreen', hjust = 0) +
                annotate('text', label = paste('[',round(confIntDf$mConfInt[i],1), ',',round(confIntDf$pConfInt[i],1),']'),
                         x = n_iter-n_iter*0.3, y = 184, size = font_size, colour = 'forestgreen', hjust = 0) +
                annotate('text', label = paste('p-value =', 
                                               format(confIntDf$pVal[i], digits=7-(nchar(n)-1), width=1)),
                         x = n_iter-n_iter*0.3, y = 174, size = font_size, colour = 'grey60', hjust = 0)
        } else{
            g = g + annotate('text', label = paste('Mode =', round(confIntDf$Mode[i],1)), 
                             x = n_iter-n_iter*0.3, y = 200, size = font_size, colour = 'red', hjust = 0) +
                annotate('text', label = '95% Conf Int', 
                         x = n_iter-n_iter*0.3, y = 190, size = font_size, colour = 'forestgreen', hjust = 0) +
                annotate('text', label = '[constant value]',
                         x = n_iter-n_iter*0.3, y = 184, size = font_size, colour = 'forestgreen', hjust = 0)
        }
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

find.steady = function(df_raw, cases, Mode_val, window_size = 20){
    eval_window = function(mw, pw, mc, pc, dat, iter){
        if(pw >= 5000){print('Iteration limit reached');output = TRUE}
        else{mo = Mode(dat[mw:pw])
        #MO = Mode(dat[mw:length(dat)])
        print(iter)
        print(paste('mo =', mo))
        output = mo<=pc&mo>=mc #& MO<=pc&MO>=mc
        print(output)}
        return(!output)
    }
    S = matrix(NA, nrow = 15, ncol = length(cases))
    
    for(j in 1:length(cases)){ # j = scenario
        sel_scen = cases[j]
        index_sel = which(grepl(sel_scen,rownames(Mode_val)))
        by_scenario = df_raw[grepl(sel_scen, df_raw$iter),]
        sel_iter = paste0(cases[j], 
                          formatC(1:nrow(Mode_val), width = 2, flag = '0')
        )
        
        pConf = round(Mode_val$pConfInt[index_sel])
        mConf = round(Mode_val$mConfInt[index_sel])
        for(i in 1:length(sel_iter)){ # i = run
            sub_data = by_scenario$infected[grepl(sel_iter[i], by_scenario$iter)]
            if(!sel_scen %in% c('d', 'h', 'r')){
                w = window_size # window size
                t=1 + w # starting time
                while(eval_window(mw=t-w, pw=t, 
                                  mc=mConf, pc = pConf, 
                                  dat = sub_data,
                                  iter = sel_iter[i])){t=t+w}
                output = t
            }
            if(sel_scen %in% c('d', 'h')){
                t=2
                while(sub_data[t]!=0){t=t+w}
                output = t
            }
            if(sel_scen %in% 'r'){
                t=2
                while(sub_data[t]!=200){t=t+w}
                output = t
            }
            if(output <= 5000){S[i,j] = output}
            else{S[i,j] = NA}
        }
    }
    SS = data.frame(mConfInt = NULL, Time2Steady = NULL, pConfInt = NULL, pVal = NULL, SD = NULL)
    for(i in 1:length(cases)){
        if(var(S[,i], na.rm = T)!=0){
            Ss = t.test(S[,i], alternative = 'two.sided')
            SS[i,1] = Ss$conf.int[1]
            SS[i,2] = Ss$estimate
            SS[i,3] = Ss$conf.int[2]
            SS[i,4] = Ss$p.value
            SS[i,5] = sd(S[,i], na.rm = T)
        }
        else{
            SS[i,1] = NA
            SS[i,2] = mean(S[,i], na.rm = T)
            SS[i,3] = NA
            SS[i,4] = NA
            SS[i,5] = NA
        }
    }
    names(SS) = c('mConfInt', 'Time2Steady', 'pConfInt', 'pVal', 'SD')
    rownames(SS) = cases
    return(SS)
    
}
