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


