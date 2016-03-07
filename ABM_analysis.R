library(ggplot2)
library(reshape2)


data_folder = "./data/"
list_files = list.files(data_folder)

df = read.csv(paste0('./data/', list_files[2]), nrows = 1, skip = 21, sep = ',')
df = df[-1,]
read.data = function(List){
    # define function to remove n characters from a string
    rm.csv = function(x, n){
        substr(x, 1, nchar(x) - n)
    }
    df = read.csv(paste0('./data/', List[2]), nrows = 1, skip = 21, sep = ',')
    df = df[-1,]
    for(i in 1: length(List)){
        csv = paste0('./data/', List[i])
        temp = read.csv(csv, nrows = 5000, skip = 21, sep = ',')
        temp$iter = rm.csv(List[i], 4) 
        df = rbind(df, temp)
    }
    df = df[,c(1,2,6,10,14,18, ncol(df))]
    names(df) = c('time', 'infected', 'healthy', 'recoveryRate', 'immuneRate', 'population', 'iter')
    return(df)
}
df = read.data(list_files)

df_wide = as.matrix(dcast(df[,c(1,2,3,7)], time ~ iter, value.var = 'infected'))
df_wide = df_wide[,-1]
iter_avg = apply(df_wide, MARGIN = 1, mean)
iter_sd = apply(df_wide, MARGIN = 1, sd)
iter_lower_confInt = iter_avg - 1.96*(iter_sd/sqrt(ncol(df_wide)-1))
iter_upper_confInt = iter_avg + 1.96*(iter_sd/sqrt(ncol(df_wide)-1))


