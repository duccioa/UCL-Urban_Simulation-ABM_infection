library(ggplot2)
library(reshape2)
pop_growth1 <- read.csv("./data/population_growth_1000t_rr112_dr100.csv", stringsAsFactors = FALSE)
names(pop_growth1)<- c("time", "sick", "healthy", "x")

ggplot(pop_growth1, aes(x=healthy)) + geom_histogram(aes(y=..density..))

data_folder <- "./data/population/"
list_files <- list.files(data_folder)
col_index <- 3
df_complete <- data.frame(time = seq(1,1500,1))
for(i in 1:length(list_files)){
    csv <- paste(data_folder, list_files[i], sep = "")
    df <- read.csv(csv, stringsAsFactors = FALSE)
    df <- df[1:1500,col_index]
    
    df_complete <- cbind(df_complete, df)
    names(df_complete)[length(names(df_complete))] <- paste("iter_", i, sep ="")
}
head(df_complete)


df_melted <- melt(df_complete, id="time")
g <- ggplot(df_melted, aes(x=time, y=value, colour=variable, group=variable))
g <- g + geom_smooth()
g

ggplot(df_melted, aes(x = time, y = value)) + geom_smooth(method="loess")
fit_nl <- loess(value ~ time, data=df_melted, span = 100)

