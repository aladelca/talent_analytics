library('ggplot2')
library(tidyr)


data = read.csv('performance_data.csv')
df_long = gather(data, key = "worker", value = "performance", worker1, worker2, worker3)

ggplot(data = df_long, aes(x = performance, fill = worker)) + geom_histogram() + theme_minimal()

