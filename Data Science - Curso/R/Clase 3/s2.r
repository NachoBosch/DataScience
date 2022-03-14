library(UsingR)
library(ggplot2)

x = read.table(file='D:/Data Science - Curso/R/Clase 3/bitcoin.csv',dec='.',sep = ',',header = TRUE)
df = data.frame(x)
p = ggplot(df,aes(x=x)) + geom_histogram()
ggsave('D:/Data Science - Curso/R/Clase 3/figures/fig1.png',p)


