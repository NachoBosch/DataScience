X = c(0.32,0.36,0.24,0.11,0.11,0.44,2.79,2.99,3.47,0.23,0.55,3.21,4.02,0.23)
X
mean(X)
length(X)
media = sum(X)/length(X)


x = c(X,28)
x

media_muestral = mean(X)
media_ouliers = mean(x)
media_ajustada = mean(x,trim=.1)
media_muestral
media_ouliers
media_ajustada
sort(x)
fn = ecdf(x)
fn(1)
