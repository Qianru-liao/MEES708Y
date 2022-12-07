EuStockMarkets

myfun <- function(x){
    means <<- apply(x, 1, mean, na.rm = TRUE)
    stop("error message")
    # browser()
    q1 = apply(x, 1, quantile, na.rm = TRUE, probs = 0.25)
    return(data.frame(Means = means, Q1 = q1))

}

myfun(swiss)

head(means)

tmp = as.numeric(time(EuStockMarkets))
plot(x = tmp, means, type = "l", ylim = c(0, 6000))
lines(x = tmp, y = q1)


head(iris)
library(ggplot2)
ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, col = Species) ) +
    geom_line()



dim(tmp)
if (is.null(dim(tmp))) {
    sdfs
}
if (is.vector(tmp)) {

}
is.vector(EuStockMarkets)

x1 = x[,1]
x10 = x[,1, drop = FALSE]
dim(x10)


