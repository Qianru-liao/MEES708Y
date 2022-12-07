myplot <- function(df, colmed = "black", plotmean = TRUE, ...){
    if (is.matrix(df)) {
        print("successful check")
    } else {
        df <- as.matrix(df)
    }
    if (dim(df)[2] == 1) {
        stop("Not correct data format. The input data must have >1 column.")
    }
    m <- apply(df, 1, mean, na.rm = TRUE)
    med <- apply(df, 1, median, na.rm = TRUE)
    q1 <- apply(df, 1, quantile, na.rm = TRUE, probs = 0.25)
    q3 <- apply(df, 1, quantile, na.rm = TRUE, probs = 0.75)
    # df2 <- data.frame(Mean = m, med = med, Q1 = q1, Q3 = q3)
    # df3 <- as.numeric(df2)
    tmp = as.numeric(time(df))
    plot(x = tmp, y = med, las = 1,
         pch = 18,
         col = "black",
         ylim = range(c(q1, q3)),
         cex = 2,
         type = "b",
         ...)
    polygon(x = c(tmp, rev(tmp)), y = c(q1, rev(q3)), col = "green")
    if (plotmean) {
        lines(x = tmp, y = m, col = "blue", lty = 2)
    }
    lines(x = tmp, y = med, col = colmed, lwd = 2)
    legend("topleft", legend = c("Mean", "Average", "Q1 & Q3"),
                col = c("blue", "green", "red"))
}
