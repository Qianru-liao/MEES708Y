myplot <- function(x, plotmean = FALSE, color = "red"){
    if (is.null(dim(x))) {
        stop("Quantiles per time point cannot be calculated with only a numeric vector.")
    }
    x = as.data.frame(x) #need to convert to matrix/dataframe
    if (ncol(x) == 1){
        stop("Quantiles per time point cannot be calculated with only a numeric vector.")
    }
    x <- tibble::rownames_to_column(x, "Year")
    x[,1] <- as.numeric(x[,1])
    # x <- subset(x,!is.na(x[,2]))
    m <- apply(select(x, -Year), 1, mean, na.rm = TRUE)
    med <- apply(select(x, -Year), 1, median, na.rm = TRUE)
    q1 <- apply(select(x, -Year), 1, quantile, na.rm = TRUE, probs = 0.25)
    q3 <- apply(select(x, -Year), 1, quantile, na.rm = TRUE, probs = 0.75)

    p = ggplot() + aes(x[,1], med) +
        geom_line()
    # +
    #         geom_line(aes(x[,1],x[,2]),color = "black") +
    #         ggtitle("Cluster 3") +
    #         xlab("Year") +
    #         ylab("Attainment Deficit (%)") +
    #         theme(plot.title.position = "plot") +
            # geom_ribbon(aes(quantiles = q1, color = "red"), method = "rqss") +
    #         geom_hline(aes(yintercept = mean(x[,2], na.rm=TRUE), color = "blue")) +
    #         geom_hline(aes(yintercept = median(x[,2], na.rm=TRUE), color = "green")) +
    #         scale_color_hue(labels = c("mean","median","quantiles")) + theme_classic()

    if (plotmean){
        p = p +
            geom_line(aes(x[,1], m), color = "black")
    }
    print(p)
    return(p)
}
