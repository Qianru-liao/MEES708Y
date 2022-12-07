checkweather <- function(x, verbose = FALSE,
                         alternative = c("two.sided", "less", "greater"),
                         y) {
    # x is temperature, degrees C
    # verbose is logical value, indicating to print results
    if (FALSE) {
        x = 5
        verbose = FALSE
        alternative = "tw"
        y = x
        checkweather(-80)
        checkweather(-80, verbose = TRUE)
    }
    alt = match.arg(alternative, c("two.sided", "less", "greater"))
    if (x < -273) {
        stop("The temperature cannot be that low")
    }
    #xf <- 32 + 1.8*x
    # print(paste0("In Farenheit it is ", xf, "."))
    if (verbose) {
        if (x < 0) {
            print("It's freezing!")
            if (x < 32) {
                # browser()
                print("And kids don't go to school.")
                print(x ^ y)
            }
        } else {
            princ
    }
    # return(xf)
    32 + 1.8*x
}

# print("finished sourcing")
