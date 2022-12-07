#####################################################################
#####################################################################
# Debugugging
as.Date(c("2011-04-05", 35))

X <- rep(NA, nrow(swiss))
for (i in 1:nrow(swiss)) { # i = 40
    m = lm(unlist(swiss[i, ]) ~ 1)
    X[i] = m$coefficients[1]
}
X

apply(swiss, 1, mean) # row means
apply(swiss, 2, mean, na.rm = TRUE) # column means

apply(swiss, 1, function(g) {
    m = lm(unlist(g) ~ 1)
    m$coefficients[1]
})


# Warning
tmp <- log(-pi)
is.na(tmp)

library(dplyr)
swiss %>%
    filter(!is.na(Agriculture))

# Error
lm(x ~ y) # we do not have x and y in our environment


#### TOOLS
traceback()

mean(x)
traceback()

# browser() and debug()
source('MyFunctions.R')


debug(checkweather)
checkweather(-80, verbose = TRUE)
undebug(checkweather)

debug(mean)
mean(x)
undebug(mean)

debug(funtimes::ARest)
funtimes::ARest(x = rnorm(100))
undebug(funtimes::ARest)
