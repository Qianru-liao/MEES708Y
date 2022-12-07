
# if else
temp = -50

if (temp < 0) {
    print("It's freezing!")
    if (temp < 32) {
        print("And kids don't go to school.")
    }
} else {
    print("It's not freezing.")
}

source("./code/fun_checkweather.R")


tempF = checkweather(55, verbose = TRUE, y = "hojoj")
checkweather(verbose = TRUE, 55)

checkweather()

10 %% 3
x = 9
(x %% 4) == 0
(x %% 400) == 0


lm(weight ~ group - 1) # formula is the 1st argument in lm()
lm(formula = weight ~ group - 1)


