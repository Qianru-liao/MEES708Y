# Data ----

## data.frame ----
worm <- read.table("./dataraw/worms.missing.txt", head = TRUE)
head(worm)
is.data.frame(worm)

worm = worm[order(worm$Area), ]

tmp <- scan("./dataraw/Lengths.dat",
            what=list(Name="",Family="",Length=0),
            na.string=".")
marine = as.data.frame(tmp)
na.omit(marine)
is.na(marine$Family)
marine2 <- marine[!is.na(marine$Family), ]

marine[order(marine$Family, marine$Length), ]

aggregate(Length ~ Family, data = marine, mean,
          trim = 0.3)

# NA
x = c(1, 3, 5, NA)
mean(x, na.rm = TRUE)
lm()

## data.table  ----

##  tibble / dplyr ----
library(dplyr)
worm_tbl = as_tibble(worm)
head(worm_tbl)

marine %>%
    # arrange(Family, Length) %>%
    group_by(Family) %>%
    summarise(Length = mean(Length))

filter(starwars,
       is.element(hair_color, c("blond", "none")) &
           eye_color == "black")


# merging
descriptions <- read.fwf("./dataraw/Chocolate.dat",
                         widths = c(4, 10, 46),
                         col.names = c("CodeNum", "Name", "Description"))
descriptions <- na.omit(descriptions)
sales <- read.table("./dataraw/chocsales.dat", header = FALSE,
                    col.names = c("CodeNum", "PiecesSold"))
# base R
X <- merge(sales, descriptions, all = TRUE)

# dplyr
X2 <- sales %>%
    full_join(descriptions, by = "CodeNum")


library(reshape2)
baseball <- read.table("./dataraw/Transpos.dat",
                       head = FALSE,
                       col.names = c("Team", "Player", "Type", "Entry"))
baseball.m <- melt(baseball,
                   idvars = c("Team", "Player", "Type"),
                   measure.vars = "Entry")
head(baseball.m)

