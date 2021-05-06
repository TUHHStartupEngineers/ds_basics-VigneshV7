library(data.table)
library(magrittr)

aq_dt <- data.table(airquality)

aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]


mtcars_dt <- data.table(mtcars)

# mtcars_dt[, mileage_type := ifelse(mpg > 20, "high", "low")]

mtcars_dt[,.(.N, mileage = mean(mpg) %>% round(2)), by = gear]