se <- function(x) sd(x, na.rm = T)/sqrt(length(x[!is.na(x)]))
