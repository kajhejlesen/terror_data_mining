library(data.table)

data <- as.data.table(read.csv(file = "data/terror.csv"))


countries_kill_by_year <- data[, j=sum(nkill, na.rm=TRUE), by = list(country_txt, iyear)]
countries_kill_by_year_wide <- reshape(countries_kill_by_year, timevar = "country_txt", idvar = c("iyear"), direction = "wide", )

setnames(countries_kill_by_year_wide, colnames(countries_kill_by_year_wide), c("year", levels(data$country_txt)))


