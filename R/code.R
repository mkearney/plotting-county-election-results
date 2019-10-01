## load dplyr
library(dplyr)
library(maps)

## read data from github
d <- readr::read_csv("https://github.com/mkearney/presidential_election_county_results_2016/blob/master/data/pres.elect16.results.2018.csv?raw=true")

## filter to trump and clinton, calculate relative vote shares, and generate
## one estimate (trump's relative vote share) per county
d %>%
  filter(cand %in% c("Donald Trump", "Hillary Clinton"), !is.na(county)) %>%
  group_by(st, county) %>%
  summarise(trump = pct[cand == "Donald Trump"] / sum(pct),
    pct = pct[cand == "Donald Trump"],
    fips = unique(fips)) ->
  d

## get vector of polygon names
nms <- maps::map("county", namesonly = TRUE)

## get county.fips data
data(list = "county.fips", package = "maps", envir = cf <- new.env())
cf <- get("county.fips", envir = cf)

## join trump [relative] vote share estimate to county fips data
cf$trump <- d$trump[match(cf$fips, d$fips)]

## there's one missing value–fill in correct value
cf$trump[is.na(cf$trump) & cf$polyname == "south dakota,shannon"] <- 0.66

## reorder trump vote share estimates to match polygname names
trump_share <- cf$trump[match(nms, cf$polyname)]

## create 100-value red-to-blue color ramp
rb <- colorRampPalette(c("#2222dd", "#dd2222"))(100)

## convert trump_share to percent (round to nearest whole)
## and use color ramp as index
colc <- rb[round(trump_share * 100, 0)]

## create and save percentile/gradient version of map
png("img/county-map-gradient.png",
  width = 12, height = 6.4, units = "in", res = 312)
par(mar = c(0, 0, 0, 0))
map("county", fill = TRUE, col = colc, lwd = 0.08, mar = c(0, 0, 0, 0))
dev.off()

## now create color values based on whether trump's relative vote share >= .50
colc2 <- ifelse(trump_share >= 0.500, "#dd2222", "#2222dd")

## create and save win/loss binary version (majority vote share)
png("img/county-map-binary.png",
  width = 12, height = 6.4, units = "in", res = 312)
par(mar = c(0, 0, 0, 0))
map("county", fill = TRUE, col = colc2, lwd = 0.08,
  mar = c(0, 0, 0, 0))
dev.off()
