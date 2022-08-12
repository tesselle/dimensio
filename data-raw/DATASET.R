## code to prepare `DATASET` dataset goes here

## Data from Greenacre 2007, p. 46
countries <- read.csv("data-raw/countries.csv", row.names = 1)
usethis::use_data(countries, overwrite = FALSE)

## Data from Greenacre 2007, p. 86
benthos <- read.csv("data-raw/benthos.csv", row.names = 1)
usethis::use_data(benthos, overwrite = FALSE)

## Data from Lebart et al. 2006, p. 170-172
colours <- data.frame(
  brun = c(68, 15, 5, 20),
  chatain = c(119, 54, 29, 84),
  roux = c(26, 14, 14, 17),
  blond = c(7, 10, 16, 94),
  row.names = c("marron", "noisette", "vert", "bleu")
)
usethis::use_data(colours, overwrite = FALSE)
