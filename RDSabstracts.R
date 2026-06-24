library(readxl)
library(dplyr)
library(stringr)

mijnabstracts <- read_excel(
  "C:/Users/u0004359/OneDrive - KU Leuven/Desktop/registraties 21 mei.xlsx",
  sheet = "abstracts2"
)

mijnabstracts <- mijnabstracts %>%
  mutate(
    key = str_replace_all(`unieke naam`, "\\s+", ""),
    fullname = paste(`Family Name`, `First name`)
  )

saveRDS(mijnabstracts, "C:/Users/u0004359/OneDrive - KU Leuven/Desktop/2026moda-master/abstracts.rds")

cat("abstracts.rds saved\n")