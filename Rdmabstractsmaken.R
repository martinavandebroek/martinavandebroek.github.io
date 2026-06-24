library(readxl)
library(dplyr)
library(stringr)

# -----------------------------
# 1. Excel inlezen
# -----------------------------
mijnabstracts <- read_excel(
  "C:/Users/u0004359/OneDrive - KU Leuven/Desktop/registraties 21 mei.xlsx",
  sheet = "abstracts2"
)

# -----------------------------
# 2. Opschonen + extra velden
# -----------------------------
mijnabstracts <- mijnabstracts %>%
  mutate(
    key = str_replace_all(`unieke naam`, "\\s+", ""),
    fullname = paste(`Family Name`, `First name`)
  )

# -----------------------------
# 3. RMarkdown content bouwen
# -----------------------------
header <- c(
  "---",
  "title: \"mODa14\"",
  "description: ABSTRACTS",
  "output: distill::distill_article",
  "---",
  "",
  "```{r setup-abstracts, include=FALSE}",
  "knitr::opts_chunk$set(echo = FALSE)",
  "library(dplyr)",
  "library(stringr)",
  "library(readxl)",
  "",
  "abstracts <- read_excel('C:/Users/u0004359/OneDrive - KU Leuven/Desktop/registraties 21 mei.xlsx', sheet='abstracts2')",
  "abstracts$key <- str_replace_all(abstracts$`unieke naam`, '\\\\s+', '')",
  "abstracts$fullname <- paste(abstracts$`Family Name`, abstracts$`First name`)",
  "```",
  ""
)

body <- c()

for (i in seq_len(nrow(mijnabstracts))) {
  
  title <- mijnabstracts$Title[i]
  name  <- mijnabstracts$fullname[i]
  aff   <- mijnabstracts$Affiliation[i]
  key   <- mijnabstracts$key[i]
  abstract <- mijnabstracts$Abstract[i]
  pdf   <- mijnabstracts$pdffile[i]
  
  key <- gsub("[^A-Za-z0-9_-]", "", key)
  
  # =========================
  # HENRY DETECTIE
  # =========================
  is_henry <- str_detect(title, regex("henry", ignore_case = TRUE)) |
    str_detect(name, regex("henry", ignore_case = TRUE))
  
  # ✅ Henry volledig overslaan
  if (is_henry) {
    next
  }
  
  # =========================
  # ABSTRACT BLOK
  # =========================
  if (is.na(abstract) || str_trim(abstract) == "") {
    
    abstract_block <- paste0(
      "<div><b>TITLE:</b> <i>", title, "</i></div>",
      "<br><br>",
      "<div><a href='", pdf, "'>Click here for abstract</a></div>"
    )
    
  } else {
    
    abstract_clean <- str_replace_all(abstract, "\r?\n", " ")
    
    abstract_block <- paste0(
      "<div><b>TITLE:</b> <i>", title, "</i></div>",
      "<br><br>",
      "<div>", abstract_clean, "</div>"
    )
  }
  
  body <- c(
    body,
    paste0("<a id='", key, "'></a>"),
    paste0("<h2 style='margin-top:40px;'>", name, "</h2>"),
    paste0("<p><strong>", aff, "</strong></p>"),
    abstract_block,
    "<div style='height:30px;'></div>"
  )
}

# -----------------------------
# 4. Alles samenvoegen
# -----------------------------
rmd_content <- c(header, body)

# -----------------------------
# 5. Rmd file schrijven
# -----------------------------
writeLines(
  rmd_content,
  "C:/Users/u0004359/OneDrive - KU Leuven/Desktop/2026moda-master-final/abstracts.Rmd"
)

cat("abstracts.Rmd successfully created\n")

#rmarkdown::render_site("abstracts.Rmd")