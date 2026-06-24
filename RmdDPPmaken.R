library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(rmarkdown)
library(htmltools)

# =====================================================
# PATHS
# =====================================================

base_dir <- "C:/Users/u0004359/OneDrive - KU Leuven/Desktop/2026moda-master"

schedule_file <- "C:/Users/u0004359/OneDrive - KU Leuven/Desktop/schedule_clean.xlsx"

abstracts_file <- file.path(
  base_dir,
  "abstracts.rds"
)

# =====================================================
# READ DATA
# =====================================================

prog <- read_excel(schedule_file)

abstracts <- readRDS(abstracts_file)

# =====================================================
# CLEAN DATA
# =====================================================

prog <- prog %>%
  mutate(
    across(
      everything(),
      ~ na_if(as.character(.x), "")
    )
  ) %>%
  fill(
    day,
    time,
    sessionname,
    chair
  )

# =====================================================
# FIX DAY ORDER
# =====================================================

day_levels <- c(
  "Sunday, June 14 2026",
  "Monday, June 15 2026",
  "Tuesday, June 16 2026",
  "Wednesday, June 17 2026",
  "Thursday, June 18 2026",
  "Friday, June 19 2026"
)

prog <- prog %>%
  mutate(
    day = trimws(day),
    day_factor = factor(
      day,
      levels = day_levels,
      ordered = TRUE
    )
  )

# =====================================================
# GROUP SESSIONS
# =====================================================

sessions <- prog %>%
  group_by(
    day,
    day_factor,
    time,
    sessionname
  ) %>%
  summarise(
    organizer = first(organizer),
    chair = first(chair),
    speakers = list(
      na.omit(sprekers)
    ),
    .groups = "drop"
  ) %>%
  arrange(
    day_factor,
    time
  )

# =====================================================
# BUILD HTML
# =====================================================

body <- tagList()

for (i in seq_len(nrow(sessions))) {
  
  row <- sessions[i, ]
  
  # ---------------------------------------------------
  # DAY HEADER
  # ---------------------------------------------------
  
  if (i == 1 ||
      sessions$day[i] != sessions$day[i - 1]) {
    
    body <- tagAppendChildren(
      body,
      tags$h1(class = "day", row$day)
    )
  }
  
  # ---------------------------------------------------
  # SESSION TYPES
  # ---------------------------------------------------
  
  is_break <- str_detect(
    row$sessionname,
    regex("break|lunch|dinner|breakfast|registration|reception|poster|board",
          ignore_case = TRUE)
  )
  
  is_contributed <- str_detect(
    row$sessionname,
    "^Contributed"
  )
  
  is_poster <- str_detect(
    row$sessionname,
    regex("poster", ignore_case = TRUE)
  )
  
  # ---------------------------------------------------
  # SESSION BLOCK
  # ---------------------------------------------------
  
  session_block <- tags$div(
    class = "session",
    tags$div(
      class = "session-row",
      tags$div(class = "time", row$time),
      tags$div(class = "session-title", row$sessionname)
    )
  )
  
  # ---------------------------------------------------
  # META
  # ---------------------------------------------------
  
  if (!is_break) {
    
    if (!is.na(row$organizer) && !is_contributed) {
      session_block <- tagAppendChildren(
        session_block,
        tags$div(class = "meta", paste("Organizer:", row$organizer))
      )
    }
    
    if (!is.na(row$chair)) {
      session_block <- tagAppendChildren(
        session_block,
        tags$div(class = "meta", paste("Chair:", row$chair))
      )
    }
  }
  
  # ---------------------------------------------------
  # SPEAKERS HEADER
  # ---------------------------------------------------
  
  sp_list <- row$speakers[[1]]
  
  sp_list <- sp_list[!is.na(sp_list) & sp_list != ""]
  
  if (length(sp_list) > 0 && !is_poster) {
    session_block <- tagAppendChildren(
      session_block,
      tags$div(class = "meta", "Speakers:")
    )
  }
  
  # ---------------------------------------------------
  # INDIVIDUAL SPEAKERS ✅ PERFECTE ALIGNMENT
  # ---------------------------------------------------
  
  for (sp in sp_list) {
    
    info <- abstracts %>%
      filter(key == sp)
    
    if (nrow(info) > 0) {
      
      speaker_div <- tags$div(
        
        class = "speaker",
        
        HTML("&bull; "),
        
        tags$strong(info$fullname[1]),
        
        paste0(" (", info$Affiliation[1], ")"),
        
        tags$br(),
        
        # vaste insprong kolom (ESSENTIEEL)
        tags$span(
          style = "display:inline-block; width:20px;",
          ""
        ),
        
        tags$em(info$Title[1]),
        
        " ",
        
        if (!str_detect(row$sessionname, "^Henry")) {
          tags$a(
            href = paste0("abstracts.html#", URLencode(sp)),
            "abstract"
          )
        }
      )
      
    } else {
      
      speaker_div <- tags$div(
        class = "speaker",
        HTML(paste0("&bull; ", sp))
      )
    }
    
    session_block <- tagAppendChildren(
      session_block,
      speaker_div
    )
  }
  
  # ---------------------------------------------------
  # APPEND SESSION
  # ---------------------------------------------------
  
  body <- tagAppendChildren(body, session_block)
}

# =====================================================
# SAVE HTML CONTENT
# =====================================================

html_string <- renderTags(body)$html

writeLines(
  html_string,
  file.path(base_dir, "details_content.html")
)

# =====================================================
# CREATE DETAILS.Rmd
# =====================================================

rmd_lines <- c(
  
  "---",
  "title: \"mODa14\"",
  "output:",
  "  distill::distill_article:",
  "    toc: false",
  "---",
  "",
  
  "```{r setup, include=FALSE}",
  "library(htmltools)",
  "```",
  "",
  
  "<style>",
  
  "body {",
  "  font-family: Arial, sans-serif;",
  "  max-width: 1150px;",
  "  margin: auto;",
  "  line-height: 1.4;",
  "}",
  
  ".day {",
  "  margin-top: 40px;",
  "  border-bottom: 2px solid #333;",
  "  padding-bottom: 6px;",
  "}",
  
  ".session {",
  "  margin-bottom: 14px;",
  "}",
  
  ".session-row {",
  "  display: flex;",
  "  margin-top: 6px;",
  "  align-items: flex-start;",
  "}",
  
  ".time {",
  "  width: 120px;",
  "  font-weight: bold;",
  "  flex-shrink: 0;",
  "  padding-right: 8px;",
  "}",
  
  ".session-title {",
  "  font-weight: bold;",
  "  flex: 1;",
  "}",
  
  ".meta {",
  "  margin-left: 128px;",
  "  color: #555;",
  "  margin-top: 3px;",
  "  line-height: 1.35;",
  "}",
  
  ".speaker {",
  "  margin-left: 145px;",
  "  margin-top: 8px;",
  "  margin-bottom: 10px;",
  "  line-height: 1.45;",
  "}",
  
  ".speaker a {",
  "  margin-left: 10px;",
  "  color: #0066cc;",
  "  text-decoration: none;",
  "  font-weight: 500;",
  "}",
  
  ".speaker a:hover {",
  "  text-decoration: underline;",
  "}",
  
  "</style>",
  "",
  
  "```{r echo=FALSE}",
  "includeHTML('details_content.html')",
  "```"
)

# =====================================================
# WRITE DETAILS.Rmd
# =====================================================

writeLines(
  rmd_lines,
  file.path(base_dir, "DETAILS.Rmd")
)