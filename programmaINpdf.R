# install.packages(c("pagedown", "fs"))

library(pagedown)
library(fs)

html_to_pdf <- function(
    html_file = "docs/DETAILS.html",
    output_pdf = "files/mODa14_programma.pdf"
) {
  
  # Absolute paden
  html_file <- path_abs(html_file)
  output_pdf <- path_abs(output_pdf)
  
  # Maak outputmap aan indien nodig
  dir_create(path_dir(output_pdf))
  
  # Werkmap tijdelijk aanpassen zodat site_libs, css en images werken
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  setwd(path_dir(html_file))
  
  if (file_exists(output_pdf)) {
    file_delete(output_pdf)
  }
  
  # HTML -> PDF
  chrome_print(
    input = path_file(html_file),
    output = output_pdf,
    wait = 3,
    timeout = 60,
    format = "pdf",
    options = list(
      printBackground = TRUE,
      preferCSSPageSize = TRUE,
      marginTop = 0.4,
      marginBottom = 0.4,
      marginLeft = 0.4,
      marginRight = 0.4
    )
  )
  
  message("PDF gemaakt: ", output_pdf)
  
  return(output_pdf)
}

# Uitvoeren
html_to_pdf()
