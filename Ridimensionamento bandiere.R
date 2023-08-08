if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")
}
library(magick)

# Codice per ridimensionare le immagini delle bandiere
flag_folder <- "Flag/"  # Assicurati che la cartella contenga solo i file delle bandiere
flags <- list.files(flag_folder, pattern = ".png", full.names = TRUE)
new_size <- c(50, 25)

for (flag_path in flags) {
  image <- image_read(flag_path)
  image_resized <- image_scale(image, paste0(new_size[1], "x", new_size[2]))
  image_write(image_resized, flag_path)
}
