source("renv/activate.R")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("here")
# install.packages("stringr")
# install.packages("patchwork")
# install.packages("tidysynth")
# install.packages("Synth")


if (interactive() & requireNamespace("shrtcts", quietly = TRUE)) {
  shrtcts::add_rstudio_shortcuts()
}

if (interactive() && Sys.getenv("RSTUDIO") == "") {
  source(file.path(Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"), ".vscode-R", "init.R"))
}


