.onAttach <- function(libname, pkgname) {

  if (!requireNamespace("cli", quietly = TRUE)) {
    suppressMessages(suppressWarnings(install.packages("cli", quietly = TRUE)))
  }
  suppressMessages(suppressWarnings(library(cli)))

  load_package <- function(pkg) {
    if (!suppressMessages(require(pkg, quietly = TRUE, character.only = TRUE))) {
      install.packages(pkg, quietly = TRUE)
      suppressMessages(library(pkg, character.only = TRUE))
    }
  }

  packages <- c("readxl", "dplyr", "tibble", "crayon", "emmeans", "multcomp", "multcompView", "agricolae")

  cli_progress_bar(
    format = "Memuat paket: {cli::pb_bar} {cli::pb_percent} | {cli::pb_current}/{cli::pb_total}",
    total = length(packages)
  )

  for (pkg in packages) {
    load_package(pkg)
    cli_progress_update()
  }

  cli_progress_done()

  vers <-  "0.1.0"
  library(crayon)
  packageStartupMessage("")
  packageStartupMessage(bold(green("CropID")))
  packageStartupMessage("")
  packageStartupMessage("Indonesian Agricultural Research Related Functions and Data")
  packageStartupMessage("Author   : Ozik Putra Jarwo")
  packageStartupMessage("Version  : ", vers)
  packageStartupMessage("Github   : https://github.com/OzikPutraJarwo/cropid")
  packageStartupMessage("Contact  : https://www.kodejarwo.com")
  packageStartupMessage("")
}