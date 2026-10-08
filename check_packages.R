# Check packages ===============================================================
#
# Checks every package the pipeline uses is installed locally, and that the
# mrc-ide packages match the latest commit on their GitHub default branch.
# provision.R is the smaller list for the cluster (demography + calibration).
#
# Run from the repo root. Set install <- TRUE to install/update what's flagged.

install <- FALSE

# Packages ---------------------------------------------------------------------
# mrc-ide packages, installed from GitHub (default branch)
github <- c(
  "mrc-ide/malariasimulation", "mrc-ide/site", "mrc-ide/netz", "mrc-ide/umbrella",
  "mrc-ide/peeps", "mrc-ide/cali", "mrc-ide/postie"
)
# Everything else (CRAN, plus hipercow from the mrc-ide r-universe)
cran <- c(
  "orderly", "hipercow", "remotes", "dplyr", "tidyr", "purrr", "stringr", "rlang",
  "sf", "terra", "ggplot2", "patchwork", "scales", "png", "qpdf", "knitr", "quarto",
  "jsonlite", "countrycode", "minpack.lm", "malariaAtlas"
)
repos <- c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org")

# Check the list matches the code ----------------------------------------------
# Packages called as pkg:: or library(pkg) anywhere in the pipeline
files <- c(
  "mission_control.R",
  list.files(c("src", "shared", "operations"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
)
code <- sub("#.*$", "", unlist(lapply(files, readLines)))
used <- c(
  unlist(regmatches(code, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*(?=::)", code, perl = TRUE))),
  unlist(regmatches(code, gregexpr("(?<=library\\()[A-Za-z0-9.]+", code, perl = TRUE)))
)
used <- setdiff(unique(used), rownames(installed.packages(priority = "base")))
listed <- c(basename(github), cran)
if(length(setdiff(used, listed)) > 0){
  warning("Used in the code but missing from this list: ", paste(setdiff(used, listed), collapse = ", "))
}

# Packages provision.R installs on the cluster but this list doesn't cover.
# Not checked: if demography or calibration start using a new package, add it to
# provision.R by hand and re-run hipercow::hipercow_provision().
provision <- readLines("provision.R")
provisioned <- basename(unlist(regmatches(provision, gregexpr("(?<=\\(\")[^\"]+", provision, perl = TRUE))))
if(length(setdiff(provisioned, listed)) > 0){
  message("In provision.R but not used by the code: ", paste(setdiff(provisioned, listed), collapse = ", "))
}

# Status -----------------------------------------------------------------------
have <- rownames(installed.packages())
installed_sha <- function(pkg){
  if(!pkg %in% have) return(NA_character_)
  sha <- utils::packageDescription(pkg)$RemoteSha
  if(is.null(sha)) "not from GitHub" else sha
}
latest_sha <- function(repo){
  tryCatch(
    jsonlite::fromJSON(paste0("https://api.github.com/repos/", repo, "/commits/HEAD"))$sha,
    error = function(e) NA_character_
  )
}

gh_status <- data.frame(package = basename(github), repo = github)
gh_status$installed <- vapply(gh_status$package, installed_sha, "")
gh_status$latest <- vapply(gh_status$repo, latest_sha, "")
gh_status$status <- ifelse(
  is.na(gh_status$installed), "missing",
  ifelse(is.na(gh_status$latest), "couldn't reach GitHub",
         ifelse(gh_status$installed == gh_status$latest, "up to date", "out of date"))
)

missing_cran <- setdiff(cran, have)
old <- utils::old.packages(repos = repos)
outdated_cran <- intersect(cran, rownames(old))

cat("\nmrc-ide packages (GitHub):\n")
print(gh_status[, c("package", "status")], row.names = FALSE)
cat("\nOther packages missing:", if(length(missing_cran)) paste(missing_cran, collapse = ", ") else "none", "\n")
cat("Other packages with updates available:", if(length(outdated_cran)) paste(outdated_cran, collapse = ", ") else "none", "\n")

# Install ----------------------------------------------------------------------
if(install){
  to_install <- union(missing_cran, outdated_cran)
  if(length(to_install) > 0) utils::install.packages(to_install, repos = repos)
  to_update <- gh_status$repo[gh_status$status %in% c("missing", "out of date")]
  for(repo in to_update) remotes::install_github(repo, upgrade = "never")
} else if(length(missing_cran) + length(outdated_cran) + sum(gh_status$status %in% c("missing", "out of date")) > 0){
  cat("\nSet install <- TRUE and re-run to install/update the packages above.\n")
}
