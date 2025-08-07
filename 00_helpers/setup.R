### BASIC SETUP###

# LIBRARIES ####
lib_standard <- c(
  "DT",         # Create dynamic tables
  "GGally",     # Quick pairs plots
  "janitor",    # Clean white space
  "knitr",      # Print tables
  "plotly",     # Interactive plots
  "ggrepel",    # Nudged graph labels
  "ggplot2",    # Data visualization
  "readxl",     # Read Excel files
  "openxlsx",   # Create Excel workbooks
  "data.table", # Wild cards (%like%)
  "lubridate",  # For dynamic timestamp
  "tidyverse",   # Core packages for data formatting
  "rmarkdown"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

# install packages if not yet installed
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

lapply(lib_standard, install_if_missing)
lapply(lib_standard, library, character.only = TRUE)

# WORKING DIRECTORIES ####

# Full data path for internal use
path_own_data       <- file.path("../own_data")
path_output_data    <- file.path(path_own_data, "output_data")
path_helpers <- "../00_helpers"
path_ext_data       <- file.path("..//")

# CITATIONS ####

get_custom_bibtex <- function(lib_name) {
  # Get the citation information for the package
  cit <- citation(lib_name)
  # Use the first citation if there are multiple options for one library, e.g. knitr
  first_cit <- cit[[1]]
  # Convert to BibTeX format
  bibtex_entry <- toBibtex(first_cit)
  # Modify BibTeX entry to use the package name as the citation key
  bibtex_entry[[1]] <- paste0("@software{", lib_name, ",")
  
  return(bibtex_entry)
}

package_fun <- function(libraries, topic){
  # save packages to object
  lib_string <- c(paste0(libraries))
  # install if missing
  lapply(libraries, install_if_missing)
  # load packages
  lapply(lib_string, library, character.only = TRUE)
  # get citation for each package
  bib_entries <- lapply(lib_string, get_custom_bibtex)
  # write citations to a .bib file
  writeLines(unlist(bib_entries), paste0(location, "/lib_citations_", topic ,".bib"))
}

# PLOT THEMES ####
theme_report <- function(angle = 0, legend.position = "right", hjust = 0, axis.text.size = 11, border.color = "red", border.width = 0) {
  theme(
    axis.text = element_text(size = axis.text.size, hjust = hjust),
    axis.title = element_text(size = 14),
    axis.title.y = element_text(vjust = 2),
    axis.title.x = element_text(vjust = -0.4),
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    # legend.background = element_rect(colour = "#BFBFBF", fill = 'white'),
    # legend.key = element_rect(fill = "white"),
    panel.background = element_rect(colour = "#BFBFBF", fill = "transparent", color = if (border.width > 0) border.color else NA, linewidth = border.width),
    panel.grid = element_line(color = "#E8E8E8"),
    legend.position = legend.position,
    axis.text.x = element_text(vjust = 0.5, hjust = hjust, angle = angle),
    plot.background = element_rect(color = if (border.width > 0) border.color else NA, fill = "transparent", linewidth = border.width),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
}

theme_poster <- function (angle = 0, legend.position = "right", hjust = 0, axis.text.size = 12, plot.margin = margin(1, 1, 1, 1, "cm"), legend.text=element_text(size = 12), strip.text = element_text(size = 12)) {
  theme(axis.text = element_text(size = axis.text.size),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(vjust = +2),
        axis.title.x = element_text(vjust = -0.4),
        legend.text = legend.text,
        legend.title = element_blank(),
        #legend.background = element_rect(colour = "#BFBFBF", fill = 'white'), # legend bg
        #legend.key = element_rect(fill = "white"),
        panel.background = element_rect(colour = "#BFBFBF", fill = "transparent"),
        panel.grid = element_line(color = "#E8E8E8"),
        axis.text.x = element_text(vjust = 0.5, hjust = hjust, angle = angle),
        legend.position = legend.position,
        plot.margin = plot.margin,
        strip.text = strip.text,
        legend.key = element_blank(),
        legend.background = element_blank()
  )
}

fun_get_caic <- function(x) {
  tryCatch({
    cAIC(x)$caic # accessing the specific value from the cAIC function
  }, error = function(e) {
    NA # return NA or some indication of failure for models that cause an error
  })
}

labels <- read.xlsx(paste0("../own_data/labels.xlsx"))

rename_param <- c(
  GDD_conservative = "GDD_SUM_QUANTAVE3",
  CDD_conservative = "CDD_SUM_QUANTAVE5",
  GDD_mainstream = "GDD_SUM_MEANAVE3",
  CDD_mainstream = "CDD_SUM_MEANAVE5",
  AWC = "SUM_AWC_ADJUSTED_ALL",
  DROUGHT = "CLIM_BALANCE6"
)

rename_species <- c(
  "FS" = "Fagus sylvatica",
  "LD" = "Larix decidua",
  "PA" = "Picea abies",
  "TC" = "Tilia cordata"
)

rename_species_abbr <- c(
  "FS" = "Fagus",
  "LD" = "Larix",
  "PA" = "Picea",
  "TC" = "Tilia"
)

rename_species_en <- c(
  "FS" = "Beech",
  "LD" = "Larch",
  "PA" = "Spruce",
  "TC" = "Lime"
)

rename_species_de <- c(
  "FS" = "Buche",
  "LD" = "Lärche",
  "PA" = "Fichte",
  "TC" = "Linde"
)

# Helper function for renaming
rename_fun <- function(data) {
  data %>%
    rename(!!!syms(rename_param))
}

# SUBSETS FOR CONSISTENCY CHECKS ####
site_subset <- c("ADB_FS","SEW_FS", "CHT_FS", "SLN_FS", "ENB_FS", "VSA_FS")

# SHORTCUTS FOR FORMATTING ####
species_color_scale <- function(lang = "latin") {
  species_names <- list(
    latin = c("Fagus sylvatica" = "#6C8ED4", "Larix decidua" = "#99AA22", "Picea abies" = "#644001", "Tilia cordata" = "#FFC83D"),
    english = c("Beech" = "#6C8ED4", "Larch" = "#99AA22", "Spruce" = "#644001", "Lime" = "#FFC83D"),
    german = c("Buche" = "#6C8ED4", "Lärche" = "#99AA22", "Fichte" = "#644001", "Linde" = "#FFC83D"),
    short = c("FS" = "#6C8ED4", "LD" = "#99AA22", "PA" = "#644001", "TC" = "#FFC83D")
  )
  
  scale_color_manual(
    values = species_names[[lang]],
    na.translate = FALSE
  )
}

# Labels
label_table <- read.xlsx(paste0("../own_data/labels.xlsx"), sheet = "label_table_c2") %>% as.tibble()

# rename labels
label_fun_sim <- function(term, col = "full") {
  label_lookup <- label_table %>%
    select(term, !!sym(col)) %>%
    deframe()
  
  result <- map_chr(term, function(t) {
    if (str_detect(t, ":")) {
      return(str_c(map_chr(str_split(t, ":", simplify = TRUE), ~ label_fun_sim(.x, col)), collapse = ":"))
    }
    
    base_term <- str_extract(t, "(?<=poly\\().*?(?=,|\\))")
    
    poly_suffix <- case_when(
      str_detect(t, "\\)1$") ~ " linear",
      str_detect(t, "\\)2$") ~ " quadratic",
      str_detect(t, "\\)3$") ~ " cubic",
      str_detect(t, "\\)$") ~ ")",
      TRUE ~ ""
    )
    
    poly_prefix <- case_when(
      str_detect(t, "\\)$") ~ "poly(",
      TRUE ~ ""
    )
    
    label <- label_lookup[t] %||% t
    base_label <- label_lookup[base_term] %||% base_term
    
    result_label <- case_when(
      !is.na(base_term) & col == "short" ~ paste0(poly_prefix, base_label, poly_suffix),
      !is.na(base_term) & col == "full" ~ paste0(base_label),
      !is.na(base_term) & col == "category" ~ paste0(base_label),
      !is.na(base_term) & col == "model_eq" ~ paste0(poly_prefix, base_label, poly_suffix),
      TRUE ~ label
    )
  })
  
  if (col == "category") {
    result <- str_extract(result, "^[^:]+")
  }
  
  return(result)
}