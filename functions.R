# SYSTEM -----------------------------------------------------------------------
# Clean slate
clean_slate <- function(.dir,
                        .recursive = TRUE) {
  if(!dir.exists(.dir)) {
    dir.create(.dir, recursive = .recursive)
  } else {
    f <- list.files(.dir, full.names = TRUE)
    sapply(f, file.remove)
  }
}

# Check and make directory if it doesn't exist
make_dir <- function(.dir,
                     .recursive = TRUE) {
  if(!dir.exists(.dir)) {
    dir.create(.dir, recursive = .recursive)
  }
}

# Unzip files 
unzipper <- function(.zipdir,
                     .outdir = NULL,
                     .delete_zip = FALSE) {
  
  if (!is.null(.outdir)) {
    make_dir(.dir = .outdir)
  }
  zfiles <- list.files(.zipdir, pattern = "zip$", full.names = TRUE)
  sapply(zfiles, function(x) {
    unzip(x, exdir = ifelse(is.null(.outdir), .zipdir, .outdir))
  })
  if (.delete_zip == TRUE) {
    message("Deleting original .zip files....")
    sapply(zfiles, file.remove)
  }
}


# METADATA ---------------------------------------------------------------------
# Elections Metadata
init_elections_meta <- function(.state,
                                .date = "",
                                .type = "",
                                .runoff = "",
                                .special = "",
                                .outdir = "00_meta",
                                .outfile = "elections_meta.csv",
                                .force = FALSE) {
  # Directory check
  out_dir <- paste0(.state, "/", .outdir)
  make_dir(out_dir)
  
  # Stop if file exists
  if (file.exists(paste0(.state, "/", .outdir, "/", .state, "_", .outfile)) 
      & .force == FALSE) {
    return(
      message("File already exists!  Please re-run with .force = TRUE to overwrite")
    )
  }
  
  # Write to file
  tibble(
    state =.state,
    date = .date,
    type = .type,
    runoff = .runoff,
    special = .special
  ) %>%
    write_csv(file = paste0(.state, "/", .outdir, "/", .state, "_", .outfile))
}

# Offices metadata
init_offices_meta <- function(.state,
                                .date = "",
                                .type = "",
                                .runoff = "",
                                .special = "",
                                .outdir = "00_meta",
                                .outfile = "offices_meta.csv",
                              .force = FALSE) {
  
  # Directory check
  out_dir <- paste0(.state, "/", .outdir)
  make_dir(out_dir)
  
  # Stop if file exists
  if (file.exists(paste0(.state, "/", .outdir, "/", .state, "_", .outfile)) 
      & .force == FALSE) {
    return(
      message("File already exists!  Please re-run with .force = TRUE to overwrite")
    )
  }
  
  # Write to file
  tibble(
    state = .state,
    office_local = "",
    office = "",
    selection_method = "",
    retention_method = "",
    source = "",
    source_access_date = ""
  ) %>%
    write_csv(file = paste0(.state, "/", .outdir, "/", .state, "_", .outfile))
}


# INSPECT ----------------------------------------------------------------------
# Preview first n lines of file and write to file 
file_inspect <- function(infile,
                         outfile,
                         n = 5) {
  
  # Ingest and write snippet to file
  raw <- read_lines(infile)
  write_lines(x = c("############", infile, "############"),
              file = outfile,
              append = TRUE)
  write_lines(head(raw, n),
              file = outfile,
              append = TRUE)
  write_lines(c("", ""),
              file = outfile,
              append = TRUE)
} 

# Sanitize/Simplify raw contest field for downstream descriptive exploration
sanitize_contest <- function(.x,
                             .remove_punct = TRUE,
                             .lowercase = TRUE,
                             .purge_county = TRUE) {
  y <- gsub("\\([0-9A-Za-z]+\\)", "", .x)
  y <- gsub("[0-9]+[A-Za-z]*", "", y)
  if (.remove_punct == TRUE) {
    y <- gsub("[[:punct:]]", "", y)
  }
  if (.lowercase == TRUE) {
    y <- tolower(y)
  }
  if (.purge_county == TRUE) {
    y <- gsub("^[A-Za-z].+county", "", tolower(y))
  }
  str_trim(y)
}

# WRANGLING  -------------------------------------------------------------------
# Subset dataframe on offices of interest
filter_contests_df <- function(.df,
                               .x,
                               .offices = "judge|court|justice|clerk|attorney|sheriff",
                               .outfile = NULL) {
  .df %>%
    filter(grepl(.offices, tolower({{ .x }}))) %>%
    {if(!is.null(.outfile)) write_csv(., .outfile) else .}
}


# Vote Aggregator 
agg_votes <- function(.df,
                      .votes_var,
                      .agg_from_var,
                      .outfile = NULL,
                      ...) {
  .df %>%
    group_by(...) %>%
    mutate({{ .votes_var }}:= sum(as.numeric({{ .votes_var }}), 
                                  na.rm =  FALSE)) %>%
    select(-{{.agg_from_var}}) %>%
    distinct() %>%
    {if(!is.null(.outfile)) write_csv(., .outfile) else .}
}


# Subset files on offices of interest and save trimmed version 
file_trim <- function(.file,
                      .offices = "judge|court|justice|clerk|attorney|sheriff",
                      .excludes = NULL,
                      .outdir) {
  require(tidyverse)
  # Subset
  raw <- read_lines(.file)
  trimmed <- raw[grepl(.offices, tolower(raw))]
  if (!is.null(.excludes)) {
    trimmed <- trimmed[!grepl(.excludes, tolower(trimmed))]
  }
  
  if (length(trimmed) > 0) {
    filename <- ifelse(grepl("/", .file),
                       gsub("^.+/", "", .file),
                       .file)
    write_lines(trimmed, file = paste0(.outdir, "/", "trim_", filename))
    message(paste("Trimmed file saved at", 
                  paste0(.outdir, "/", "trim_", filename)))
  } else {
    message(paste("No relevant contests in document -", .file))
    message("Skipping file!")
  }
}


# Add elections metadata
add_elections <- function(.df,
                          .state,
                          .metafile) {
  read_csv(.metafile) %>%
    filter(state == .state) %>%
    mutate(across(.cols = c(runoff, special), 
           ~ifelse(is.na(.x), 0, .x))) %>%
    right_join(.df) %>%
    mutate(election_id = paste0(state, "_", gsub("-", "", as.character(date))))
}

# Add office_local
add_office_local <- function(.df,
                             .x,
                             .state) {
  
  if (.state == "NC") {
    .df %>%
      mutate(z =tolower({{ .x }}),
             office_local =
               case_when(
                 grepl("appeals", z) ~ "Court of Appeals",
                 grepl("supreme", z) ~ "Supreme Court",
                 grepl("district", z) & grepl("attorney", z) ~
                   "District Attorney",
                 grepl("clerk", z) ~ "Clerk of Superior Court",
                 grepl("superior", z) ~ "Superior Court",
                 grepl("district|d\\.? ct\\.?", z) ~ "District Court",
                 grepl("sheriff", z) ~ "Sheriff",
                 grepl("attorney", z) ~ "Attorney General"
               )) %>%
      select(-z)
  }
}
  
# Add offices
add_offices <- function(.df,
                        .state,
                        .metafile) {
  
  read_csv(.metafile) %>%
    filter(state == .state) %>%
    select(office_local, office) %>%
    right_join(.df)
}


# Add seats
seat_wrangler <- function(.df, 
                          .state = "NC",
                          .x = contest,
                          .z = NULL) {
  
  if (.state == "NC") {
    df_out <- .df %>%
      mutate(
        seat =
          case_when(
            {{ .z }} %in% c("SC", "IA") & grepl("SEAT", {{ .x }}) ~ 
              gsub("[^0-9]", "", {{ .x }}),
            {{ .z }} %in% c("SC", "IA") & grepl("Seat|contest", {{ .x }}) ~ 
              gsub("\\(|\\)", "", word({{ .x }},-2)),
            {{ .z }} %in% c("SC", "IA") ~ 
              gsub("\\(|\\)", "", str_extract({{ .x }}, "\\(.*\\)")),
            {{ .z }} == "TC2" & grepl("2 SEATS", {{ .x }}) ~ "2 SEATS",
            {{ .z }} == "TC2" & grepl("SEAT", {{ .x }}) ~ 
              gsub("SEAT ", "", str_extract({{ .x }}, "SEAT [0-9]*")),
            {{ .z }} == "TC2" ~ 
              gsub("\\(|\\)", "", str_extract({{ .x }}, "\\([A-Z]{1,}\\)")),
            {{ .z }} == "TC1" & grepl("D\\.? CT\\.?", {{ .x }}) & grepl("O NEAL", {{ .x }}) ~
              "O'NEAL",
            {{ .z }} == "TC1" & grepl("D\\.? CT\\.?", {{ .x }}) ~ 
              gsub("\\(|\\)", "", str_extract({{ .x }}, "\\([A-Z]+-?[A-Z]*\\)\\)?$")),
            {{ .z }} == "TC1" & grepl("Seat\\)?$", {{ .x }}) ~ 
              gsub("\\(|\\)", "", word({{ .x }}, -2)),
            {{ .z }} == "TC1" & grepl("SEAT", {{ .x }}) & !grepl("SEATON", {{ .x }}) ~ 
              gsub("SEAT ", "", str_extract({{ .x }}, "SEAT [0-9]*")),
            {{ .z }} == "TC1" ~
              gsub("\\(|\\)", "", str_extract({{ .x }}, "\\(.*\\)"))
          )
      )
  }
  
  # Drop leading zeros
  df_out %>%
    mutate(
      seat = str_trim(seat),
      seat = gsub("^0+", "", seat)
    )
}

# Add district
district_wrangler <- function(.df, 
                              .state = "NC",
                              .x = contest,
                              .z = office) {
  # Common assumptions
  df_out <- .df %>%
    mutate(
      district = 
        case_when(
          {{ .z }} %in% c("SC", "IA", "AG") ~ .state
        )
    )
  
  # State-specific pipelines
  if (.state == "NC") {
    df_out <- df_out %>%
      mutate(
        district =
          case_when(
            office == "TC1" & grepl("D\\.? CT\\.?", contest) ~
              gsub("\\(|\\)", "", str_extract(contest, "\\([0-9]+[A-Z]*\\)")),
            grepl("DIST[A-Z]* [0-9]+[A-Z]*", contest) ~ 
              gsub("DIST[A-Z]*", "", str_extract(contest, "DIST[A-Z]* [0-9]+[A-Z]*")),
            office %in% c("DA", "TC2") ~ gsub("\\(|\\)", "", str_extract(contest, "\\(.*\\)")),
            office %in% c("SF", "CK") ~ county,
            TRUE ~ district
          )
      )
    
    # Drop leading zeros
    df_out %>%
      mutate(
        district = str_trim(district),
        district = gsub("^0+", "", district)
      )
  }
}

# Add IDs
add_identifiers <- function(.df,
                            .state) {
  # Wrangle
  .df %>%
    mutate(state = .state,
           election_id = paste(state, date, sep = "_"),
           office_id = paste0(office, district, seat, sep = "_"))
}

# Arrange order of columns
reorder_columns <- function(.df) {
  .df %>%
    select(
      any_of(
        c("election_id", "state", "date", "type", "runoff", "special",
          "office_id", "district", "office_local", "office", "seat", "contested",
          "candidate", "party", "votes", "vote_share",
          "county", "county_id", "votes_cnty", "vote_share_cnty", "winner_cnty")
      )
    )
}

# Convert flat-file to relationship file
flat_to_relational <- function(.df,
                               .state,
                               .outdir,
                               .elections = TRUE,
                               .offices = TRUE,
                               .results = TRUE,
                               .county_results = FALSE,
                               .candidates = FALSE,
                               .flat = TRUE) {
  # Local copy
  df <- .df
  
  # Elections workflow
  if (.elections == TRUE) {
    df %>%
      select(election_id, state, date, type, runoff, special) %>%
      write_csv(file = paste0(.outdir, "/", .state, "_elections.csv"), 
                na = "")
  }
  # Offices workflow
  if (.offices == TRUE) {
    df %>%
      select(election_id, office_id, office_local, office, district, seat,
             contested) %>%
      write_csv(file = paste0(.outdir, "/", .state, "_offices.csv"), 
                na = "")
  }
  # Results workflow
  if (.results == TRUE) {
    df %>%
      select(election_id, office_id, candidate, party, votes, vote_share) %>%
      write_csv(file = paste0(.outdir, "/", .state, "_results.csv"), 
                na = "")
  }
  # County-results workflow
  if (.county_results == TRUE) {
    df %>%
      select(election_id, office_id, county, candidate, votes_cnty, 
             vote_share_cnty, winner_cnty) %>%
      write_csv(file = paste0(.outdir, "/", .state, "_county_results.csv"), 
                na = "")
  }
  # Flat file of everything
  if (.flat == TRUE) {
    .df %>%
      write_csv(file = paste0(.outdir, "/", .state, "_all.csv"), 
                na = "")
  }
}
