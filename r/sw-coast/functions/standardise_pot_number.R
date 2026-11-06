#' Standardise lobster pot numbers recorded across survey years
#'
#' Pot numbers are entered by hand in the field and arrive in inconsistent
#' forms: padded ("02"), uncertain ("45?") and non-numeric extra pots
#' ("F3", "F3-2", "FIII"). Numeric pots are reduced to a plain integer string so
#' that "02" and " 2 " match, while non-numeric pots keep their own identity -
#' stripping digits out of "F3" would silently merge it with pot 3.

standardise_pot_number <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA", "-")] <- NA_character_

  is_numeric_pot <- grepl("^[0-9]+\\??$", x) & !is.na(x)
  x[is_numeric_pot] <- as.character(as.integer(gsub("\\?", "", x[is_numeric_pot])))

  x
}
