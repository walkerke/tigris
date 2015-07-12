# Called to check to see if "state" is a FIPS code, full name or abbreviation.
#
# returns NULL if input is NULL
# returns valid state FIPS code if input is even pseud-valid (i.e. single digit but w/in range)
# returns NULL if input is not a valid FIPS code
validate_state <- function(state, .msg=interactive()) {

  if (is.null(state)) return(NULL)

  state <- tolower(str_trim(state)) # forgive white space

  if (grepl("^[[:digit:]]+$", state)) { # we prbly have FIPS

    state <- sprintf("%02d", as.numeric(state)) # forgive 1-digit FIPS codes

    if (state %in% fips_state_table$fips) {
      return(state)
    } else {
      # perhaps they passed in a county FIPS by accident so forgive that, too,
      # but warn the caller
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% fips_state_table$fips) {
        message(sprintf("Using first two digits of %s - '%s' (%s) - for FIPS code.",
                        state, state_sub,
                        fips_state_table[fips_state_table$fips == state_sub, "name"]),
                call.=FALSE)
        return(state_sub)
      } else {
        warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)
        return(NULL)
      }
    }

  } else if (grepl("^[[:alpha:]]+", state)) { # we might have state abbrev or name

    if (nchar(state) == 2 & state %in% fips_state_table$abb) { # yay, an abbrev!

      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        fips_state_table[fips_state_table$abb == state, "fips"],
                        state))
      return(fips_state_table[fips_state_table$abb == state, "fips"])

    } else if (nchar(state) > 2 & state %in% fips_state_table$name) { # yay, a name!

      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        fips_state_table[fips_state_table$name == state, "fips"],
                        state))
      return(fips_state_table[fips_state_table$name == state, "fips"])

    } else {
      warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)
      return(NULL)
    }

  } else {
    warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)
    return(NULL)
  }

}

