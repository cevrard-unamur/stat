# This package contains useful function for probablity

probability.and <- function(e, f, all, compl_e = FALSE, compl_f = FALSE) {
  result <- c()

  if (compl_e) {
    e = probability.compl(e, all)
  }

  if (compl_f) {
    f = probability.compl(f, all)
  }

  for(i in 1:length(e)) {
    for(j in 1:length(f)) {
      if (isTRUE(compare(e[i], f[j], shorten = FALSE))) {
        result <- c(result, e[i])
      }
    }
  }

  cat(sprintf("%.0f / %.0f\n\n", length(result[!duplicated(result)]), length(all)))

  return (result[!duplicated(result)])
}

probability.or <- function(e, f, all, compl_e = FALSE, compl_f = FALSE) {
  result <- c()

  if (compl_e) {
    e = probability.compl(e, all)
  }

  if (compl_f) {
    f = probability.compl(f, all)
  }

  for(i in 1:length(e)) {
    result <- c(result, e[i])

  }

  for(j in 1:length(f)) {
    result <- c(result, f[j])
  }

  cat(sprintf("%.0f / %.0f\n\n", length(result[!duplicated(result)]), length(all)))

  return (result[!duplicated(result)])
}

probability.compl <- function(e, all) {
  result <- c()

  isIn = FALSE

  for(i in 1:length(all)) {
    for(j in 1:length(e)) {
      if (isTRUE(compare(all[i], e[j], shorten = FALSE))) {
        isIn = TRUE
      }
    }

    if(!isIn) {
      result <- c(result, all[i])
    }

    isIn = FALSE
  }

  return (result[!duplicated(result)])
}
