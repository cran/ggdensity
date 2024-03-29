# this script contains several unexported helper functions

# normalization/scaling functions
normalize <- function(v) v / sum(v)
standardize <- function(v, min = min(v), max = max(v)) (v - min) / (max - min)
rescale <- function(v) v / max(v)

# discrete approximation to P[f^(X,Y) >= c]
prob_above_c <- function(df, c) {
  if (length(c) > 1) return(vapply(c, prob_above_c, df = df, numeric(1)))
  with(df, sum(fhat_discretized[fhat >= c]))
}

# numerical approximation for finding hdr
# if method = "histogram", don't want to use uniroot, runs into issue if n is small
find_cutoff <- function(df, conf, uniroot = TRUE) {

  if (length(conf) > 1) return(vapply(conf, function(x) find_cutoff(df, x, uniroot), numeric(1)))

  # the following is set to FALSE to override when other code calls
  # the function with uniroot = TRUE, remove once we're confident the
  # code works in all circumstances
  if (FALSE) {

    uniroot(function(c) prob_above_c(df, c) - conf, lower = 0, upper = 1)$root

  } else {

    # sort df rows by fhat
    df <- df[order(df$fhat, decreasing = TRUE),]

    # compute cumsum of probs
    df$cumprob <- cumsum(df$fhat_discretized)

    # determine cutoff
    max(df[df$cumprob >= conf,]$fhat)

  }

}
