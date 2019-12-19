#' Test of a Super Family Hypotheses Tree
#'
#' Tests all hypotheses nodes contained in a super family hypotheses tree.
#'
#' The nodes in \code{super_families} are tested recursively. For testing the
#' level-1 hypotheses the attribute "p_values" assigned with a vector of
#' p-values is required. The function \code{\link{hierasymptkappa}} is applied
#' on the the list of p-value vectors. For level larger than one a hypothesis is
#' rejected if and only if at least one child hypothesis is rejected.
#'
#' @import data.tree
#' @export
#' @param super_families Hypotheses tree of type \code{data.tree::Node}.
#' @param kappa The tuning parameter \code{kappa} describes the percentage of signals
#'   (small p-values) needed for a level-1 hypotheses to be rejected, i.e., to
#'   be not considered as noise only. See the parameter \code{kappa} in
#'   \code{\link{hierasymptkappa}}.
#' @return The hypotheses tree super_families with added or updated attribute
#'   "test_results" for level-1 hypotheses (logical vector) and attribute "rejected" for
#'   all nodes. (TRUE or FALSE)
#' @examples
#' super_families <- Node$new("H31")
#' super_families$AddChild("H21")$AddChild("H11")$AddSibling("H12")
#' super_families$AddChild("H13")
#'
#' set.seed(1)
#' for (H in super_families.get_hypotheses_at_level(super_families, level = 1)) {
#'   p_values <- runif(10)
#'   ind <- sample(1:10, sample(c(0,1)))
#'   p_values[ind] <- 0
#'   Set(list(H), p_values = list(p_values))
#' }
#' rm(ind, p_values)
#' super_families.test(super_families)
#' Get(Traverse(super_families), "p_values")
#' Get(Traverse(super_families), "test_results")
#'
#' print(super_families, "rejected")
super_families.test <- function(super_families, kappa = 0.001) {
  H <- super_families.get_hypotheses_at_level(super_families, level = 1)
  p_values_list <- Get(H, function(node) list(node$p_values))
  test_results <- hierasymptkappa(p_values_list, kappa = kappa) ## this is a two-stage procedure; stage 1: filter inactive level-1 hypotheses using kappa, i.e., do not reject any individual hypotheses contained in these level-1 hypotheses; stage 2: test the individual hypotheses contained in the remaining level-1 hypotheses;
  Set(H, test_results = test_results, rejected = lapply(test_results, sum) > 0)
  max_level <- max(Get(H, "level"))
  for (level in 2:max_level) {
    H <- super_families.get_hypotheses_at_level(super_families, level)
    for (H_ in H) {
      if (!is.null(H_$children)) Set(list(H_), rejected = as.logical(max(Get(H_$children, "rejected"))))
    }
  }
}