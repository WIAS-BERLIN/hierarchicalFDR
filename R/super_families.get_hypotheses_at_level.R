#' Method for Super Family Hypotheses Trees
#'
#' This function returns the hypotheses at a given level.
#'
#' The function \code{super_families.get_hypotheses_at_level} translates
#' \code{nodes$level} of all nodes in \code{super_families} to the corresponding
#' hypotheses level. For all nodes the attribute "hypothesis_level" is added or
#' updated.
#'
#' @import data.tree
#' @export
#' @param super_families Hypotheses tree of type \code{data.tree::Node}.
#' @param level Level of the hypotheses within the tree.
#' @return A list of nodes containing all hypotheses at the given level.
#' @examples
#' super_families <- Node$new("H31")
#' super_families$AddChild("H21")$AddChild("H11")$AddSibling("H12")
#' super_families$AddChild("H13")
#'
#' super_families.get_hypotheses_at_level(super_families)
#'
#' print(super_families, "level", "hypothesis_level")
super_families.get_hypotheses_at_level <- function(super_families, level = 1) {
  H <- Traverse(super_families)
  if (max(is.na(Get(H, "hypothesis_level")))) {

    H <- Traverse(super_families, filterFun = function(node) is.null(node$children))
    Set(H, hypothesis_level = 1)

    max_level <- max(Get(H, "level"))
    for (i in 2:max_level) {
      H <- Traverse(super_families, filterFun = function(node) node$level == max_level + 1 - i)
      for (H_ in H) {
        if (!is.null(H_$children)) Set(list(H_), hypothesis_level = max(Get(H_$children, "hypothesis_level")) + 1)
      }
    }
  }

  Traverse(super_families, filterFun = function(node) Get(list(node), "hypothesis_level") == level)
}