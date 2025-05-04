#' @title Structural Equation Model (SEM) Effects Analysis and Visualization
#'
#' @description This function calculates direct, indirect, and total effects for a target variable in a SEM model.
#'              It supports visualization of effects through bar plots and includes automated dependency checks.
#'
#' @param object A fitted SEM model object of class \code{"lavaan"}.
#' @param target Character string specifying the target variable name for effect analysis.
#' @param plot Logical indicating whether to generate effect visualization plots (default: \code{TRUE}).
#' @param total_only Logical controlling plot mode. If \code{TRUE}, shows only total effects with customizable colors;
#'   if \code{FALSE}, displays all effect types with palette coloring (default: \code{FALSE}).
#' @param total_colors Single color or vector of colors for total effect bars when \code{total_only=TRUE}
#'   (default: \code{"skyblue"}).
#' @param color_palette Character vector of 3 colors for direct/indirect/total effects when \code{total_only=FALSE}
#'   (default: \code{c("darkgreen", "skyblue", "orange")}).
#'
#' @return A list containing two components:
#' \itemize{
#'   \item \code{effect_table}: A data frame with variables and their standardized effect values (direct, indirect, total)
#'   \item \code{plot_object}: A ggplot2 object (if plot=TRUE), NULL otherwise
#' }
#'
#' @examples
#' \donttest{
#' # Example using lavaan::sem()
#' library(lavaan)
#' model <- '
#'   # Measurement model
#'   ind60 =~ x1 + x2 + x3
#'   dem60 =~ y1 + y2 + y3 + y4
#'   dem65 =~ y5 + y6 + y7 + y8
#'
#'   # Structural model
#'   dem60 ~ ind60
#'   dem65 ~ ind60 + dem60
#' '
#' fit <- sem(model, data = PoliticalDemocracy)
#'
#' # Analyze effects for target variable "dem65"
#' results <- sem_effects(fit, target = "dem65")
#' print(results$effect_table)
#'
#' # Customize plot appearance
#' results$plot_object +
#'   ggplot2::theme_minimal() +
#'   ggplot2::ggtitle("Standardized Effects for dem65")
#' }
#'
#' @importFrom lavaan lavInspect
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual geom_hline labs theme_bw position_dodge
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% select
#' @importFrom utils install.packages
#' @import checkmate
#' @export

sem_effects <- function(
    object,
    target,
    plot = TRUE,
    total_only = FALSE,
    total_colors = "skyblue",
    color_palette = c("darkgreen", "skyblue", "orange")) {

# Automatically install missing dependencies:
  required_packages <- c("lavaan", "ggplot2", "checkmate", "tidyr", "dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, warn.conflicts = FALSE)
    )
  }

  # Parameter validation:
  checkmate::assert_class(object, "lavaan")
  checkmate::assert_string(target)
 checkmate::assert_subset(target, rownames(lavInspect(object)$beta))
checkmate::assert_flag(total_only)

  # Get normalized path coefficients:
  std <- lavInspect(object, "std")
  beta <- std$beta
  n <- nrow(beta)

  # Verify the validity of the target:
  if (!target %in% rownames(beta)) {
    stop(paste("Target variable", target, "not found in model matrix"))
  }

  # Calculate the direct effects matrix:
  direct_effect <- beta[target, ]

  # Calculate the indirect effects matrix:
  indirect_effect_matrix <- matrix(0, nrow = n, ncol = n)
  for (k in 2:n) {
    indirect_effect_matrix <- indirect_effect_matrix + Reduce(`%*%`, replicate(k, beta, simplify = FALSE))
  }

  indirect_effect <- indirect_effect_matrix[target, ]
  total_effect <- direct_effect + indirect_effect

  # Build a result data frame:
  effect_results <- data.frame(
    Variable = colnames(beta),
    Direct_Effect = direct_effect,
    Indirect_Effect = indirect_effect,
    Total_Effect = total_effect
  )

  # Filter the influence of the target variable itself:
  effect_filtered <- effect_results[effect_results$Variable != target, ]

# Check the all-zero effects:
  zero_rows <- rowSums(abs(effect_filtered[, c("Direct_Effect", "Indirect_Effect", "Total_Effect")])) < 1e-10

  if (any(zero_rows)) {
    zero_vars <- effect_filtered$Variable[zero_rows]
    warning(
      "The target variable'", target, "'might be incorrect due to variable(s) with all-zero effect(s): ", paste(zero_vars, collapse = ", "), ". If this is expected, you can disregard the warning."
    )
  }


  # Long format data:
 effect_long <- tidyr::pivot_longer(effect_filtered,  cols = -Variable,
                                            names_to = "Effect_Type",  values_to = "Effect_Value" )


# ggplot:
  if (plot) {
     if (total_only) {  # Total effect only:
      effect_total <- effect_filtered %>% select(Variable, Total_Effect)

      if (length(total_colors) == 1) {
        colors <- rep(total_colors, nrow(effect_total))
      } else {
        colors <- total_colors[1:nrow(effect_total)]
      }
         p <- ggplot(effect_total, aes(x = Variable, y = Total_Effect, fill = Variable)) +
             geom_col(width = 0.5) +
            scale_fill_manual(values = colors) +
            geom_hline(yintercept = 0) +
            labs(x = NULL, y = "Total effects") +
            theme_bw()
    } else  {  # All 3 effects:

    p <- ggplot(effect_long, aes(x = Variable, y = Effect_Value, fill = Effect_Type)) +
      geom_col(position = position_dodge(0.9), width = 0.8) +
      scale_fill_manual(values = color_palette) +
      geom_hline(yintercept = 0) +
      labs(x = NULL, y = "Standardized effects") +
      theme_bw()
      }

    print(p)
  }

  # Return:
  return(list(
    effect_table = effect_filtered,
    plot_object = if (exists("p")) p else NULL
  ))
}

# Examples:
# results <- sem_effects(fit, target = "dem65")
# print(results$effect_table)
# results$plot_object + theme_minimal()
