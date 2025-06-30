#' @title Structural Equation Model Effect Analysis and Visualization
#'
#' @description Provides standardized effect decomposition (direct, indirect, and total effects) for three major structural equation modeling frameworks:
#'              'lavaan', 'piecewiseSEM', and 'plspm'. Automatically handles zero-effect variables, generates publication-ready 'ggplot2' visualizations, and returns
#'              both wide-format and long-format effect tables. Supports effect filtering, multi-model object inputs, and customizable visualization parameters.
#'
#' @param object SEM object (lavaan/psem/plspm).
#' @param target Character string specifying the target variable name for effect analysis.
#' @param plot Logical indicating whether to generate effect visualization plots (default: \code{TRUE}).
#' @param delete_zero_effect Logical indicating whether to removes rows where all specified effect columns contain only zeros (default: \code{TRUE}).
#' @param total_only Logical controlling plot mode. If \code{TRUE}, shows only total effects with customizable colors;
#'   if \code{FALSE}, displays all effect types with palette coloring (default: \code{FALSE}).
#' @param total_color Single color or vector of colors for total effect bars when \code{total_only=TRUE}
#'   (default: \code{"skyblue"}).
#' @param color_palette Character vector of 3 colors for direct/indirect/total effects when \code{total_only=FALSE}
#'   (default: \code{c("darkgreen", "skyblue", "orange")}).
#'
#' @return A list containing three components:
#' \itemize{
#'   \item \code{effect_table}: A data frame with variables and their standardized effect values (direct, indirect, total)
#'   \item \code{effect_long}: A long-format version of \code{effect_table}
#'   \item \code{plot_object}: A ggplot2 object (if plot=TRUE), NULL otherwise
#' }
#'
#' @author Weiping Mei
#'
#' @seealso \code{\link[lavaan]{sem}}, \code{\link[piecewiseSEM]{psem}}, \code{\link[plspm]{plspm}}
#'
#' @examples
#' \donttest{
#'
#' # Example 01: lavaan -------------------------------
#'
#' library(lavaan)
#'
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
#'
#' print(results$effect_table)
#' print(results$effect_long)
#' print(results$plot_object)
#'
#' # Customize plot appearance
#' results$plot_object +
#'   ggplot2::coord_flip()+
#'   ggplot2::theme_minimal() +
#'   ggplot2::ggtitle("Standardized effects for dem65")
#'
#'
#' # Example 02: piecewiseSEM --------------------------
#'
#' library(piecewiseSEM)
#' pmod <- psem(
#'   lm(rich ~ cover, data = keeley),
#'   lm(cover ~ firesev, data = keeley),
#'   lm(firesev ~ age, data = keeley),
#'   data = keeley
#'   )
#'
#' sem_effects(pmod, target = "rich",
#'         color_palette = c("darkgreen", "grey80", "purple"))
#'
#'
#' # Example 03: plspm ---------------------------------
#'
#' library(plspm)
#' data(satisfaction)
#'
#' # path matrix
#' IMAG = c(0,0,0,0,0,0)
#' EXPE = c(1,0,0,0,0,0)
#' QUAL = c(0,1,0,0,0,0)
#' VAL = c(0,1,1,0,0,0)
#' SAT = c(1,1,1,1,0,0)
#' LOY = c(1,0,0,0,1,0)
#' sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)
#'
#' # blocks of outer model
#' sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
#'
#' # vector of modes (reflective indicators)
#' sat_mod = rep("A", 6)
#'
#' # apply plspm
#' plsmodel = plspm(satisfaction, sat_path, sat_blocks, modes = sat_mod)
#'
#' sem_effects(plsmodel, target = "LOY", plot = TRUE, delete_zero_effect = TRUE,
#'             total_only = TRUE,
#'             total_color = RColorBrewer::brewer.pal(5,"Set3"))
#'
#' }
#'
#' @importFrom lavaan lavInspect
#' @importFrom piecewiseSEM coefs
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual geom_hline labs theme_bw position_dodge
#' @importFrom tidyr separate pivot_longer
#' @importFrom dplyr %>% filter select
#' @importFrom RColorBrewer brewer.pal
#' @import utils
#' @import checkmate
#' @import plspm
#' @export

sem_effects <- function(
    object,
    target,
    plot = TRUE,
    delete_zero_effect = TRUE,
    total_only = FALSE,
    total_color = "skyblue",
    color_palette = c("darkgreen", "skyblue", "orange")) {


  # Automatically load but not install missing dependencies:
  required_packages <- c("lavaan", "piecewiseSEM", "plspm", "ggplot2", "checkmate", "tidyr", "dplyr", "RColorBrewer")
  for (pkg in required_packages) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, warn.conflicts = FALSE)
    )
  }

  # Parameter validation:
  checkmate::assert_string(target)
  checkmate::assert_flag(plot)
  checkmate::assert_flag(total_only)
  checkmate::assert_flag(delete_zero_effect)

  if (inherits(object,"lavaan")){
# Case 1: lavaan ---------------------------------------------------------

    # Get normalized path coefficients:
    std <- lavaan::lavInspect(object = object, what = "std")
    beta <- std$beta
    n <- nrow(beta)

    # Verify the validity of the target:
    if (!target %in% rownames(beta)) {
      stop(paste("Target variable", target, "not found in model matrix:", paste(rownames(beta), collapse = ", ")))
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


  }else if(inherits(object,"psem")){
# Case 2: piecewiseSEM ----------------------------------------------------
    # checkmate::checkClass(pmod,"piecewiseSEM")  # class(pmod)
    # Must inherit from class 'piecewiseSEM', but has class 'psem'.

    pbeta <- piecewiseSEM::coefs(modelList = object)
    pbeta <- pbeta[, c("Response","Predictor","Std.Estimate")]

    all_nodes <- unique(c(as.character(pbeta$Response), as.character(pbeta$Predictor)))
    pbeta_matrix <- matrix(0,
                           nrow = length(all_nodes),
                           ncol = length(all_nodes),
                           dimnames = list(all_nodes, all_nodes))
    rows <- match(pbeta$Response, all_nodes)
    cols <- match(pbeta$Predictor, all_nodes)
    pbeta_matrix[cbind(rows, cols)] <- pbeta$Std.Estimate

    np <- nrow(pbeta_matrix)

    if (!target %in% rownames(pbeta_matrix)) {
      stop(paste("Target variable", target, "not found in model matrix:", paste(rownames(pbeta_matrix), collapse = ", ")))
    }

    direct_effect <- pbeta_matrix[target, ]

    indirect_effect_matrix <- matrix(0, nrow = np, ncol = np)
    for (k in 2:np) {
      indirect_effect_matrix <- indirect_effect_matrix + Reduce(`%*%`, replicate(k, pbeta_matrix, simplify = FALSE))
    }

    indirect_effect <- indirect_effect_matrix[target, ]
    total_effect <- direct_effect + indirect_effect

    # Build a result data frame:
    effect_results <- data.frame(
      Variable = colnames(pbeta_matrix),
      Direct_Effect = direct_effect,
      Indirect_Effect = indirect_effect,
      Total_Effect = total_effect
    )

    effect_filtered <- effect_results[effect_results$Variable != target, ]

    }else if(inherits(object,"plspm")){
# Case 3: plspm ----------------------------------------------------------

      pls.sum <- summary(object)
      #separate into from and to
      new_df <- pls.sum$effects %>%
        tidyr::separate(col = relationships,
                        into = c("from", "to"),
                        sep = "\\s*->\\s*",   # Match any whitespace and arrow
                        extra = "merge",      # Handling Exception Format
                        fill = "right") %>%
        dplyr::select(from, to, direct, indirect, total)

      if (!target %in% new_df$to) {
        stop(paste("Target variable", target, "not found in model matrix:", paste(unique(new_df$to), collapse = ", ")))
      }

      target_df <- new_df %>%
        dplyr::filter(to == target) %>%   # 精确匹配
        drop_na(to) %>%
        dplyr::select(from, direct, indirect, total)

      # Unified row and column names
      effect_results <- target_df %>%
        rename(
          Variable = from,
          Direct_Effect = direct,
          Indirect_Effect = indirect,
          Total_Effect = total
          )
      rownames(effect_results) <- effect_results$Variable

      effect_filtered <- effect_results[effect_results$Variable != target, ]

    }else{
# Case 4-----------------------------------------------------------------
      stop(paste("Unsupported object class:",
                 paste(class(object), collapse = "/"),
           "\n  Expected classes: lavaan, psem (piecewiseSEM), plspm"))
    }

  #--------------------------------------------------------------------------
  # Check the all-zero effects:
  all_zero_rows <- apply(effect_filtered[, c("Direct_Effect", "Indirect_Effect", "Total_Effect")], 1,   # 1 means row, 2 means column.
                         function(x)
                         all(abs(x) < 1e-10) && abs(sum(x)) < 1e-10)

  if (any(all_zero_rows)) {
    zero_vars <- effect_filtered$Variable[all_zero_rows]
    warning(
      "(1) The target variable'", target, "'might be incorrect due to variable(s) with all-zero effect(s): ", paste(zero_vars, collapse = ", "),
      ".\n  (2) The all-zero effect variable '",paste(zero_vars, collapse = ", "), "' is removed from the plot by default.",
      "\n  (3) If you want to keep the all-zero effect variable '",paste(zero_vars, collapse = ", "),"', please set the parameter delete_zero_effect to FALSE."
    )
  }

  if (delete_zero_effect){
    clean_data <- effect_filtered[!all_zero_rows, ]
  }else{
    clean_data = effect_filtered
    }

  # Long format data:
 effect_long <- tidyr::pivot_longer(clean_data,  cols = -Variable,
                                            names_to = "Effect_Type",  values_to = "Effect_Value" )


# ggplot:
  if (plot) {
     #Reset graphics device
    #while (!is.null(dev.list())) dev.off()
    #dev.new()

     if (total_only) {
# plot total effect only:
      effect_total <- effect_filtered %>%
            dplyr::select(Variable, Total_Effect)

      if (length(total_color) == 1) {
        colors <- rep(total_color, nrow(effect_total))
      } else {
        colors <- total_color[1:nrow(effect_total)]
      }
         p <- ggplot2::ggplot(effect_total, aes(x = Variable, y = Total_Effect, fill = Variable)) +
           ggplot2::geom_col(width = 0.5) +
           ggplot2::scale_fill_manual(values = colors) +
           ggplot2::geom_hline(yintercept = 0) +
           ggplot2::labs(x = NULL, y = "Total effects") +
           ggplot2::theme_bw()
    } else  {
# plot all 3 effects (direct, indirect, total):

    p <- ggplot2::ggplot(effect_long, aes(x = Variable, y = Effect_Value, fill = Effect_Type)) +
      ggplot2::geom_col(position = position_dodge(0.9), width = 0.8) +
      ggplot2::scale_fill_manual(values = color_palette) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::labs(x = NULL, y = "Standardized effects") +
      ggplot2::theme_bw()
      }

  }

  # Return:
  return(list(
    effect_table = effect_filtered,
    effect_long = effect_long,
    plot_object = if (exists("p")) p else NULL
  ))
}

