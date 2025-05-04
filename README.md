# semEffect: an R package for structural equation model (SEM) effects analysis and visualization

[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/semEffect)](https://github.com/metacran/cranlogs.app)

This package 'semEffect' calculates direct, indirect, and total effects for a target variable in a SEM model. It supports visualization of effects through bar plots and includes automated dependency checks.

## 1. Install 'semEffect' package in R

- from Github:

	 install.packages("devtools")
	 devtools::install_github("PhDMeiwp/semEffect", dependencies = TRUE)

- from gitee:

	 install.packages("git2r")
	 install.packages("remotes")
	 library(git2r)
	 library(remotes)
	 remotes::install_git("https://gitee.com/openResearch/semEffect.git")  



## 2. Usage

	 sem_effects(
	  object,
	  target,
	  plot = TRUE,
	  total_only = FALSE,
	  total_colors = "skyblue",
	  color_palette = c("darkgreen", "skyblue", "orange")
	)

	
## 3. Examples

	 library(semEffect)
	 library(lavaan)
	 library(ggplot2)

	 model <- '
		# Measurement model
		ind60 =~ x1 + x2 + x3
		dem60 =~ y1 + y2 + y3 + y4
		dem65 =~ y5 + y6 + y7 + y8

		# Structural model
		dem60 ~ ind60
		dem65 ~ ind60 + dem60
		'
	
	 fit <- sem(model, data = PoliticalDemocracy)
	
	 # Effects analysis and visualization
	 sem_effects(fit, target = "dem65")

	 # Analyze effects for target variable "dem65"
	 results <- sem_effects(fit, target = "dem65")
	 print(results$effect_table)

	 # Customize plot appearance
	 results$plot_object +
	 ggplot2::theme_minimal() +
	 ggplot2::ggtitle("Standardized Effects for dem65")


## 4. Contact

- Bugs and feature requests can be filed to https://github.com/PhDMeiwp/semEffect/issues. 
- BTW, [Pull requests](https://github.com/PhDMeiwp/semEffect/pulls) are also welcome.