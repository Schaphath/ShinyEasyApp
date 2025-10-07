




#############################
# Fonction pour Histogramme #
#############################

histPlots <- function(data, vars = NULL, target,
                      bins = 30, add_line = "mean",
                      palette = "Dark2", alpha = 0.4,
                      title = NULL, subtitle = NULL, caption = NULL, tagLevel = NULL) {
  

  # Vérifier que la cible existe
  if (!(target %in% names(data))) {
    stop("La variable cible 'target' n'existe pas dans le jeu de données.")
  }
  
  # Vérifier que la cible est qualitative
  if (!inherits(data[[target]], c("factor", "character"))) {
    stop("La variable cible 'target' doit être factor ou character.")
  }
  
  # Si vars est NULL → prendre toutes les numériques
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
    message("Aucune variable spécifiée : toutes les variables numériques seront utilisées.")
  }
  
  # Vérifier qu’il reste des numériques
  if (length(vars) == 0) {
    stop("Aucune variable numérique trouvée dans 'vars'.")
  }
  
  # Liste des plots
  plots <- list()
  
  # Boucle sur les variables
  for (var in vars) {
    
    if (!(var %in% names(data))) {
      warning(paste("Variable", var, "non trouvée. Passage à la suivante."))
      next
    }
    
    # Création de l’histogramme
    p <- gghistogram(
      data, x = var, fill = target,
      add = add_line, bins = bins, ylab = "Effectif",
      alpha = alpha, palette = get_palette(palette, 5)
    ) + theme_gray()
    
    plots[[var]] <- p
  }
  
  # Combiner avec patchwork
  combined <- wrap_plots(plots) +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption, 
      tag_levels = tagLevel
    )
  
  return(combined)
}


