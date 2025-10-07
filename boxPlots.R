
#########################
# Fonction pour boxplot #
#########################

boxPlots <- function(data, vars = NULL, target, jitter = NULL, 
                     palette = "Dark2", alpha = 0.4,
                     title = NULL, subtitle = NULL, caption = NULL, tagLevel = NULL) {
  

  # Vérifier que target existe
  if (!(target %in% names(data))) {
    stop("La variable cible 'target' n'existe pas dans le jeu de données.")
  }
  
  # Vérifier que target est qualitative
  if (!inherits(data[[target]], c("factor", "character"))) {
    stop("La variable cible 'target' doit être de type factor ou character.")
  }
  
  # Convertir target en facteur
  data[[target]] <- as.factor(data[[target]])
  
  # Si vars est NULL -> prendre toutes les numériques
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
    message("Aucune variable spécifiée : toutes les variables numériques seront utilisées.")
  }
  
  # Vérifier qu’il reste bien des numériques
  if (length(vars) == 0) {
    stop("Aucune variable numérique trouvée dans 'vars'.")
  }
  
  # Liste pour stocker les plots
  plots <- list()
  
  
  # Boucle sur chaque variable
  for (var in vars) {
    
    if (!(var %in% names(data))) {
      warning(paste("Variable", var, "non trouvée. Passage à la suivante."))
      next
    }
    
    # Création du boxplot
    p <- ggboxplot(
      data, 
      x = target ,
      y = var,
      fill = target,
      add = jitter,
      alpha = alpha,
      palette = palette
    ) + theme_gray()
    
    plots[[var]] <- p
  }
  
  # Combiner avec patchwork + annotation globale
  combined <- wrap_plots(plots) +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption, 
      tag_levels = tagLevel
      
    )
  
  return(combined)
}


