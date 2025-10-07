



#################################
# Fonction pour nuage de points #
#################################


# Ecriture fonction 

scatterPlots <- function(data, vars = NULL, target = NULL,
                         useTarget = TRUE,
                         add = NULL, # "reg.line", "loess", NULL
                         palette = "Dark2", alpha = 0.6, size = 2,
                         title = NULL, subtitle = NULL, caption = NULL, tagLevel = NULL,
                         method = "pearson", color="black") {
  
  
  
  # Sélection des variables numériques si vars = NULL
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
    message("Aucune variable spécifiée : toutes les variables numériques seront utilisées.")
  }
  
  if (length(vars) < 2) {
    stop("Il faut au moins deux variables numériques pour faire des scatter plots.")
  }
  
  # Vérification de la target si useTarget = TRUE
  if (useTarget) {
    if (is.null(target)) stop("Vous devez fournir une target si useTarget = TRUE.")
    if (!(target %in% names(data))) stop("La variable cible 'target' n'existe pas.")
    if (!inherits(data[[target]], c("factor", "character"))) {
      stop("La variable cible 'target' doit être de type facteur ou caractère.")
    }
  }
  
  # Génération des combinaisons de paires
  combs <- combn(vars, 2, simplify = FALSE)
  plots <- list()
  
  for (pair in combs) {
    x_var <- pair[1]
    y_var <- pair[2]
    
    if (!(x_var %in% names(data)) || !(y_var %in% names(data))) {
      warning(paste("Variables", x_var, "ou", y_var, "non trouvées."))
      next
    }
    
    # Calcul corrélation
    cor_val <- cor(data[[x_var]], data[[y_var]], use = "complete.obs", method = method)
    cor_label <- paste0("r = ", round(cor_val, 2))
    
    # Plot avec ou sans target
    if (useTarget) {
      p <- ggscatter(
        data, x = x_var, y = y_var,
        color = target, alpha = alpha, size = size,
        palette = get_palette(palette, 10),
        add = add
      ) +
        annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
                 label = cor_label, size = 4, fontface = "bold", colour = color) +
        theme_gray()
      
    } else {
      p <- ggscatter(
        data, x = x_var, y = y_var,
        alpha = alpha, size = size,
        add = add
      ) +
        annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
                 label = cor_label, size = 6, fontface = "bold", colour = color) +
        theme_gray()
    }
    
    plots[[paste(x_var, y_var, sep = "_vs_")]] <- p
  }
  
  # Combinaison finale
  combined <- wrap_plots(plots) +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption, 
      tag_levels = tagLevel
    )
  
  return(combined)
}





