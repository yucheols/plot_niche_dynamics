# ============================================================================================================
# FUNCTION ::: reproduce ecospat niche dynamics plot using ggplot2
#
# Package dependencies: ecospat, terra, dplyr, ggplot2, factoextra
#
# This function was tested in R v4.6.0
#
#
# Key inputs
#   1) z1: native range niche dynamic; can be created by running "ecospat.grid.clim.dyn()" in ecospat
#   2) z2: introduced range niche dynamic; can be created by running "ecospat.grid.clim.dyn()" in ecospat
#   3) pca_object: output of PCA-env (sensu Broennimann et al. 2012)
#                  can be created by running "raster.pca()" in ENMTools
#
#
# plot width and height unit is in inches
#
# ============================================================================================================


# the function starts here
plot_niche_dynamics <- function(
    z1,
    z2,
    pca_object,
    z1_name = 'Europe',
    z2_name = 'North America',
    intersection = 0.05,
    density_min = 0.02,
    plot_padding = 0.10,
    save_path = NULL,
    width = 7.5,
    height = 7,
    dpi = 800
) {
  
  # ----------------------------------------------------------
  # niche dynamics
  #
  # z1 = native range
  # z2 = introduced range
  # ----------------------------------------------------------
  
  niche_dyn <- ecospat::ecospat.niche.dyn.index(z1 = z1, z2 = z2, intersection = intersection)
  print(niche_dyn$dynamic.index.w)
  print(niche_dyn$category_quantity)
  
  
  # ----------------------------------------------------------
  # extract rasters
  # ----------------------------------------------------------
  
  # niche-dynamics categories
  dyn_rast <- niche_dyn$dyn
  
  # uncorrected occurrence density
  # used for density shading
  z1_density <- z1$z.uncor
  z2_density <- z2$z.uncor
  
  # environmental density
  # used for environmental contours
  z1_env <- z1$Z
  z2_env <- z2$Z
  
  
  # ----------------------------------------------------------
  # combine rasters
  # ----------------------------------------------------------
  
  niche_rast <- c(dyn_rast, z1_density, z2_density, z1_env, z2_env)
  names(niche_rast) <- c('dynamic', 'z1_density', 'z2_density', 'z1_env', 'z2_env')
  
  
  # ----------------------------------------------------------
  # convert to dataframe
  # ----------------------------------------------------------
  
  niche_df <- terra::as.data.frame(niche_rast, xy = T, na.rm = F)
  
  
  # ----------------------------------------------------------
  # normalize occurrence densities independently to 0-1
  # ----------------------------------------------------------
  
  niche_df <- niche_df %>%
    dplyr::mutate(z1_density = z1_density / max(z1_density, na.rm = T),
                  z2_density = z2_density / max(z2_density, na.rm = T))
  
  
  # ----------------------------------------------------------
  # label niche-dynamics categories
  # ----------------------------------------------------------
  
  niche_df <- niche_df %>%
    dplyr::mutate(dynamics = dplyr::case_when(dynamic == 1 ~ 'Abandonment',
                                              dynamic == 2 ~ 'Unfilling',
                                              dynamic == 3 ~ 'Stability',
                                              dynamic == 4 ~ 'Expansion',
                                              dynamic == 5 ~ 'Pioneering',
                                              dynamic == 6 ~ 'Non-analog',
                                              T ~ NA_character_),
                  dynamics = factor(dynamics, levels = c('Stability',
                                                         'Expansion',
                                                         'Unfilling',
                                                         'Pioneering',
                                                         'Abandonment',
                                                         'Non-analog')))
  
  
  # ----------------------------------------------------------
  # PCA axis labels
  # ----------------------------------------------------------
  
  pca_eig <- factoextra::get_eigenvalue(pca_object)
  pc1_lab <- paste0('PC1 (', round(pca_eig[1, 'variance.percent'], 1), '%)')
  pc2_lab <- paste0('PC2 (', round(pca_eig[2, 'variance.percent'], 1), '%)')
  
  
  # ----------------------------------------------------------
  # environmental contour thresholds
  #
  # solid  = complete environmental extent
  # dashed = environmental extent after excluding marginal
  #          environments according to intersection
  # ----------------------------------------------------------
  
  z1_env_positive <- niche_df$z1_env[!is.na(niche_df$z1_env) & niche_df$z1_env > 0]
  z2_env_positive <- niche_df$z2_env[!is.na(niche_df$z2_env) & niche_df$z2_env > 0]
  
  # outer environmental extent
  z1_env_outer <- unname(quantile(z1_env_positive, probs = 0))
  z2_env_outer <- unname(quantile(z2_env_positive, probs = 0))
  
  # inner environmental extent
  z1_env_inner <- unname(quantile(z1_env_positive, probs = intersection))
  z2_env_inner <- unname(quantile(z2_env_positive, probs = intersection))
  
  
  # ----------------------------------------------------------
  # plotting extent
  # ----------------------------------------------------------
  
  plot_extent <- niche_df %>%dplyr::filter(!is.na(dynamics))
  
  x_range <- range(plot_extent$x, na.rm = T)
  y_range <- range(plot_extent$y, na.rm = T)
  
  x_pad <- diff(x_range) * plot_padding
  y_pad <- diff(y_range) * plot_padding
  
  x_limits <- c(x_range[1] - x_pad, x_range[2] + x_pad)
  y_limits <- c(y_range[1] - y_pad, y_range[2] + y_pad)
  
  
  # ----------------------------------------------------------
  # occurrence-density shading data
  # ----------------------------------------------------------
  
  z1_density_df <- niche_df %>% dplyr::filter(!is.na(z1_density), z1_density >= density_min)
  z2_density_df <- niche_df %>% dplyr::filter(!is.na(z2_density), z2_density >= density_min)
  
  
  # ----------------------------------------------------------
  # colors
  # ----------------------------------------------------------
  
  # occurrence-density shading
  z1_shade_col <- '#7E97C9'
  z2_shade_col <- '#C9A46A'
  
  # environmental contours
  z1_line_col <- 'cornflowerblue'
  z2_line_col <- 'goldenrod2'
  
  
  # ----------------------------------------------------------
  # plot
  # ----------------------------------------------------------
  
  niche_plot <- ggplot2::ggplot(niche_df, ggplot2::aes(x = x, y = y)) +
    
    # niche-dynamics categories
    ggplot2::geom_raster(data = niche_df %>% dplyr::filter(!is.na(dynamics)), ggplot2::aes(fill = dynamics), alpha = 1) +
    
    
    # z1 occurrence-density shading
    ggplot2::geom_raster(data = z1_density_df, ggplot2::aes(alpha = z1_density), fill = z1_shade_col) +
    
    
    # z2 occurrence-density shading
    ggplot2::geom_raster(data = z2_density_df, ggplot2::aes(alpha = z2_density), fill = z2_shade_col) +
    
    
  # --------------------------------------------------------
  # z1 environmental contours
  # --------------------------------------------------------
  
  # complete environmental extent
  ggplot2::geom_contour(data = niche_df, ggplot2::aes(z = z1_env, colour = z1_name), 
                        breaks = z1_env_outer, linetype = 'solid', linewidth = 0.8) +
    
    # environmental extent excluding margins
    ggplot2::geom_contour(data = niche_df, ggplot2::aes(z = z1_env,colour = z1_name),
                          breaks = z1_env_inner, linetype = 'dashed', linewidth = 0.8) +
    
    
  # --------------------------------------------------------
  # z2 environmental contours
  # --------------------------------------------------------
  
  # complete environmental extent
  ggplot2::geom_contour(data = niche_df, ggplot2::aes(z = z2_env, colour = z2_name), 
                        breaks = z2_env_outer, linetype = 'solid', linewidth = 0.8) +
    
    # environmental extent excluding margins
    ggplot2::geom_contour(data = niche_df, ggplot2::aes(z = z2_env, colour = z2_name), 
                          breaks = z2_env_inner, linetype = 'dashed', linewidth = 0.8) +
    
    
  # --------------------------------------------------------
  # zero reference lines
  # --------------------------------------------------------
  
  ggplot2::geom_vline(xintercept = 0, linetype = 'dashed', linewidth = 0.5, colour = 'black') +
    ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5, colour = 'black') +
    
    
  # --------------------------------------------------------
  # niche-dynamics colors
  # --------------------------------------------------------
  
  ggplot2::scale_fill_manual(
    values = c(
      'Stability' = '#C7B9E8',
      'Expansion' = '#EFC1A2',
      'Unfilling' = '#B2D8D8',
      'Pioneering' = '#F4B6C2',
      'Abandonment' = '#C1D8B5',
      'Non-analog' = '#D9D9D9'
    ),
    breaks = c(
      'Stability',
      'Expansion',
      'Unfilling',
      'Pioneering',
      'Abandonment',
      'Non-analog'
    ),
    drop = T
  ) +
    
    
  # --------------------------------------------------------
  # environmental contour colors
  # --------------------------------------------------------
  
  ggplot2::scale_colour_manual(values = stats::setNames(c(z1_line_col, z2_line_col), c(z1_name, z2_name))) +
    
    
  # --------------------------------------------------------
  # occurrence-density transparency
  # --------------------------------------------------------
  
  ggplot2::scale_alpha_continuous(range = c(0.06, 0.45), limits = c(0, 1), guide = 'none') +
    
    
  # --------------------------------------------------------
  # plotting window
  # --------------------------------------------------------
  
  ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, expand = F) +
    
    
  # --------------------------------------------------------
  # labels
  # --------------------------------------------------------
  
  ggplot2::labs(x = pc1_lab, y = pc2_lab, fill = 'Niche dynamics', colour = 'Environmental extent') +
    
    
  # --------------------------------------------------------
  # theme
  # --------------------------------------------------------
  
  ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(aspect.ratio = 1, 
                   axis.title = ggplot2::element_text(face = 'bold', size = 15),
                   axis.text = ggplot2::element_text(colour = 'black', size = 12),
                   legend.position = 'bottom',
                   legend.box = 'vertical',
                   legend.title = ggplot2::element_text(face = 'bold'),
                   legend.key.width = grid::unit(1.2, 'cm')) +
    
    
  # --------------------------------------------------------
  # legends
  # --------------------------------------------------------
  
  ggplot2::guides(fill = ggplot2::guide_legend(order = 1, nrow = 2, byrow = T, override.aes = list(alpha = 1)),
                  colour = ggplot2::guide_legend(order = 2, nrow = 1, override.aes = list(linewidth = 1)))
  
  
  # ----------------------------------------------------------
  # save figure if path is supplied
  # ----------------------------------------------------------
  
  if (!is.null(save_path)) {
    
    dir.create(dirname(save_path), recursive = T, showWarnings = F)
    ggplot2::ggsave(filename = save_path, plot = niche_plot, width = width, height = height,
                    units = 'in', dpi = dpi, bg = 'white')
  }
  
  
  # ----------------------------------------------------------
  # return results
  # ----------------------------------------------------------
  
  return(list(plot = niche_plot, niche_dyn = niche_dyn,
              plot_data = niche_df,
              contour_thresholds = c(z1_outer = z1_env_outer,
                                     z1_inner = z1_env_inner, 
                                     z2_outer = z2_env_outer, 
                                     z2_inner = z2_env_inner),
              plot_limits = list(x = x_limits, y = y_limits)))
}


#### example
#niche_result <- plot_niche_dynamics(z1 = eu_esp, z2 = na_esp,pca_object = glob_env_pca$pca.object,
#                                    z1_name = 'Europe', z2_name = 'North America', intersection = 0.05,
#                                    save_path = 'path/to/plot/output/filename.png')

#print(niche_result$plot)
