#' Phd thesis theme for ggplot2
#'
#' @param ... Named argument to modify the theme
#'
#' @export
theme_mythesis <- function(...) {
  ggplot2::theme_minimal(base_family = 'Ubuntu Mono', 
                         base_size = 14) +
    ggplot2::theme(plot.title = element_text(face = 'bold', 
                                             size = 16, 
                                             colour = '#ffffff'),
                   #panel.grid.major = element_line(linetype = 'longdash'),
                   #panel.grid.minor = element_line(linetype = 'longdash'),
                   #strip.background = element_rect(fill = '#357935',
                   #                                colour = '#357935'),
                   strip.text = element_text(color = '#ffffff'), ...)
}

#' Phd thesis color palettes
#'
#' @param palette the palette to be used, options are \code{c('main', 'secondary', 'grey')}
#' @param reverse boolean, represents the order of the palette
#' @param ... Named argument to modify the palette
#'
#' @export
mythesis_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- list(main = wes_palettes[["Darjeeling1"]], secondary = wes_palettes[["GrandBudapest1"]], grey = c("#CCCCCC", "#8C8C8C"))[[palette]] # mythesisthemes::colors[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


#' Phd thesis scale colors for ggplot2
#'
#' @param palette the palette to be used, options are \code{c('main', 'secondary', 'grey')}
#' @param discrete boolean, the scale is discrete? \code{TRUE} if it is.
#' @param reverse boolean, represents the order of the palette
#' @param ... Named argument to modify the \code{scale_color_} fucntion
#'
#' @export
scale_color_mythesis <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- mythesis_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("mythesis_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Phd thesis scale colors for ggplot2
#'
#' @param palette the palette to be used, options are \code{c('main', 'secondary', 'grey')}
#' @param discrete boolean, the scale is discrete? \code{TRUE} if it is.
#' @param reverse boolean, represents the order of the palette
#' @param ... Named argument to modify the \code{scale_color_} fucntion
#'
#' @export
scale_fill_mythesis <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- mythesis_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("mythesis_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
