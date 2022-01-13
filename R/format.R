# ----------theme----------
gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

npg_color_hue <- function (n){
        return (npg_pal [1:n])
}

#' Define color scale
#'
#' @param color_fill whether to color (FALSE) or to fill the dots (TRUE) 
#' @param rever whether to reverse the legend order
#' @importFrom magrittr %>%
npg_colors <- function (color_fill=T, color_vec=NULL, rever=F, AP=NULL){
        if (is.null(color_vec)){ show_year <- F
        }else{
                color_vec %>% unique () %>% gtools::mixedsort () -> color_vec
                if ( mean ( color_vec %in% AP$col_vec ) == 1 ){
                        show_year <- T
                        new_color <- AP$col_vec [AP$col_vec %in% color_vec ]
                }else{show_year <- F}
        }
        if (!color_fill){
                if (show_year){
                        return (ggplot2::scale_color_manual (values=names (new_color), 
                                                             limits=new_color))
                }else{
                        return (ggsci::scale_color_npg (guide=ggplot2::guide_legend (reverse=rever) ) )
                }
        }else{
                if (show_year){
                        return (ggplot2::scale_fill_manual (values=names (new_color), 
                                                             limits=new_color  ))
                }else{
                        return (ggsci::scale_fill_npg (guide=guide_legend (reverse=rever) ) )
                }
        }
}

#' Obtain aesthetic settings
#' 
#' @description If the supplied list is NULL, then the default settings will be
#' returned. Otherwise, the named fields in the supplied list will replace the
#' corresponding field in the default settings
#'
#' @param aes_param a named list of aesthetic settings
#' @examples
#' # change the fontsize and font family
#' return_aes_param (list (fontsize=12, font_fam='Arial') )
#' @references TBdev package
#' @export
return_aes_param <- function (aes_param, load_format_conf=NULL){
        if (is.null (load_format_conf)) {data (config, package='deepCSMRI'); format_conf <- config
        }else{format_conf <- load_format_conf}
        if (is.null (aes_param)){
                aes_param <- format_conf
        }else{
                # if there are missing fields
                matched_names <- names (format_conf) %in% names (aes_param) 
                if ( mean (matched_names) != 1  ){
                        aes_param <- append (aes_param, format_conf [names (
                                              format_conf) [!matched_names] ] )
                }
        }
        return (aes_param)
}

#' @importFrom ggplot2 element_blank element_text element_rect
theme_dim_red <- function (aes_param=NULL){
        AP <- return_aes_param (aes_param)
        list (ggplot2::theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(),
              panel.border = element_rect(color='black', fill=NA),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text (hjust=0.5, size=AP$point_fontsize*3),
              legend.background= element_blank(),
              legend.key = element_blank(),
              text=element_text (size=AP$fontsize),
              aspect.ratio=1,
              legend.text = element_text (size=AP$fontsize),
              legend.title = element_text (size=AP$fontsize) ,
              legend.position='top',
              strip.background = element_blank()),
              ggsci::scale_color_npg (), ggsci::scale_fill_npg (),
              ggplot2::guides( fill= ggplot2::guide_legend(override.aes = 
                                list(size=AP$legend_point_size, alpha=1) 
              ))
        )
}

#' @importFrom ggplot2 element_blank element_text element_rect
theme_dotplot <- function (aes_param=NULL, rotation=0, ...){
        AP <- return_aes_param (aes_param)
        list (ggplot2::theme(
              # panel setting
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(),
              panel.border = element_rect(color='black', fill=NA),

              # axis setting
              # hjust = 0.95, make sure the labels are assigned to the bottom
              # of the x axis
              axis.text.x = element_text(angle=rotation, family=AP$font_fam,
                                         hjust=0.95, size=AP$fontsize),
              axis.text.y = element_text(family=AP$font_fam, size=AP$fontsize),
              axis.title.x = element_text(family=AP$font_fam, size=AP$fontsize),
              axis.title.y = element_text(family=AP$font_fam, size=AP$fontsize),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),

              # legend setting
              plot.title = element_text (hjust=0.5, size=AP$fontsize*1.5, 
                                         family=AP$font_fam, face='bold'),
              legend.background= element_blank (),
              legend.key = element_blank(),
              text=element_text (size=AP$fontsize, family=AP$font_fam),
              aspect.ratio=1,
              legend.text = element_text (size=AP$fontsize, family=AP$font_fam),
              legend.title = element_text (size=AP$fontsize, family=AP$font_fam) ,
              legend.position = 'top',

              # facet setting
              strip.background = element_blank (),
              strip.text = element_text (size=AP$point_fontsize*3, family=AP$font_fam, face='bold')
              ),

              #guides(color = guide_legend(override.aes = list(size=legend_point_size, alpha=1))),
              npg_colors (color_fill=T, AP=AP,...),
              npg_colors (color_fill=F, AP=AP,...)
              )
}

custom_round <- function (vec, num_out=3, more_precision=0, quantile_val=0,
                          round_updown=F, round_downup=F, no_rounding=F,
                          lower_b=NULL, upper_b=NULL){
        min_vec <- stats::quantile(vec, quantile_val, na.rm=T)
        max_vec <- stats::quantile(vec, 1-quantile_val, na.rm=T)
        if (!is.null(lower_b)) {min_vec <- pmax (min_vec, lower_b)}
        if (!is.null(upper_b)) {max_vec <- pmin (min_vec, upper_b)}
        nfig <- round (pmin (log10 (1/max_vec), 0, na.rm=T)) + more_precision
        if (!no_rounding){
                if (round_updown){
                        min_vec <- floor (min_vec*10^(nfig))/10^(nfig)
                        max_vec <- ceiling (max_vec*10^(nfig))/10^(nfig)
                }else{
                        min_vec <- round (min_vec*10^(nfig))/10^(nfig)
                        max_vec <- round (max_vec*10^(nfig))/10^(nfig)
                }
                if (round_downup){
                        min_vec <- ceiling (min_vec*10^(nfig))/10^(nfig)
                        max_vec <- floor (max_vec*10^(nfig))/10^(nfig)
                }
        }
        return (seq (min_vec, max_vec, length.out=num_out))
}

custom_labeller <- function (vec, num_out=3, min_prec=0){
        min_vec <- min (vec, na.rm=T)
        max_vec <- max (vec, na.rm=T)
        nfig <- min_prec
        seq_vec <- seq (min_vec, max_vec, length.out=num_out)
        return_vec <- round (seq_vec, nfig)
        for (i in 1:5){
                if (return_vec[1]==return_vec[length(return_vec)]){
                        nfig <- nfig + 1
                        return_vec <- round (seq_vec, nfig)
                }else{break}
        }
        return (format (return_vec, nsmall=nfig))
}

#' Make the x or y axis only show the min, middle and max points
#'
#' @export
custom_tick <- function (vec=NULL, x_y='y', more_prec=3, min_prec=1, num_out=3, limits=NULL,...){
        breaking <- function (x){custom_round (x, more_precision=more_prec, 
                                               num_out=num_out, no_rounding=T,...)}
        labelling <- function (x){custom_labeller (x, min_prec=min_prec, num_out=num_out)}
        if (x_y == 'y'){return (ggplot2::scale_y_continuous (breaks = breaking, labels=labelling, limits=limits) )
        }else {return (ggplot2::scale_x_continuous (breaks = breaking, labels=labelling, limits=limits) )}
}
