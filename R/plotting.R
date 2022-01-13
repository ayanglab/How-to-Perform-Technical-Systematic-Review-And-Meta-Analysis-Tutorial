
#' Same function as `grid.arrange`, with the functionality of integrating
#' ComplexHeatmap and adding panel labels
#' 
#' @param grob_list a list of plots. They can be ggplots, ComplexHeatmap,
#' data.frame, character, image array
#' @param save_path where the plots are saved
#' @param grid_layout, same as the argument in `grid.arrange`
#' @param margin_width the width of the margin in inches. The default is 0.79in
#' (2cm). Same unit in `margin_height`, `page_width`, `page_height`
#' @param plot_width the width of each subplot. 
#' @param plot_height same concept as plot_width
#' @importFrom grid pushViewport viewport grid.layout grid.rect grid.draw
#' popViewport grid.text grid.grabExpr 
#' @references TBdev package
#' @export
arrange_plots <- function (grob_list, save_path, grid_layout, panel_label=NULL,
                           margin_width=1, margin_height=1,
                           panel_spacing=0.05, plot_width=9, plot_height=9, AP=NULL){
        AP <- return_aes_param (AP)
        # calculate the width and height for all the plots
        page_width <- ncol (grid_layout)*plot_width
        page_height <- nrow(grid_layout)*plot_height
        # calculate the margin for the entire page
        width_pro <- 1 - margin_width/page_width
        height_pro <- 1 - margin_height/page_height

        # default panel labels: A, B, C, D ... 
        if (is.null (panel_label)){ panel_label <- letters[1:length (grob_list)]}
        grDevices::cairo_pdf (save_path, width=page_width, height=page_height)
        grid::grid.newpage ()
        pushViewport(viewport(layout=grid.layout(nrow (grid_layout), ncol(grid_layout)) ,
                              width=width_pro, height=height_pro))

        # start plotting
        for (i in 1:length (grob_list)){
                print (paste ('arranging figure', i))
                row_pos <- unique (which (grid_layout == i, arr.ind=T) [, 'row'])
                col_pos <- unique (which (grid_layout == i, arr.ind=T) [, 'col'])

                # calculate the plotting dimensions
                # calculate the margin for each subplot
                subplot_width <- length (col_pos)*plot_width
                subplot_height <- length (row_pos)*plot_height
                width_pro_panel <- 1 - 2*panel_spacing/subplot_width
                height_pro_panel <- 1 - 2*panel_spacing/subplot_height

                pushViewport (viewport (layout.pos.col=col_pos,layout.pos.row=row_pos))
                # in order to increase spacing between panels, it is necessary
                # to draw an empty rectangle
                grid.rect(gp=grid::gpar(lty=0)) #lty=0 means blank line
                pushViewport (viewport (layout.pos.col=col_pos,layout.pos.row=row_pos, 
                                        width=width_pro_panel, height=height_pro_panel))

                # need different functions for ggplot and ComplexHeatmap
                # objects
                if ( 'ggplot' %in% class (grob_list[[i]] ) ){
                        grid.draw (grid.grabExpr ( print (grob_list[[i]])))
                }else if ( 'pheatmap' %in% class (grob_list[[i]] ) ){ 
                        #plotify <- ggplotify::as.grob (grob_list [[i]])
                        grid.draw(grob_list[[i]]$gtable)
                }else if ('data.frame' %in% class (grob_list[[i]] ) ) {
                        colnames (grob_list [[i]] ) <- gsub ('\\.', ' ', colnames (grob_list [[i]]))
                        mytheme <- gridExtra::ttheme_default(padding=unit(c(4, 8), "mm"), 
                                                             base_size=AP$fontsize, base_family='sans' )
                        gridExtra::grid.table(grob_list [[i]], rows=NULL, theme=mytheme)
                }else{ ComplexHeatmap::draw (grob_list[[i]], newpage=F) }
                grid.text (panel_label[i], x=unit (0.01, 'npc'), y=unit (0.95, 'npc'), 
                           gp=grid::gpar (fontsize=AP$high_font, fontfamily=AP$font_fam, fontface='bold'))
                popViewport (2)
        }
        dev.off ()
}

get_arrow <- function (){
        ggplot2::arrow(angle=30, length = grid::unit( 0.01, 'npc'), type= 'closed')
}

#' @importFrom ggplot2 aes aes_string
#' @importFrom magrittr %>%
arrow_axis <- function (plot_ob, length_ratio=0.05, nudge_ratio=0.2, move_x=0,
                        move_y=0, AP=NULL){
        AP <- return_aes_param (AP)
        plot_build <- ggplot2::ggplot_build (plot_ob)
        origin_x <- plot_build$layout$panel_scales_x[[1]]$range$range[1]
        end_x <- plot_build$layout$panel_scales_x[[1]]$range$range[2]
        origin_y <- plot_build$layout$panel_scales_y[[1]]$range$range[1]
        end_y <- plot_build$layout$panel_scales_y[[1]]$range$range[2]
        x <- gsub ('_', ' ', plot_build$plot$labels$x)
        y <- gsub ('_', ' ', plot_build$plot$labels$y)

        dist_x <- length_ratio*(end_x - origin_x)
        dist_y <- length_ratio*(end_y - origin_y)
        dist_xy <- max (dist_x, dist_y)

        df_arrow <- data.frame (x1=c(origin_x, origin_x), 
                                  y1=c(origin_y, origin_y), 
                                  x2=c(origin_x+dist_x, origin_x), 
                                  y2=c(origin_y, origin_y+dist_y))

        df_arrow$x1 <- df_arrow$x1 - dist_x*move_x
        df_arrow$x2 <- df_arrow$x2 - dist_x*move_x
        df_arrow$y1 <- df_arrow$y1 - dist_y*move_y
        df_arrow$y2 <- df_arrow$y2 - dist_y*move_y
        df_arrow$axis_labels <- c(x, y)
        df_arrow %>% 
                dplyr::mutate (xlabel = x2 + 0.2*dist_x*c(1, 0)  ) %>%
                dplyr::mutate (ylabel = y2 + 0.2*dist_y*c(0, 1)  ) -> df_arrow

        return (list(
                # type = 'closed' produces a solid triangle at the arrow end
                # linejoin = 'mitre' produces triangle with sharp edges
                ggplot2::geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), data = df_arrow, 
                            arrow = get_arrow (), size = 1.5, 
                            linejoin='mitre'),

                # x axis
                ggplot2::geom_text (aes (x=xlabel, y=ylabel, label=axis_labels), data =
                           df_arrow[1,], nudge_y=0, nudge_x=nudge_ratio*dist_x, 
                           size=AP$point_fontsize, hjust='left', vjust=0.5,
                           fontface='italic', angle=0, family=AP$font_fam),

                # y axis
                ggplot2::geom_text (aes (x=xlabel, y=ylabel, label=axis_labels), data =
                           df_arrow [2,], nudge_y=nudge_ratio*dist_y, nudge_x=0, 
                           size=AP$point_fontsize, vjust=0.5, hjust='bottom',
                           fontface='italic', angle=90, family=AP$font_fam)
        ))
}
