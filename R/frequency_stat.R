# ----------Summarise discrete variables----------

#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes_string
ggbar_1var <- function (x, feature, color, AP, freq='Freq', show_num=8,
                        extend_ratio=1.5, scale_factor=NULL){
        x %>% dplyr::group_by (!!as.symbol (feature)) %>% 
                dplyr::summarise (sum_freq = sum (!!as.symbol (freq))) %>%
                dplyr::arrange (dplyr::desc (sum_freq) ) -> sum_x

        sum_x %>% dplyr::select (!!as.symbol (feature)) %>%
                tibble::deframe () -> order_study
        freq_vec <- sum_x$sum_freq/sum (sum_x$sum_freq)
        if (!is.null (scale_factor)){freq_vec <- freq_vec*sum(sum_x$sum_freq)/scale_factor}
        sum_x$perc_freq <- format (round (freq_vec*100, 2), nsmall=2)
        sum_x %>% dplyr::mutate (label_freq = paste (sum_freq, 
                        ' (', perc_freq, '%)', sep='') ) -> sum_x

        x [, color] %>% unique () %>% gtools::mixedsort () %>% rev () -> order_color
        x %>% dplyr::arrange (dplyr::desc (freq) ) %>% 
              dplyr::filter ( !!as.symbol (feature) %in% order_study[1:show_num] ) %>%
              dplyr::mutate ( Feature =  factor (!!as.symbol (feature), 
                                          levels= rev(order_study) )) %>%
              dplyr::mutate ( Color = factor (!!as.symbol (color), 
                                              levels=order_color) ) -> plot_data

        min_x <- min (plot_data [, freq])
        max_x <- max (plot_data [, freq])*(1. + extend_ratio)
        plot_data %>% dplyr::filter (!(!!as.symbol (freq) %in% c(0, NA) )) -> plot_data
        ggplot2::ggplot (plot_data, aes_string (y='Feature', x=freq, fill='Color') ) +
                ggplot2::geom_bar (stat='identity', color='black', alpha=AP$transparency)+
                ggplot2::geom_text (aes_string (y=feature, x='sum_freq', label= 'label_freq'), 
                           data=sum_x [1:show_num,], inherit.aes=F, hjust=0, 
                           size=AP$point_fontsize) +
                theme_dotplot (color_vec = plot_data [, 'Color']) + xlim (c(min_x, max_x))
}

#' Flatten each list in a dataframe
#'
#' @description Dataframes may contain such a column that contains strings, in
#' which items are joined by some delimiter. This makes it difficult to perform
#' analysis in individual items in the strings. This function creates a new
#' vector with each element containing one item in the strings. Each ittem has
#' an ID label from the original dataframe.
#' @param x_df a dataframe
#' @param feature which column to extract the features
#' @param ID_col which column becomes the ID for the new features
#' @return a named vector
#' @importFrom magrittr %>%
resolve_overlap <- function (x_df, feature, ID_col){
        x_df %>% dplyr::select (dplyr::all_of (feature) ) %>%
                lapply (function (x){strsplit (as.character(x), ',')})  -> split_r

        names (split_r [[feature]]) <- paste (x_df [, ID_col], '_', sep='')
        split_r <- unlist (split_r [[feature]]) 
        names (split_r) <- gsub ('_[0-9]*$', '', names (split_r))
        split_r %>% gsub (pattern='\\(.+\\)$', replacement='') %>% trimws ()
}

#' @export
barplot_1var <- function (xx, var1, var2, AP=NULL,...){
        AP <- return_aes_param (AP)
        inp_df <- table (xx [, var1], xx [, var2]) %>% data.frame ()
        ggbar_1var (inp_df, 'Var1', 'Var2', AP=AP, ...) + 
                ggplot2::ylab (recode_features (var1)) + 
                ggplot2::labs (fill=recode_features (var2) ) + 
                ggplot2::xlab ('Publication number')
}

#' Barplot of each study that contains multiple features
#'
#' @param xx a dataframe
#' @param var1 the variable for the y axis
#' @param var2 the variable to fill the color
#' @param ... arguments for `ggbar_1var`
#' @return a ggplot bar plot orientated horizontally
#' @importFrom magrittr %>%
#' @export
barplot_1var_overlap <- function (xx, var1, var2, ID_col=NULL, ...){
        if (is.null (ID_col)){ID_col <- var2}
        plot_vec <- resolve_overlap (xx, var1, ID_col)
        plot_df <- data.frame ('feature'=plot_vec, 'color'=names (plot_vec) )
        plot_df %>% dplyr::group_by (color) %>% dplyr::distinct (feature ) %>% 
                dplyr::ungroup () %>% as.data.frame () -> plot_df
        plot_df$color <- xx [match (plot_df$color, xx [, ID_col]), var2]
        num_study <- nrow (xx)
        plot_df$feature <- recode_features (plot_df$feature)
        barplot_1var (plot_df, 'feature', 'color', scale_factor=num_study, ...) + 
                ggplot2::ylab (recode_features (var1)) + 
                ggplot2::labs (fill=recode_features (var2))
}

#' @export
overlap_count <- function (dat, var1, ID_col){
        var_freq <- resolve_overlap (dat, var1, ID_col) 
        names (var_freq) %>% table () %>% data.frame () %>% 
                magrittr::set_colnames (c('ID', 'freq')) -> var_count
        return (var_count$freq [match (dat [, ID_col], var_count$ID) ])
}

#' @importFrom ggplot2 aes_string
#' @export
overlap_count_plot <- function (dat, var1, var2, ID_col, bw=0.1, AP=NULL){
        AP <- return_aes_param (AP)
        dat [, var2] <- factor (dat [, var2], levels= rev (sort (unique (dat [, var2]) ) ) )
        correlate <- stats::cor ( as.numeric (dat [, var2]), as.numeric (dat [, var1]) )
        ggplot2::ggplot (dat, aes_string (x=var1)) + 
                ggplot2::geom_histogram (aes_string (fill=var2, group=var2),
                                binwidth=bw, color='black',
                                alpha=AP$transparency) +
                #geom_density (aes (y=bw*..count..), color='red', show.legend=F) + 
                ggplot2::annotate ('text', label=paste ('Correlation =', round (correlate, 3) ), 
                          x=-Inf, y=Inf, vjust=1, hjust=-0.1, size=AP$point_fontsize)+
                theme_dotplot (aes_param=AP, color_vec = dat [, var2])+ 
                ggplot2::theme (legend.position='top') +
                ggplot2::labs (fill = recode_features (var2))
}

#' @importFrom ggplot2 aes_string
#' @export
barplot_2var <- function (dat, var1, var2, AP=NULL, threshold=10, extend_ratio=0.){
        AP <- return_aes_param (AP)
        var1_2 <- data.frame (table (dat[, var1], dat[, var2]))
        colnames (var1_2) <- c(var1, var2, 'freq')
        var1_2 %>% dplyr::group_by (!!as.symbol (var2)) %>% 
                dplyr::summarise (sum_freq=sum(freq)) %>%
                dplyr::slice_max (sum_freq, n=threshold) %>% 
                dplyr::select (dplyr::all_of (var2) ) %>%
                tibble::deframe () -> show_names

        var1_2 [, var2] <- as.character (var1_2 [, var2])
        var1_2 [! var1_2 [, var2] %in% show_names, var2] <- 'Others'
        color_vec <- color_name_with_others (unique (var1_2 [, var2]), AP)

        freq_var <- var1_2 %>% dplyr::group_by (!!as.symbol (var1)) %>% 
                dplyr::summarise (sum_freq = sum (freq))
        max_x <- max (freq_var [, 'sum_freq'])*(1. + extend_ratio)
        ggplot2::ggplot (var1_2, aes_string (x=var1, y='freq'))+
                ggplot2::geom_bar (aes_string(fill=var2), stat='identity', 
                                   color='black', alpha=AP$transparency) +
                theme_dotplot (aes_param=AP, color_vec = var1_2 [, var2]) + 
                ggplot2::ylim (c(0, max_x)) +
                ggplot2::scale_fill_manual (values=color_vec, breaks=names (color_vec))+
                ggplot2::xlab (recode_features (var1) ) + 
                ggplot2::labs (fill=recode_features (var2))
}

#' Barplot with percentage labels
#'
#' @param dat a dataframe
#' @param var1 the x axis
#' @param var2 the y axis
#' @return a barplot with the labels of the percentage of the first category
#' over each bar, as well as the correlation of this percentage with `var1`
#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes
#' @export
barplot_2var_label <- function (dat, var1, var2, AP=NULL, ...){
        AP <- return_aes_param (AP)
        dat [, var1] %>% table () %>% data.frame () -> label_text
        colnames (label_text) <- c(var1, 'freq')
        table (dat [, var1], dat [, var2]) -> perc
        perc <- perc/rowSums (perc)
        cor_perc <- stats::cor (as.numeric (label_text [, var1]), perc [,1])
        label_text$perc <- paste (format (round (100*perc [,1], 2), nsmall=2), '%', sep='')

        barplot_2var (dat, var1, var2, AP=AP,...) +
                ggplot2::geom_text (aes (label=perc), data=label_text, 
                                    size=AP$point_fontsize, vjust=-0.3)+
                # put the correlation label in the top left corner
                ggplot2::annotate ('text', label=paste ('Correlation =', round (cor_perc, 2) ), 
                          x=-Inf, y=Inf, vjust= 2, hjust=-0.05, size=AP$point_fontsize)+
        ggplot2::labs (fill = recode_features (var2) ) + 
        ggplot2::xlab (recode_features (var1))
}

barplot_multi_var <- function (dat, var1, var2, threshold=10){
        dat %>% select (all_of (c(var1, var2))) %>%
                magrittr::set_colnames (c(var1, var2)) %>%
                gather ('variable', 'value', -!!as.symbol (var1)) %>%
                ggplot (aes_string (x=var1, fill='value'))+
                        geom_bar (stat='count', color='black', alpha=transparency) +
                        facet_wrap (~variable) +
                        theme_dotplot () 
}

get_size <- function (dat, size_col){
        size_info <- dat [, size_col] %>% as.character () %>% gsub (pattern=';.+$', replacement='')
        # first remove brackets and their contents
        size_info <- gsub ('\\(.*\\)', '', size_info) %>% trimws ()
        size_info <- gsub ('~.*$', '', size_info) %>% trimws ()

        size_info %>% as.list () %>% lapply (function (x){strsplit (x, ',')[[1]][[1]]}) %>% 
                unlist () %>% as.numeric () -> dat$pixel1
        size_info %>% as.list () %>% lapply (function (x){strsplit (x, ',')[[1]][1:2]}) %>% unlist () -> pixel2
        pixel2 [seq (1, length(pixel2), 2 )] %>% trimws () %>% as.numeric -> dat$pixel2
        return (dat)
}

#' Histogram of image sizes
#'
#' @param dat a dataframe
#' @param size_col which column has size information. The pixel size data is
#' extracted from the column. For example, if a stirng in that column is '256,
#' 256', then the pixel size is $256 \times 256$
#' @param color_by which column contains color information
#' extracted
#' @importFrom ggplot2 aes aes_string
#' @export
size_freq <- function (dat, size_col, color_by, bw=100, AP=NULL){
        AP <- return_aes_param (AP)
        dat <- get_size (dat, size_col)
        dat %>% dplyr::mutate (pixel12 = sqrt (pixel1*pixel2) ) %>% 
                dplyr::mutate (color_by = as.character (!!as.symbol (color_by)) ) %>%
                dplyr::mutate (color_by = factor (color_by, levels=rev (
                                        gtools::mixedsort (unique (color_by)) ) ) ) -> plot_dat

        ggplot2::ggplot(plot_dat, aes_string (x='pixel12')) + 
                ggplot2::geom_histogram (aes_string (fill='color_by', group='color_by'),
                                position='stack', color='black', alpha=AP$transparency, binwidth=bw) + 
                #ggplot2::geom_density (aes (y=bw*..count..), color='red', show.legend=F) + 
                ggplot2::scale_x_log10()+ 
                theme_dotplot (aes_param=AP, color_ve = plot_dat [, color_by]) + 
                ggplot2::xlab (expression (sqrt ('Pixel number') )) + 
                ggplot2::ylab ('Publication number') + 
                ggplot2::labs (fill=recode_features (color_by))
}

one_freq_over_time_cor <- function(dat, var1, var2, thres=1){
        if (sum (grepl (',', dat [, var2]) ) > 1 ){
                print (paste ('resolving overlap for', var2))
                no_lap <- resolve_overlap (dat, var2, var1) 
                var1_vec <- names (no_lap)
                no_lap <- data.frame(no_lap)
                colnames (no_lap) <- var2
                no_lap [, var1] <- var1_vec
                dat <- no_lap
        }
        table (dat [, var1], dat [, var2]) -> dat_tb
        row_sum <- rowSums (dat_tb)
        row_sum [row_sum == 0] <- 1
        dat_tb <- dat_tb/row_sum
        sel_col <- apply (dat_tb==0, 2, sum) <= thres
        dat_tb [, sel_col, drop=F] %>% data.frame () %>%
                magrittr::set_colnames (c('x_var', 'variable', 'freq')) %>%
                group_by (variable) %>% 
                mutate (x_var=as.numeric (x_var) ) %>%
                summarise (correlation = stats::cor (x_var, freq) ) -> dat_df
        dat_df$feature <- var2
        dat_df %>% filter (variable != 'FALSE')
}

#' @export
multi_freq_over_time_cor <- function (dat, var1, vars2){
        all_df <- lapply (as.list (1:length(vars2)), function(i){
                                  one_freq_over_time_cor (dat, var1, vars2[i])} )
        do.call (rbind, all_df) %>% 
                tidyr::drop_na () %>% 
                dplyr::arrange (dplyr::desc (abs (correlation) ) ) %>%
                dplyr::mutate (feature = recode_features (feature)) %>% 
                dplyr::relocate (feature, .before=variable) -> final_df
        if (final_df$feature %>% unique () %>% length () == 1){
                final_df$feature %>% unique () %>% as.character () -> colnames (final_df) [
                                                        colnames (final_df) == 'variable']
                final_df %>% dplyr::select (!feature) -> final_df
        }
        final_df %>% dplyr::mutate (correlation=format (round (correlation, 2), nsmall=2)) %>%
                magrittr::set_colnames (firstup_vec (colnames (final_df) )) 
}

#  ----------design statistics----------

color_name_with_others <- function (color_fields, AP){
     #colvec_names <- gtools::mixedsort (color_fields)
     colvec_names <- color_fields
     if ('Others' %in% colvec_names){
             colvec_names <- c(as.character(colvec_names [!colvec_names %in% 'Others']), 'Others') 
     }
     colvec <- AP$npg_pal [1:length (colvec_names)]
     colvec [colvec_names %in% 'Others'] <- '#808080'
     names (colvec) <- colvec_names
     return (colvec)
}

#' Plot bar plots with labels of the frequency of a category in one single bar
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes aes_string element_blank element_text
#' @noRd
one_freq_bar <- function (rdata, x, max_num, AP){
     table (rdata [,x]) %>% data.frame () %>%  
             magrittr::set_colnames (c(x, 'value')) %>% 
             dplyr::slice_max (value, n=max_num)  -> plot_data

     #plot_data [, x] <- as.character (plot_data [,x])
     plot_ob <- rdata [as.character (rdata [,x]) %in% plot_data [, x], x]

     other_field <- nrow (rdata) - sum (plot_data$value)
     if (other_field > 0) {plot_ob <- c(plot_ob, rep ('Others', other_field) ) }

     plot_ob <- data.frame (plot_ob)
     colnames (plot_ob) <- x
     plot_ob$features <- x

     colvec <- color_name_with_others (levels (factor(plot_ob [, x])), AP)
     plot_ob [, x] <- factor ( plot_ob [, x], levels=names(colvec))
     plot_ob %>% dplyr::select (dplyr::all_of (x) ) %>% 
             table () %>% data.frame () %>%
             magrittr::set_colnames (c('value', 'freq')) -> plot_ob
     plot_ob$features <- x
     all_num <- sum (plot_ob$freq)
     plot_ob$value <- recode_features (plot_ob$value)
     names (colvec) <- recode_features (names (colvec))
     plot_ob$value <- factor (plot_ob$value, levels=names (colvec))
     plot_ob$plot_label <- paste (plot_ob$freq, '\n', '(',
                                  sprintf ('%.1f', plot_ob$freq/all_num*100), '%)',
                                  sep='')

     ggplot2::ggplot (plot_ob, aes_string (y='features', x= 'freq', fill='value') )+
             ggplot2::geom_bar (stat='identity', color='black', alpha=AP$transparency) + 
             ggplot2::geom_text (stat='identity', aes (label = plot_label), size=4, 
                        position=position_stack (0.5), lineheight=0.8) +
             ggplot2::theme (
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    panel.background = element_blank(),
                    panel.border = element_blank (),

                    axis.title.x = element_blank(), 
                    axis.title.y = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank() ,
                    legend.text = element_text (size=AP$fontsize),
                    legend.title = element_text (size=AP$fontsize) ,
                    legend.position = 'top',
                    legend.background = element_rect (fill=NA, color=NA),
                    plot.margin = grid::unit (c(0,0,0,0), 'lines')
                    )+
             ggplot2::scale_fill_manual (values=colvec, breaks=names (colvec))+
             ggplot2::scale_x_reverse () +
             ggplot2::labs (fill=recode_features (x) )+
             ggplot2::coord_cartesian (clip='off')
}

#' Barplot for multiple features
#'
#' @export
freq_bar <- function (rdata, plot_fea, max_num=6, AP=NULL){
        AP <- return_aes_param (AP)
        plot_list <- lapply (as.list (plot_fea), function (x){
                                     one_freq_bar (rdata, x, max_num, AP=AP)} )
        standard_height <- ggplot2::ggplotGrob (plot_list[[1]])$heights
        plot_list <- lapply (plot_list, function (x){
                                     gb <- ggplot2::ggplotGrob (x)
                                     gb$heights <- standard_height
                                     return (gb)
                      })
        ggpubr::ggarrange (plotlist = plot_list, ncol=1)
}

# ----------Continuous variables ----------

#' Boxplot for continuous variable
#'
#' @param x_var a discrete variable
#' @param y_var continuous variables
#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes_string
#' @export
compare_continuous_var_over_feature <- function (x, x_var, y_var, AP=NULL){
        AP <- return_aes_param (AP)
        x [, x_var] <- as.character (x [, x_var])
        x %>% dplyr::select (c(dplyr::all_of(x_var), dplyr::all_of (y_var) ) ) %>%
                tidyr::gather ('feature', 'value', -x_var) %>%
                dplyr::mutate (feature = recode_features (feature) ) %>%
                dplyr::filter (!is.na (value)) %>%
                ggplot2::ggplot (aes_string (y='value', x=x_var) ) +
                ggplot2::geom_boxplot (aes_string (color=x_var), show.legend=F, 
                                       alpha=AP$transparency) + 
                ggplot2::facet_wrap (~feature, scales='free', nrow=1) +
                ggplot2::scale_y_log10 ()+
                ggpubr::stat_compare_means (size=AP$point_fontsize) +
                theme_dotplot (aes_param=AP) 
}
