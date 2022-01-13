# ----------performance analysis----------

#' Plot the performance metrics of all studies
#'
#' @param x a dataframe containing the column `paper`
#' @param metric_types which metrics to plot
#' @param log_shape whether to take the log of the column `shape_col`
#' @param num_col number of columns to show
#' @param color_by which feature to color
#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes aes_string
#' @export
metric_acc <- function (x, metric_types = c('PSNR', 'SSIM', 'NMSE'),
                        metric_col='metric',
                        metric_val_col='value',
                        shape_col='method',
                        log_shape=T,
                        shape_fil=c('proposed', 'ZF'), 
                        num_col=NULL,
                        color_by='acceleration', 
                        AP=NULL){
        AP <- return_aes_param (AP)
        # select the appropriate metric
        x %>% dplyr::filter (!!as.symbol(metric_col) %in% metric_types) %>%
              dplyr::mutate (metric = !!as.symbol (metric_col) ) %>%
              # filter out unwanted methods
              dplyr::filter (!!as.symbol (shape_col) %in% shape_fil ) %>%
              dplyr::mutate (method = firstup_vec (!!as.symbol (shape_col) ) ) %>%
              dplyr::filter (!is.na (!!as.symbol (color_by)) ) %>%
              dplyr::mutate (paper = factor (paper, levels= 
                                             rev (unique (paper))) ) -> plot_data

        # take the log of the column `shape_col`, which is now called `method`
        if (log_shape){
                plot_data %>% dplyr::mutate (color.by = log (!!as.symbol (
                                            color_by)) ) -> plot_data
                shape_lab <- paste ('log', color_by)
        }else{plot_data$color.by <- plot_data [, color_by]; shape_lab <- color_by}

        # draw a dashed line at the mean of all metrics
        plot_data %>% dplyr::group_by (metric) %>% 
                dplyr::summarise (mean_metric = mean (!!as.symbol (metric_val_col), 
                                                      na.rm=T) ) -> line_data

        ggplot2::ggplot (plot_data) +
                ggplot2::geom_point (aes_string (x=metric_val_col, y='paper', 
                                                 fill='color.by', shape='method'), 
                            alpha=0.8, color='black', size=AP$pointsize) +
                ggplot2::geom_vline (aes (xintercept=mean_metric), data=line_data, 
                            color='red', linetype='dashed') +
                ggplot2::facet_wrap (~metric, scales='free_x', ncol=num_col) +
                ggplot2::scale_fill_viridis_c ()+
                ggplot2::scale_shape_manual (values=AP$shape_ops[1:length(shape_fil)], 
                                             breaks=firstup_vec (shape_fil))+
                ggplot2::labs (fill = shape_lab)+ theme_dotplot (aes_param=AP)[[1]] +
                ggplot2::xlab ('Value') + 
                ggplot2::ylab ('Paper') + 
                ggplot2::labs (shape = 'Method')
}

#' @importFrom magrittr %>%
#' @export
filter_main_metric <- function (x, metrics= c('PSNR', 'SSIM')){
        x %>%   dplyr::filter (metric %in% metrics) %>%
                dplyr::filter (method %in% c('proposed', 'ZF') ) %>% 
                tidyr::drop_na () %>%
                dplyr::mutate (acceleration = as.numeric (as.character (acceleration)) ) 
}

#' Scatterplot with raw PSNR and SSIM scores on the x and y axes respectively
#'
#' @param x_df a dataframe containing the performance data
#' @param color_by which feature in `x_df` to color
#' @param label_text whether to label individual dots
#' @importFrom ggplot2 aes aes_string
#' @importFrom magrittr %>%
#' @export
plot_PSNR_SSIM <- function (x_df, color_by, rotation=0, label_text=T,
                            metrics=c('PSNR', 'SSIM'), log_color=F, AP=NULL){
        AP <- return_aes_param (AP)
        x <- filter_main_metric (x_df, metrics)
        x %>% dplyr::filter (method == 'proposed') %>%
                reshape2::dcast (method+acceleration+paper+dataset~metric, mean) %>% 
                dplyr::filter (!is.na (!!as.symbol (metrics[1])) & !is.na (
                                        !!as.symbol (metrics[2]) )) -> plot_df
        if (log_color){
                plot_df [, color_by] <- log (plot_df [, color_by])
                color_legend <- paste ('log', color_by)
        }else{color_legend <- color_by}

        ggplot2::ggplot (plot_df, aes_string(x=metrics[1], y=metrics[2])) +
                ggplot2::geom_point (alpha=0.8, mapping=aes_string (fill=color_by), 
                            size=AP$pointsize, shape=21, color='black') +
                ggplot2::geom_smooth (method='lm', se=F, linetype='dashed', color='red')+
                ggpubr::stat_cor (aes(label=..rr.label..), label.x.npc='left', 
                                  size=AP$point_fontsize, color='red')+
                ggplot2::labs (fill=color_legend)+
                theme_dotplot (aes_param=AP, rotation=rotation) [[1]]+
                ggplot2::coord_cartesian (ylim=c(min (plot_df [, metrics[2]]), max (plot_df [, metrics[2] ])),
                xlim=c(min (plot_df [, metrics[1]]), max (plot_df [, metrics[1] ])) ) -> plot_ob

        if (label_text){
                plot_df %>% 
                        dplyr::group_by (paper) %>%
                        dplyr::summarise (mean_PSNR = mean (PSNR), mean_SSIM = mean (SSIM ) ) -> text_df
                plot_ob <- plot_ob + ggrepel::geom_text_repel (data=text_df, 
                        aes (x=mean_PSNR, y=mean_SSIM, label=paper),
                        size=AP$point_fontsize) 
        }
        return (plot_ob)
}

metrics_cor <- function (all_data, metric1, metric2){
        if (metric1 != metric2){
                x <- filter_main_metric (all_data, c(metric1, metric2))
                x %>% dplyr::filter (method == 'proposed') %>%
                        reshape2::dcast (method+acceleration+paper+dataset~metric, mean) -> plot_df
                plot_df %>% dplyr::filter (!is.na (!!as.symbol (metric1)) & !is.na (
                                                !!as.symbol (metric2) )) -> plot_df
                if (nrow (plot_df ) >3 ){
                        comp_form <- paste (metric1, '~', metric2)
                        plm <- stats::lm(as.formula (comp_form), plot_df)
                        return (summary (plm)$r.squared)
                }else{
                        print ('not enough values')
                        return (NA)}
        }else{return (1)}
}

#' Pairwise correlation matrix
#'
#' @param all_data a dataframe
#' @param metrics a vector of metrics along the rows
#' @param metrics2 a vector of metrics along the columns
#' @export
all_metrics_cor <- function (all_data, metrics, metrics2=NULL){
        if (is.null (metrics2)){metrics2 <- metrics}
        N <- length (metrics)
        N2 <- length (metrics2)
        metric_mat <- matrix (0, N, N2)
        for (i in 1:N){
                for (j in 1:N2){
                        metric_mat [i, j] <- metrics_cor (all_data, metrics[i], metrics2[j])
                }
        }
        rownames (metric_mat) <- metrics
        colnames (metric_mat) <- metrics2
        return (metric_mat)
}

#' Append odds ratio
#'
#' @description Calculate the odds ratio of performance improvement on one
#' metric compared to zero filled data
#' @param rdata a dataframe to be appended with the odss ratio 
#' @param acc_data a dataframe used to calculate the odds ratio. It must have
#' the column: `method` (which must have 2 types of methods--'proposed' and
#' 'ZF'), `paper` for the name of each study
#' @return a dataframe
#' @export
add_impr <- function (rdata, acc_data, one_metric='SSIM'){
        # only focus on those methods that have zero filled
        acc_data %>% reshape2::dcast (...~method, mean) %>% 
                tidyr::drop_na () %>%
                dplyr::mutate (perc_impr = proposed /ZF ) %>% 
                dplyr::filter (metric == one_metric) %>%
                dplyr::group_by (paper) %>%
                dplyr::summarise (mean_impr = mean (perc_impr) ) -> one_metric_df
        rdata [, one_metric ]<- one_metric_df $mean_impr [match (rdata$ID, 
                                                                 one_metric_df$paper) ]
        return (rdata)
}

#' perform Deeks' test on a metric
#'
#' @param x a dataframe with the following column: `testing.number` for the
#' number of samples used in testing, `ID` for the ID of each study
#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes aes_string
#' @export
Deeks_test <- function (x, one_metric='SSIM', AP=NULL){
        AP <- return_aes_param (AP)
        x %>%   dplyr::filter (!is.na (!!as.symbol (one_metric) )) %>%
                dplyr::mutate (test_size = 1/sqrt (testing.number) ) %>%
                ggplot2::ggplot (aes_string (x=one_metric, y='test_size') )+
                ggplot2::geom_point (size=AP$pointsize) +
                ggpubr::stat_cor(aes (label=..p.label..), label.x.npc='center', 
                         size=AP$point_fontsize, color='red', fontface='bold') +
                ggrepel::geom_text_repel (aes (label=ID), size=AP$point_fontsize)+
                ggplot2::stat_smooth (method='lm', se=F) +
                ggplot2::ggtitle (one_metric) +
                ggplot2::xlab ('Odds ratio') + ggplot2::ylab (expression (1/sqrt(ESS))) +
                theme_dotplot (aes_param=AP) 
}

#' Boxplot showing the accuracy of a metric against a selected feature
#'
#' @param rdata a dataframe
#' @param one_metric the y axis
#' @param feature the x axis
#' @param color_by fill the boxes
#' @param max_y manually cap the maximum of y axis in case outlier occurs
#' @param rotation rotation of x axis labels
#' @param num_rotation rotation of the labels for the number of samples for
#' each bar
#' @param comparison whether to compute Kruskal-Wallis test
#' @importFrom ggplot2 aes_string
#' @export
acc_over_feature <- function (rdata, one_metric, feature, color_by, max_y=NA,
                              rotation=0, num_rotation=0, signif_y=NA,
                              comparison=NULL, AP=NULL, remove_NA=F){
        AP <- return_aes_param (AP)
        # obtain the sample sizes
        if (remove_NA){
                rdata %>% dplyr::filter (!is.na (!!as.symbol (one_metric) ) ) -> rdata
        }
        rdata [, one_metric] <- as.numeric (rdata [, one_metric])
        rdata [, feature] <- factor (rdata [, feature])
        rdata -> no_na
        rdata %>% dplyr::filter (!is.na (!!as.symbol (one_metric) )) %>% 
                dplyr::select (!!as.symbol (feature) ) %>% 
                table () %>% data.frame () -> sample_size

        colnames (sample_size) <- c(feature, 'num')
        sample_size$num <- paste ('n =', sample_size$num)
        if (!is.na (max_y)){max_val <- max_y}else{max_val <- max (no_na [, one_metric])}
        sample_size$y_val <- max_val

        no_na [, feature] <- as.character (no_na [, feature])
        pval <- stats::kruskal.test (as.formula (paste (one_metric, '~', feature) ), 
                                     data = no_na)$p.value
        recoded_col <- recode_features (color_by)
        recoded_x <- recode_features (feature)
        recoded_y <- recode_features (one_metric)

        ggplot2::ggplot (no_na, aes_string (x=feature, y=one_metric) ) +
                #ggplot2::geom_point (size=0, shape=22)+
                ggplot2::geom_boxplot (position=ggplot2::position_dodge (), 
                                       mapping=aes_string (color=color_by))+
                ggplot2::geom_text (aes_string (x=feature, label='num', y='y_val'),
                           data=sample_size, size=AP$point_fontsize, angle=num_rotation)+
                ggplot2::xlab (recoded_x) + 
                ggplot2::labs (color=recoded_col)+ 
                ggplot2::ylab (recoded_y)+
                ggplot2::ggtitle (paste ('Kruskal-Wallis p =', format (round (pval, 2), nsmall=2)))+
                theme_dotplot (rotation=rotation, color_vec=no_na[, color_by])+
                ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 22))) -> plot_ob

        if (!is.na (max_y)){
                vec <- no_na %>% dplyr::filter (!!as.symbol (one_metric) < max_y) %>%
                        dplyr::pull (!!as.symbol (one_metric) )
                plot_ob <- plot_ob + custom_tick (vec=vec, limits=c(NA, max_y))
        }
        if (!is.null(comparison)){plot_ob <- plot_ob + ggpubr::stat_compare_means (
                                comparison=comparison, label='p.adj', label.y=signif_y, 
                                size=AP$point_fontsize) }
        return (plot_ob)
}
