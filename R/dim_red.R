# ----------PCA----------

#' @importFrom magrittr %>%
preprocess_pca <- function (x, selected_features){
        x %>% dplyr::select (selected_features) %>% data.frame () -> sel_data
        # convert logical vectors into 0 and 1
        for (i in selected_features [!selected_features %in% c('dimension', 'max_acc', 'year')]){
                sel_data [, i] <- as.logical (sel_data [, i])
                sel_data [, i] <- ifelse (sel_data [,i], 1, 0)
        }

        # the inputs have been in factor form, need to make it numeric
        sel_data %>% as.matrix () %>% apply (2, as.numeric) -> sel_data
        sel_data [is.na (sel_data)] <- 0
        return (sel_data)
}

#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes_string
pca_dim_red <- function (pca_dat, x, color_by, AP=NULL,...){
        AP <- return_aes_param (AP)
        pca_dat$x %>% data.frame () %>%
                tibble::add_column (study = rownames (x) ) %>%
                cbind ('category'= x [, color_by]) -> plot_data
        ggplot2::ggplot (plot_data, aes_string(x='PC1', y='PC2')) +
                        ggplot2::geom_point (aes_string (color='category'), 
                                             show.legend=F, alpha=AP$transparency)+
                        ggrepel::geom_text_repel (aes_string (label='study', color='category' ), 
                                                   size=AP$point_fontsize)+
                        ggplot2::labs (color=recode_features (color_by) ) +
                        theme_dim_red () -> plot1
        print ('plotting')
        return ( plot1 + arrow_axis (plot1, ...))
}

get_feature_size <- function (x, one_feature){
        if (one_feature == 'dimension'){
                x [, one_feature] <- x [, one_feature] == 2
        }else if ( one_feature == 'max_acc' ){ 
                x [, one_feature] <- x [, one_feature] > quantile (x [, one_feature], 1/3, na.rm=T)
        }else{ x [, one_feature] <- as.logical (x [, one_feature]) }
        return (mean(x [, one_feature], na.rm=T) )
}

#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes_string
pca_rotation <- function (pca_dat, x, AP=NULL,...){
        AP <- return_aes_param (AP)
        sel_features <- rownames (pca_dat$rotation)
        label_size <- sapply (as.list (sel_features), function (inp){get_feature_size (x,inp)} )
        pca_dat$rotation %>%
                as.data.frame () %>%
                tibble::add_column (labsize = label_size) %>%
                dplyr::mutate (feature = recode_features (sel_features) ) %>%
                ggplot2::ggplot (aes (x=PC1, y=PC2) ) + 
                ggplot2::geom_point (aes (size=labsize), alpha=0.3)+
                ggrepel::geom_text_repel (aes (label=feature), size=AP$point_fontsize) +
                ggplot2::scale_size (range = c(1, 20))+
                ggplot2::labs (size='Proportion')+
                theme_dim_red (aes_param=AP) -> plot2
        return (plot2 + arrow_axis (plot2, ...))
}

plot_pca <- function (x, selected_features, color_by='category', return_sep=F, ...){
        sel_data <- preprocess_pca (x, selected_features)
        prcomp (sel_data, scale=T, center=T) -> pca_dat

        # PCA of studies
        plot1 <- pca_dim_red ( pca_dat, x , color_by, ...)
        # PC contributions of featurs
        plot2 <- pca_rotation ( pca_dat, x, ...)
        if (return_sep){return (list (plot1, plot2))
        }else{return (ggpubr::ggarrange (plot1 , plot2))}
}

# ----------PCA incorporating GMM parameters----------

#' Plot PCA with the cluster results and the mean and variance of the clusters
#' as ellipses
#'
#' @description need to load mclust first
#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes_string
#' @export
pca_GMM <- function (x, selected_features, GMM_mean=NULL, GMM_var=NULL, dims=c(1,2),
                     cluster_col='cluster', AP=NULL, ...){
        AP <- return_aes_param (AP)
        sel_data <- preprocess_pca (x, selected_features)
        print (apply (sel_data, 2, sd) )
        stats::prcomp (sel_data, scale=T, center=T) -> pca_dat

        if (is.null (GMM_mean) | is.null (GMM_var)){
                sel_data %>% scale () %>% mclust::Mclust(G=1:20) -> d_clust
                x$cluster <- paste ('cluster', d_clust$classification, sep='')
                colnames (d_clust$parameters[['mean']]) <- paste ('cluster', 
                                                                  1:length (unique (x$cluster)), sep='' )
                covar <- d_clust$parameters[['variance']] [['sigma']]
                GMM_var <- lapply (as.list (1:dim(covar)[3]), 
                                   function (x) {diag (covar [,,x]) } )
                GMM_var <- do.call (cbind, GMM_var)
                colnames (GMM_var) <- colnames (d_clust$parameters[['mean']])
                GMM_mean <- d_clust$parameters[['mean']]
        }

        # calculate the mean and variance along the PCs
        w <- pca_dat$rotation
        proj_mean <- t(GMM_mean) %*% w
        # set a low tolerance to prevent numerical instability
        proj_var <- t(solve (w^2, as.matrix(GMM_var), tol=1e-20))
        proj_var <- 2*sqrt (proj_var)
        colnames (proj_mean) <- paste ('mean_', colnames (proj_mean), sep='')
        colnames (proj_var ) <- paste ('var_',  colnames (proj_var ), sep='')
        proj_all <- cbind (proj_mean, proj_var)
        proj_all %>% data.frame () %>% tibble::add_column (
                                        cluster = rownames (proj_all) ) -> proj_all
        x %>% dplyr::select (dplyr::all_of (c('cluster', cluster_col)) ) %>% 
                tibble::deframe () -> clust_names
        clust_names <- clust_names [!duplicated (clust_names)]
        proj_all$cluster <- clust_names [match (names (clust_names), proj_all$cluster) ]

        axes <- paste ('PC', dims[1:2], sep='')
        ellipse_coord <- paste ('mean_', axes, sep='')
        ellipse_size <- paste ('var_', axes, sep='')
        pca_dat$x %>% data.frame () %>% cbind ('Category'= x[, cluster_col]) %>%
                ggplot2::ggplot (aes_string(x=axes[1], y=axes[2])) +
                        ggplot2::geom_point (aes_string (color='Category'), 
                                             size=AP$pointsize, alpha=AP$transparency)+
                        ggforce::geom_ellipse (aes_string (x0=ellipse_coord[1], y0=ellipse_coord[2], 
                                                  a=ellipse_size[1], b=ellipse_size[2], 
                                                  color='cluster', angle=0),
                                      inherit.aes=F, data=proj_all, show.legend=F) +
                        ggrepel::geom_text_repel (aes_string (x=ellipse_coord[1], y=ellipse_coord[2], 
                                         label='cluster', color='cluster'), data=proj_all, 
                                         show.legend=F, size=AP$point_fontsize)+
                        theme_dim_red (aes_param=AP)[1:3] +
                        ggplot2::theme (legend.position='none') -> pc_plot
                        #ggplot2::guides (color=ggplot2::guide_legend (ncol=3)) 
        return (pc_plot + arrow_axis (pc_plot, ...) )
}
