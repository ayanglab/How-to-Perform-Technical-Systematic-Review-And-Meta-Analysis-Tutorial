#' @export
custom_heat <- function (mat, clustering=NULL, legend_title = 'norm value',
                         style='biology', column_split=NULL, cluster_rows=T,
                         cluster_columns=T, width=7, height=7, grid_height=8,
                         AP=NULL, ...) {
        AP <- return_aes_param (AP)
        if (style == 'pheamap'){
                color <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(
                                                  n = 7, name = "RdYlBu")))(100)
        }
        if (style == 'biology'){color <- c("#00B0F0", "#FFF2F1", "#FF0000")} #blue, white, red
        font_lab <- grid::gpar (fontsize=AP$fontsize, fontfamily=AP$font_fam)

        if (!is.null (clustering)){
        anna_param <- list (labels_gp=gpar(fontsize=AP$fontsize),
                            title_gp=gpar(fontsize=AP$fontsize),
                            )
        HA_df <- data.frame (cluster = clustering)
        color_vec_names <- unique (clustering) 
        color_vec <- AP$npg_pal [1:length (color_vec_names) ]
        names (color_vec) <- color_vec_names
        color_map_list <- list (cluster = color_vec)
        hori_bars<- ComplexHeatmap::HeatmapAnnotation (
                                       df=HA_df, which='column', 
                                       col= color_map_list,
                                       annotation_legend_param=anna_param,
                                       annotation_name_gp = gpar (fontsize=AP$fontsize))
        }else{hori_bars <- NULL}
        if (!is.null (column_split) ){column_split <- HA_df$cluster}

        break_points <- seq(min (mat, na.rm=T), max (mat, na.rm=T), length.out=3) %>% round (2)
        heat_param <- list(title=legend_title, title_gp=font_lab,
                           labels_gp=font_lab, at=break_points,
                           title_position='leftcenter',
                           grid_width=grid::unit (grid_height, 'mm'),
                           direction='horizontal'
        )
        ComplexHeatmap::Heatmap(mat, column_title_rot=90, col = color,
                            row_names_gp= font_lab, 
                            top_annotation=hori_bars, 
                            column_names_gp=font_lab, 
                            column_split = column_split,
                            cluster_rows= cluster_rows,
                            cluster_columns= cluster_columns,
                            heatmap_legend_param =heat_param,
                            width=unit (width, 'in'), height=unit (height, 'in'), ...)
}

add_heat <- function (x, mat, group.by=NULL, ID_col='ID', cluster_column=T,
                      cluster_rows=T,...){
        metadata <- x [match (colnames (mat), x [, ID_col]), ]
        rownames (metadata) <- colnames (mat)
        sel_seurat <- Seurat::CreateSeuratObject (mat, meta.data=metadata)
        color_row <- as.character (rownames (sel_seurat))
        TBdev::seurat_heat (sel_seurat, group.by = group.by, color_row=
                            color_row, left_HA=F, center_scale=T, automatic=F,
                    column_rotation=90, column_legend_labels='Cluster',
                    row_names_side='right',
                    cluster_column=cluster_column, cluster_rows=cluster_rows, ...)
}

hclust_feature <- function (x, sel_feature, cell_size=35, print_mat=F, ...){
        sel_data <- DIR$preprocess_pca (x, sel_feature)
        rownames (sel_data) <- CL$recode_features (rownames (sel_data))
        colnames (sel_data) <- CL$recode_features (colnames (sel_data))
        sel_data %>% as.matrix() %>% apply (2, as.numeric) %>% stats::cor () ->sel_mat
        if (print_mat){sel_mat %>% data.frame () %>% add_column (var1= rownames (sel_mat)) %>% 
                gather ('var2', 'corr', -var1) %>% arrange (corr) %>% print ()}
        x <- data.frame (cluster = colnames (sel_mat), ID=colnames (sel_mat))
        add_heat (x, sel_mat, group.by='cluster', top_HA= F, column_split=NA,
                  show_column_names=T, heat_name= 'Correlation', ...)
}

hclust_study <- function (x, sel_feature, group.by, print_mat=F, ...){
        sel_data <- DIR$preprocess_pca (x, sel_feature)
        colnames (sel_data) <- CL$recode_features (colnames (sel_data))
        sel_data %>% as.matrix () %>% apply (2, as.numeric) %>% t ()%>%
                magrittr::set_colnames (rownames (x)) -> sel_mat
        color_vec_names <- levels(x [, group.by])
        color_vec <- AP$npg_color_hue(length (color_vec_names)) 

        # set transparency to 0.6
        color_vec <- paste (color_vec, '99', sep='')
        names (color_vec) <- color_vec_names
        return (add_heat (x, sel_mat, group.by, heat_name='Norm value',
                          row_scale=T,AP=list(color_vec=color_vec),...))
}
