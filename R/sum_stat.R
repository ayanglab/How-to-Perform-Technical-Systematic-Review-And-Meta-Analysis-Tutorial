#' p value for whether one feature affects performance. 
#'
#' @param rdata the input dataframe
#' @param one_metric metric for performance, must be a continuous variable
#' @param selected_features a vector of features to compare. If a feature is
#' categorical, Wilcoxon rank sum test will be carried out. If it is
#' continuous, the R2 correlation will be computed
#' @param save_dir which directory to save the results as a text file
compare_multi_features <- function (rdata, one_metric, selected_features, save_dir){
        no_na <- rdata [!is.na (rdata [, one_metric]),]
        result_file <- paste (save_dir, paste ('sig_p_val_', one_metric, '.txt', sep=''), sep='/')
        if (file.exists (result_file)){file.remove (result_file)}
        sink (result_file)
        for (one_feature in selected_features){
                # for continuous variable, R2 is calculated
                if (is.numeric (rdata [, one_feature]) ){
                        no_na <- rdata [!is.na (rdata [, one_feature])  & !is.na (rdata [, one_metric]), ]
                        cor (no_na [, one_feature], no_na [, one_metric])^2 -> R2_val
                        cat ( paste ('R2 for', one_feature, ':', R2_val) )

                # for categorical variable, Wilcox test is used
                }else{
                        # only if there are more than 1 category
                        if ( length (unique (rdata [, one_feature]) ) > 1 ){
                                cat ( paste ('Wilcoxon rank sum test for', one_feature, ': ') )
                                compa_formula <- paste (one_metric, '~', one_feature)
                                compare_means (as.formula (compa_formula), data=no_na, 
                                               method='wilcox.test')$p.adj %>% cat ()
                                cat ( '\n' )
                                cat (table (no_na [, one_feature]))
                                cat ('\n')
                                no_na %>% group_by (!!as.symbol (one_feature)) %>% 
                                        summarise (mean_val = mean (!!as.symbol (one_metric)) ) %>% 
                                        print ()
                        }
                }
                cat ('\n')
                cat ('-------------------------------------------------- \n')
        }
        sink ()
}

logic_numer <- function (x_str){
        is.numeric (x_str) | is.logical (x_str)
}

test_p <- function (com_df, x_val, y_val, ID=NA){
        # use Wilcoxon rank sum test for categorical variable x_val and numerica
        # variable y_val
        if (!is.numeric (com_df [, x_val]) & is.numeric (com_df[, y_val]) ){
                print ('using Wilcoxon rank sum test')
                compa_formula <- paste (y_val, '~', x_val)
                ggpubr::compare_means (as.formula (compa_formula), data=com_df,
                                       method='wilcox.test',
                                       ref.group='absence')$p -> p_val
                p_val <- data.frame (p=p_val, group1=ID, group2='absence', method='wilcox') 

        # If both variables are ordinal/categorical, use chisq
        }else if (!is.numeric (com_df [, x_val]) & !is.numeric (com_df [, y_val]) ){
                print ('using x2')
                stats::chisq.test (com_df [, x_val], com_df [, y_val] )[['p.value']] -> p_val
                p_val <- data.frame (p=p_val, group1=ID, group2='absence', method='chisq') 

        # If both variables are numerical, use correlation test
        }else if (is.numeric (com_df [, x_val]) & is.numeric (com_df [, y_val]) ){
                print ('using correlation')
                stats::cor.test (com_df [, x_val], com_df [, y_val], method='pearson')[['p.value']] -> p_val
                p_val <- data.frame (p=p_val, group1=x_val, group2='absence', method='Pearson') 
        }else{
                print ('no tests available')
                p_val <- NULL
        }
        # append other information
        if (!is.null (p_val)) {
                if (!is.numeric (com_df [, x_val]) & is.numeric (com_df [, y_val])){
                        com_df %>% group_by (!!as.symbol (x_val)) %>%
                                dplyr::summarise (mean_y = mean (!!as.symbol (y_val)) ) -> mean_dat
                        p_val$absence <- mean_dat$mean_y [mean_dat[, x_val] == 'absence']
                        p_val$presence <- mean_dat$mean_y [mean_dat[, x_val] == as.character (ID)]
                }else if (is.logical (com_df [, y_val]) ){
                        p_val$absence <- sum (com_df [com_df [, x_val] == 'absence', y_val] )
                        p_val$presence <- sum (com_df [com_df [, x_val] != 'absence', y_val] )
                }else{
                        p_val$absence <- NA
                        p_val$presence <- NA
                }
                p_val$x <- x_val
                p_val$y <- y_val
                p_val$absence_num <- sum (com_df [, x_val] == 'absence')
                p_val$presence_num<- sum (com_df [, x_val] != 'absence')
        }
        return (p_val)
}

#' y_val ~ x_val if x_val is categorical
compare_2_group_per_metric <- function (com_df, x_val, y_val, ID){
        com_df %>% dplyr::select (all_of (c(x_val, y_val) )) %>% 
                tidyr::drop_na () -> com_df
        print (paste ('processing', x_val, ID, 'for', y_val))

        com_df [, x_val] <- as.character (com_df [, x_val])
        com_df [com_df[, x_val] != ID, x_val] <- 'absence'

        if (length (com_df [com_df[, x_val] == ID, x_val]) > 2){
                p_val <- test_p (com_df, x_val, y_val, ID)
                return (p_val)
        }
}

compare_multi_group_per_metric <- function (com_df, x_val, y_val){
        if (x_val != y_val){
                # need a comparison group for categorical variable
                if (!is.numeric (com_df [, x_val]) ){
                        all_var <- unique (com_df [, x_val]) 
                        all_var <- all_var [!is.na (all_var) & all_var != 'FALSE']
                        out_df <- lapply (as.list(all_var), function(i){compare_2_group_per_metric (
                                                                        com_df, x_val, y_val, i) })
                        return (do.call (rbind, out_df))
                # for numerical variable, use test_p directly
                }else{
                        p_val <- test_p (com_df, x_val, y_val)
                        if (!is.null (p_val)) {return (p_val)}
                }
        }
}

compare_multi_features_per_metric <- function (com_df, x_vals, y_val){
        out_df <- lapply (as.list(x_vals), function(i){compare_multi_group_per_metric (
                                                        com_df, i, y_val) })
        return (do.call (rbind, out_df))
}

#' Calculate p values for multiple variables
#'
#' @description This function calculates the p value between all possible pairs
#' of variables between those in `x_vals` and those in `y_vals`. The `y_vals`
#' must be numerical. If the `x_vals` is logical, Wilcoxon rank sum test
#' will be carried out. If the `x_vals` is numerical, p value will be that from
#' linear regression.
#' @return a dataframe with the following column:
#' `x`: the independent variable
#' `y`: the dependent variable
#' `feature`: which group in `x`
#' `presence`: the value of `y` in that group
#' `absence`: the value of `y` outside that group
#' `presence_num`: number of samples in that group
#' `absence_num`: number of samples outside that group
#' `p`: unadjusted p value
#' `p_adj`: adjusted p value
#' @importFrom magrittr %>%
#' @export
compare_multi_features_multi_metrics <- function (com_df, x_vals, y_vals, padj_method='hochberg'){
        out_df <- lapply (as.list(y_vals), function(i){compare_multi_features_per_metric (
                                                        com_df, x_vals, i) })
        print ('final merging')
        do.call (rbind, out_df) %>%  
                tidyr::unite ('feature', c('group1', 'group2'), sep='') %>% 
                dplyr::mutate (feature= gsub ('absence', '', feature)) -> out_df
        out_df$p_adj <- stats::p.adjust (out_df$p, method=padj_method)
        out_df %>% dplyr::relocate (p, .before=p_adj) %>% 
                dplyr::relocate (x, .before=feature) %>% 
                dplyr::relocate (y, .before=x) %>% 
                dplyr::mutate (x= recode_features (x)) %>%
                dplyr::mutate (method=gsub ('wilcox', 'Wilcoxon',method)) %>%
                dplyr::mutate (feature=gsub ('max_acc', 'NA',feature)) %>%
                dplyr::mutate_at (c('absence', 'presence', 'p', 'p_adj'), 
                                  function (i){format (round (i,2), nsmall=2) }) %>%
                magrittr::set_colnames (c('Metric', 'Feature', 'Variable',
                                          'Method', 'Absence', 'Presence',
                                          'Absence number', 'Presence number', 
                                          'p-value', 'Adjusted p-value'))
}
