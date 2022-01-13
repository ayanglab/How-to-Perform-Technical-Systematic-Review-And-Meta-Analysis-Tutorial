# ----------load manually annotated study design features----------

#' Combine csv files containing performance data
#'
#' @param root where the csv files are stored. Each csv must contain the
#' following columns: `acceleration`, `method`, `paper`, and columns for
#' different metrics.
#' @return a list
#' @description This function appends the following columns:
#' `paper`: extracted from the csv filename
#' `dataset`: if not in the csv file, it will be 'dataset0'
#' `mask`: by uniting the `dataset` and `mask` columns
load_all_studies <- function (root){
        all_files <- list.files(root, pattern='*.csv')
        all_data <- list ()
        for (i in 1:length (all_files)){
                print (paste ('loading', all_files[i] ))
                one_study <- read.csv (paste (root, all_files[i], sep='/') )
                one_study$paper <- gsub ('.csv', '', all_files[i])
                if (! 'dataset' %in% colnames (one_study)){one_study$dataset <- 'dataset0'}
                if ('mask' %in% colnames (one_study)){
                        one_study %>% tidyr::unite ('dataset', c('dataset', 'mask'), sep='_') -> one_study}
                all_data [[i]] <- one_study
        }
        names (all_data) <- gsub ('.csv', '', all_files)
        return (all_data)
}

#' Clean the merged dataset
#'
#' @description This function cleans the output from `load_all_studies`.
#' Firstly, 2 columns are created: `metric` and `value` by melting the
#' dataframes in each element of the input list.
#' @return a dataframe
merge_all_studies <- function (all_list){
        for (i in 1:length (all_list)){
                print (names (all_list)[i])
                all_names <- colnames (all_list[i])
                all_list[[i]] %>% tidyr::gather ('metric', 'value', -dataset, 
                                          -acceleration, -method, -paper) -> all_list[[i]]
        }
        all_df <- do.call (rbind, all_list)
        for (remove_symbols in c('\u00B1', '\\(', '\\+') ){
                gsub ('\u00B1', '', all_df$value)
                regexpress <- paste (remove_symbols, '.*$', sep='')
                all_df$value <- gsub (regexpress, '', all_df$value) 
        }
        all_df$value %>% stringr::str_trim () %>% as.numeric () -> all_df$value
        return (all_df)
}


remove_bracket <- function (x){gsub ( '\\(.*\\)', '', x) %>% trimws ()}

remove_bracket_delim <- function (x, delim=','){
        sapply (x, function (xi) {
                sep_vec <- strsplit (as.character (xi), delim)[[1]]
                paste (remove_bracket (sep_vec), collapse=',')
        })
}

#' Use a cleaner term to represent the names of model design features
#' 
#' @param xx a string
#' @param recode_dict a dataframe with 2 columns: `original` and `new`
replace_feature <- function (xx, recode_dict){ 
        if (xx %in% recode_dict$original){
                return (as.character (recode_dict$new) [as.character (
                                                recode_dict$original) == xx])
        }else{return (xx) }
}

#' Convert the first letter of a string to upper case
#'
#' @param x a single character string
#' @noRd
firstup <- function (x){
        paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

#' @export
firstup_vec <- function (x){sapply (x, firstup) }

recode_features <- function (x, recoding=NULL){
        if (is.null (recoding)){data (recode_dict, package='deepCSMRI'); recoding <- recode_dict}
        x_level <- levels (factor (x) )
        new_level <- sapply (x_level, function (i){replace_feature(i, recoding)} )
        new_x <- new_level [match (x, x_level) ]
        sub_ind <- !is.na (new_x)
        new_x [sub_ind] <- firstup_vec (new_x[sub_ind])
        return (new_x)
}

#' Preprocess the data for performance accuracy
#'
#' @param x a dataframe with the columns: `method`, `acceleration`, `paper`
#' @importFrom magrittr %>%
#' @noRd
#' @export
preprocess_acc_mat <- function (x){
        x$method <- as.character (x$method)
        x$acceleration %>% as.character () %>% as.numeric () -> x$acceleration
        x [x$paper == x$method, 'method'] <- 'proposed'
        x$paper <- gsub ('_', ' ', x$paper)
        return (x)
}
