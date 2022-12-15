
# `survivalROC_helper()` function
# source: (https://datascienceplus.com/time-dependent-roc-for-survival-prediction-models-in-r)

## Define a helper function to evaluate at various t
# Not @export

survivalROC_helper <- function(t, data, mm, tt, ss) {
     
    survivalROC::survivalROC(Stime = data[[tt]],
                status       = data[[ss]],
                marker       = data[[mm]],
                predict.time = t,
                method       = "NNE",
                span = 0.25 * nrow(data)^(-0.20))
}


#' `create_survivalROC_data()` function 

#' @export
create_survivalROC_data <- function(tvec, data, marker, time = time, status=status){
        dnm <- as.character(substitute(data))
        mm <- as.character(substitute(marker))
        tt <- as.character(substitute(time))
        ss <- as.character(substitute(status))
        cnms <- colnames(data)
        exit <- FALSE
        if (!any(cnms == mm)) exit <- TRUE
        if (!any(cnms == tt)) exit <- TRUE
        if (!any(cnms == ss)) exit <- TRUE
    
        if (exit) stop("One(or more) variables: ", mm, ", ", tt, ", ", ss, " not found in ", dnm) 

    tibble(t = tvec) %>%
    mutate(survivalROC = map(t, survivalROC_helper, data = data, mm = mm, tt=tt, ss=ss),
           ## Extract scalar AUC
           auc = map_dbl(survivalROC, magrittr::extract2, "AUC"),
           ## Put cut off dependent values in a data_frame
           df_survivalROC = map(survivalROC, function(obj) {
               as_tibble(obj[c("cut.values","TP","FP")])
           })) %>%
           select(-survivalROC) %>%
    unnest(df_survivalROC) %>%
    arrange(t, FP, TP)
    }


# Auxiliary `plot_timedep_ROC()` function to Plot Time-dependent ROC is defined

#' @export
plot_timedep_ROC <- function(surv_ROC_data){
 surv_ROC_data %>%
    ggplot(mapping = aes(x = FP, y = TP)) +
    geom_point() +
    geom_line() +
    geom_label(data = surv_ROC_data %>% select(t, auc) %>% unique,
               mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
    facet_wrap( ~ t) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.key = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank())
    }     
