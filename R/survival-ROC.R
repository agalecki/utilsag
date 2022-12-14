
# `survivalROC_helper()` function
# source: (https://datascienceplus.com/time-dependent-roc-for-survival-prediction-models-in-r)

## Define a helper function to evaluate at various t
# Not @export

survivalROC_helper <- function(t, data, marker, time = time, status = status) {
    mm <- as.character(substitute(marker))
    tt <- as.character(substitute(time))
    ss <- as.character(substitute(status))
    cnms <- colnames(data)
    exit <- FALSE
    if (!str_detect(cnms, mm)) exit <- TRUE
    if (!str_detect(cnms, tt)) exit <- TRUE
    if (!str_detect(cnms, ss)) exit <- TRUE
    if (exit) stop("One(or more) variables:", mm, tt, ss, " not found in ", data) 
    
    stop("Variable ", mm, " not found"
    
    survivalROC::survivalROC(Stime = data[[tt]],
                status       = data[[ss]],
                marker       = data[[mm]],
                predict.time = t,
                method       = "NNE",
                span = 0.25 * nrow(data)^(-0.20))
}

# dtx <-survivalROC_helper(15, dt, disp)    
#names(dtx)


# `create_survivalROC_data()` function 

create_survivalROC_data <- function(tvec, cmarker){ 
    # Ex. cmarker = "lp_M2"
    tibble(t = tvec) %>%
    mutate(survivalROC = map(t, survivalROC_helper, markr = cmarker),
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
