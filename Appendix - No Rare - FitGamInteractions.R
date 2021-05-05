################################################################################
##  fit_cumulative_gams.R: Script that fits GAMs to the cumulative RAC metrics
##  data. The goal is to compare models with and without a treatment effect.
##
##  Author: Andrew Tredennick (atredenn@gmail.com)
##  Date created: March 21, 2018
##  Date update: 2020-06-08
##    1. Added check to abort GAM fitting if response has fewer than 2 unique
##       values.
##    2. Added test of NA fractions on a per-treatment basis.
##    3. Added tryCatch to drop any GAMs that produce warnings.
################################################################################

# NOTES:
#  (1) for deviance and AIC, negative deltas indicate better models
#  (2) the p-value then says whether the deviance difference is significant
#  (3) some p-values will be NA -- this is OK and indicates the full the model
#      is DEFINITELY NOT BETTER than the null model. So, think of NA as p>0.05.


##  Clear the workspace
rm(list = ls(all.names = TRUE))



####
####  LOAD LIBRARIES -----------------------------------------------------------
####
library(tidyverse)
library(mgcv)
library(rlang)



####
####  SET WORKING DIRECTORIES AND FILENAMES ------------------------------------
####

# ##meghan's computer
setwd("C:\\Users\\mavolio2\\Dropbox\\C2E\\Products\\CommunityChange\\March2018 WG\\")
dat<-read.csv("CORRE_RAC_Metrics_norares.csv")
treatment_info<-read.csv("ExperimentInformation_March2019.csv")%>%
  dplyr::select(site_code, project_name, community_type, treatment,plot_mani)%>%
  unique()%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))

####
####  DEFINE MODEL FITTING FUNCTION --------------------------------------------
####

fit_compare_gams <- function(df, response, diff_type = "last_year"){
  # Fits two GAMs and compares them with AIC and LLR
  #
  # Args:
  #  data: a dataframe with necessary columns for fitting the GAMMs
  #  response: name of the response variable, must be a column in the dataframe
  #  diff_type: type of treatment-control different to return. options are one
  #   of "all_years" (mean of all diffs), "year_five" (diff at year 5), and
  #   "last_year" (diff at final year of data). Note that the option "year_five"
  #   requires the dataset to have at least five years of data. "last_year" is
  #   default.
  #
  # Returns:
  #  A tibble with LLR delta deviance, LLR p-value, and delta AIC
  
  # Check that there aren't too many NAs and skip modeling if fraction > 0.5
  y <- df %>% pull(response)
  # num_nas <- length(which(is.na(y)))
  # fraction_nas <- num_nas/length(y)
  
  fraction_nas <- df %>% 
    ungroup() %>%
    dplyr::select(response, treatment) %>%
    group_by(treatment) %>%
    summarize(frac_nas = sum(is.na(UQ(rlang::sym(response)))) / n())
  test <- length(which(fraction_nas$frac_nas > 0.5))
  
  if(test | length(unique(y)) < 2){
    return(
      tibble(
        response_var = response,
        p_value = -9999,
        delta_deviance = NA,
        delta_aic = NA,
        diff = NA,
        diff_se = NA,
        diff_lower = NA,
        diff_upper = NA,
        diff_treatment_year = NA
      )
    )
  }
  
  if(!test & length(unique(y)) > 1){
    test_formula <- as.formula(
      paste(response, 
            "~ treatment + s(treatment_year, by = treatment, k = (num_years-1)) + 
            s(plot_id, bs='re')"
      )
    )
    
    null_formula <- as.formula(
      paste(response, 
            "~ s(treatment_year, k = (num_years-1)) + 
            s(plot_id, bs='re')"
      )
    )
    
    gam_test <- tryCatch(expr = {
      gam(
        test_formula, 
        data = df,
        method = "REML",
        family = gaussian(link = "identity")
      )
    }, error = function(e) e, warning = function(w) w)
    
    
    gam_null <- tryCatch(expr = {
      gam(
        null_formula, 
        data = df,
        method = "REML",
        family = gaussian(link = "identity")
      )
    }, error = function(e) e, warning = function(w) w)
    
    test1 <- class(gam_test)[1] %in% c("simpleWarning", "simpleError", "try-error")
    test2 <- class(gam_null)[1] %in% c("simpleWarning", "simpleError", "try-error")
    
    if(test1 == FALSE & test2 == FALSE) {
      # LLR tests
      pvalue <- anova(gam_null, gam_test, test="Chisq")$`Pr(>Chi)`[2]
      dev <- anova(gam_null, gam_test, test="Chisq")$`Resid. Dev`
      delta_div <- diff(dev)  # full - null
      
      # AIC tests
      aics <- AIC(gam_null, gam_test)$AIC
      delta_aic <- diff(aics)  # full - null
      
      # Simulate predictions from the interaction model
      min_year <- min(df$treatment_year)
      max_year <- max(df$treatment_year)
      pdat <- expand.grid(
        treatment_year = seq(min_year, max_year, by = 1),
        treatment = unique(df$treatment)
      )
      pdat$plot_id <- unique(df$plot_id)[1]
      
      control_name <- filter(df, plot_mani == 0) %>% pull(treatment) %>% unique()
      treat_name <- filter(df, plot_mani != 0) %>% pull(treatment) %>% unique()
      tmp_diffs <- smooth_diff(model = gam_test, newdata = pdat, 
                               f1 = treat_name, 
                               f2 = control_name, 
                               var = "treatment", alpha = 0.05, 
                               unconditional = FALSE)
      
      if(diff_type == "all_years"){
        outdiff <- as.data.frame(t(colMeans(tmp_diffs[c("diff","se","upper","lower")])))
        outdiff$treatment_year <- NA
      }
      
      if(diff_type == "mid_year"){
        # find median index, rounds up
        # mid_year <- floor(0.5 + median(1:nrow(tmp_diffs)))
        mid_year <- 5
        outdiff <- tmp_diffs[mid_year,] 
      }
      
      if(diff_type == "last_year"){
        outdiff <- tail(tmp_diffs, 1)
      }
      
      return(
        tibble(
          response_var = response,
          p_value = pvalue,
          delta_deviance = delta_div,
          delta_aic = delta_aic,
          diff = outdiff$diff,
          diff_se = outdiff$se,
          diff_lower = outdiff$lower,
          diff_upper = outdiff$upper,
          diff_treatment_year = outdiff$treatment_year
        )
      )
    } else {
      return(
        tibble(
          response_var = response,
          p_value = -9999,
          delta_deviance = NA,
          delta_aic = NA,
          diff = NA,
          diff_se = NA,
          diff_lower = NA,
          diff_upper = NA,
          diff_treatment_year = NA
        )
      )
    }
    
  }
  
}  # end of model fit and comparison function


####
####  DEFINE FUNCTION FOR CALCULATING SMOOTHER DIFFS ---------------------------
####

smooth_diff <- function(model, newdata, f1, f2, var, alpha = 0.05,
                        unconditional = FALSE) {
  # Calculates the difference between two fitted smooths from
  # a GAM model with a 'by' interaction term. This function written
  # by Gavin Simpson:
  # (https://www.fromthebottomoftheheap.net/2017/10/10/difference-splines-i/)
  # Modified slightly by Andrew Tredennick.
  #
  # Args:
  #   model: A fitted GAM model object. Must include an interaction smooth
  #    term via 's(by = xterm)'
  #  newdata: A dataframe with specific predictors for the model.
  #  f1: First factor variable for comparison (control id)
  #  f2: Second factor variable for comparison (treatment id)
  #  var: Name of the 'by' variable (e.g., column name that contains f1 and f2)
  #  alpha: Significance level. Default is 0.05.
  #  unconditional: Correct for uncertainty in paramter covariance (TRUE) or 
  #   or not (FALSE). Default is FALSE.
  #
  # Returns:
  #  A data frame with the difference, se, and 95% CIs for each of the time
  #  points in newdata.
  
  xp <- predict(model, newdata = newdata, type = 'lpmatrix', 
                exclude = "s(plot_id)")  # make prediction for avareage plot
  
  c1 <- grepl(f1, colnames(xp))
  c2 <- grepl(f2, colnames(xp))
  r1 <- newdata[[var]] == f1
  r2 <- newdata[[var]] == f2
  
  # difference rows of xp for data from comparison
  X <- xp[r1, ] - xp[r2, ]
  
  # zero out cols of X related to splines for other factors (redundant here)
  X[, ! (c1 | c2)] <- 0
  
  # zero out the parametric cols
  X[, !grepl('^s\\(', colnames(xp))] <- 0
  
  dif <- X %*% coef(model)
  se <- sqrt(rowSums((X %*% vcov(model, unconditional = unconditional)) * X))
  crit <- qt(alpha/2, df.residual(model), lower.tail = FALSE)
  upr <- dif + (crit * se)
  lwr <- dif - (crit * se)
  
  data.frame(treatment_year = unique(newdata$treatment_year),
             pair = paste(f1, f2, sep = '-'),
             diff = dif,
             se = se,
             upper = upr,
             lower = lwr)
}  # end of smooth_diff function



####
####  DEFINE FUNCTION TO FILL TIBBLE WHEN YEARS < 4 ----------------------------
####

fill_empties <- function(...){
  return(
    tibble(
      response_var = c(
        "richness_change_abs", 
        "evenness_change_abs", 
        "rank_change", 
        "gains", 
        "losses"
      ),
      p_value = NA,
      delta_deviance = NA,
      delta_aic = NA,
      diff = NA,
      diff_se = NA,
      diff_lower = NA,
      diff_upper = NA,
      diff_treatment_year = NA
    )
  )
}



####
####  READ IN DATA AND CALCULATE CUMULATIVE CHANGE -----------------------------
####
# change_metrics <- as_tibble(read.csv(paste0(data_dir, data_file))) %>%
#   dplyr::select(-X) 

change_metrics <- as_tibble(dat) %>%
  left_join(treatment_info)

##  Calculate cumulative sums of each metric (from Kevin)
change_cumsum <- change_metrics %>%
  filter(treatment_year!=0)%>%
  group_by(site_project_comm, treatment, plot_id) %>%
  mutate(richness_change_abs = abs(richness_change)) %>%
  mutate(evenness_change_abs = abs(evenness_change)) %>%
  mutate_at(vars(richness_change, 
                 richness_change_abs, 
                 evenness_change,
                 evenness_change_abs, 
                 rank_change, 
                 gains, 
                 losses), 
            list(cumsum)) %>%
  mutate(control = ifelse(plot_mani==0,"control","treatment")) %>%
  arrange(site_project_comm, plot_id, treatment_year) %>%
  ungroup()



####
####  LOOP OVER SITE_PROJECT_COMMS AND COMPARE CONTROLS VS. TREATMENTS ---------
####
all_sites <- unique(change_cumsum$site_project_comm)
all_comparisons <- {} # empty object for storage
diff_type <- "last_year"

for(do_site in all_sites){
  site_data <- filter(change_cumsum, site_project_comm == do_site)
  site_controls <- filter(site_data, plot_mani == 0)
  site_treatments <- filter(site_data, plot_mani != 0)
  all_treatments <- unique(site_treatments$treatment)
  
  for(do_treatment in all_treatments){
    treatment_data <- filter(site_treatments, treatment == do_treatment)
    model_data <- rbind(site_controls, treatment_data)
    model_data <- model_data %>%
      mutate(plot_id = as.factor(plot_id),
             treatment = as.factor(treatment))
    num_years <- length(unique(model_data$treatment_year))
    
    # Skip data with less than three pairs of years
    if(num_years < 4){
      tmp_out <- fill_empties() %>%
        mutate(
          site_proj_comm = do_site,
          treatment = do_treatment
        ) %>%
        dplyr::select(
          site_proj_comm,
          treatment,
          response_var,
          p_value,
          delta_deviance,
          delta_aic
        )
    }
    
    # Compare models for data with more than four pairs of years
    if(num_years > 3){
      
      model_data <- model_data %>%
        mutate(richness_change_abs = richness_change_abs + 0.001,
               evenness_change_abs = evenness_change_abs + 0.001,
               rank_change = rank_change + 0.001,
               gains = gains + 0.001,
               losses = losses + 0.001)
      
      # Richness
      rich_test <- fit_compare_gams(
        df = model_data,
        response = "richness_change_abs",
        diff_type
      )
      
      # Evenness
      even_test <- fit_compare_gams(
        df = model_data,
        response = "evenness_change_abs",
        diff_type
      )
      
      # Rank change
      rank_test <- fit_compare_gams(
        df = model_data,
        response = "rank_change",
        diff_type
      )
      
      # Gains
      gain_test <- fit_compare_gams(
        df = model_data,
        response = "gains",
        diff_type
      )
      
      # Losses
      loss_test <- fit_compare_gams(
        df = model_data,
        response = "losses",
        diff_type
      )
      
      # Compositional change
      # comp_test <- fit_compare_gams(
      #   df = model_data,
      #   response = "composition_change",
      #   diff_type
      # )
      
      tmp_out <- bind_rows(
        rich_test,
        even_test,
        rank_test,
        gain_test,
        loss_test
        # comp_test
      ) %>%
        mutate(
          site_proj_comm = do_site,
          treatment = do_treatment
        ) %>%
        dplyr::select(
          site_proj_comm,
          treatment,
          response_var,
          p_value,
          delta_deviance,
          delta_aic,
          diff,
          diff_se,
          diff_lower,
          diff_upper,
          diff_treatment_year
        )
      
    } # end if for num_years
    
    all_comparisons <- all_comparisons %>%
      bind_rows(tmp_out)
    
  } # end treatment loop
  
  print(paste("Done with site:", do_site))
  
} # end site loop



####
####  SAVE DELTA_AIC TABLE -----------------------------------------------------
####
save_comparisons <- all_comparisons %>%
  filter(is.na(delta_deviance) == FALSE) %>%  # remove sites we don't model
  mutate(
    sig_diff_cntrl_trt = ifelse(
      p_value <= 0.05 & sign(delta_deviance) == -1,
      "yes",
      "no"
    )
  ) %>%
  mutate(
    sig_diff_cntrl_trt = ifelse(is.na(sig_diff_cntrl_trt) == TRUE, "no", sig_diff_cntrl_trt)
  )

write.csv(save_comparisons, "C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\GamSigTable_norare.csv", row.names=F)
