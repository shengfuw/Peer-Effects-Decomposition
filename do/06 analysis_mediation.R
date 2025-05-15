# Author: Shengfu Wang
# The code was adapted with reference to: https://doi.org/10.5281/zenodo.7688984

library(haven)
library(dplyr)
library(survey)
memory.limit(size=20000)

workdata_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/workdata"
rawdata_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/rawdata"
output_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/output"
do_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/do"

standardize_vars <- function(data, vars) { data |> mutate(across(all_of(vars), ~ (. - mean(., na.rm = T)) / sd(., na.rm = T))) } 

compute_estimate <- function(i, beta_mat, var_mat, nmi, d = 5) {
  est <- numeric(4)
  est[1] <- round(mean(beta_mat[,i]), digits = d)
  est[2] <- round(sqrt(mean(var_mat[,i]) + (var(beta_mat[,i]) * (1 + (1/nmi)))), digits = d)
  est[3] <- round(est[1]/est[2], digits = d)
  est[4] <- round((pnorm(abs(est[3]), 0, 1, lower.tail = F) * 2), digits = d)
  
  return(est)
}

four_way_decompose = function(data_name, specified_vars, saved_data_add_name = "", nmediator = 2, nmi = 30, nboot = 1) {
  start <- Sys.time()
  
  # Set variables 
  treatment = specified_vars[1]
  outcome <- specified_vars[2]
  mediator1 <- specified_vars[3]
  mediator2 <- specified_vars[4]
  cv <- specified_vars[5]
  peercv <- specified_vars[6]
  post_cv1 <- specified_vars[7]
  post_cv2 <- specified_vars[8]
  
  mediator1_x_treatment <- "mediator1_x_treatment"
  mediator2_x_treatment <- "mediator2_x_treatment"
  
  
  # Import data
  file_path = file.path(workdata_path, paste0(data_name, ".dta"))
  analytic_sample_mi <- read_dta(file_path)
  analytic_sample_mi <- analytic_sample_mi[which(analytic_sample_mi$"_mi_m" != 0), ]
  
  
  # Standardize variables
  vars_to_standardize <- c(
    # Outcome
    outcome,
    # Mediator
    mediator1, mediator2, 
    # Treatment
    treatment, 
    # Pre-treatment confounders
    trimws(strsplit(cv, "\\+ ")[[1]]), trimws(strsplit(peercv, "\\+")[[1]])
    # Post-treatment confounders: not standardized?
    # post_cv1, post_cv2
  )
  analytic_sample_mi <- standardize_vars(analytic_sample_mi, vars_to_standardize)
  
  # Initialize matrices to store beta/variance values
  mibeta_rate <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mivar_rate <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mibeta_rnde <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mivar_rnde <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mibeta_cde <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mivar_cde <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mibeta_rintref <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mivar_rintref <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mibeta_rnie <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mivar_rnie <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mibeta_rpie <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mivar_rpie <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mibeta_rintmed <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  mivar_rintmed <- matrix(data = NA, nrow = nmi, ncol = nmediator)
  
  mibeta_mediator_on_treatment <- matrix(data = NA, nrow = nmi, ncol = 4)
  mivar_mediator_on_treatment <- matrix(data = NA, nrow = nmi, ncol = 4)
  
  mibeta_treatment <- matrix(data = NA, nrow = nmi, ncol = 2)
  mivar_treatment <- matrix(data = NA, nrow = nmi, ncol = 2)
  mibeta_mediator <-matrix(data = NA, nrow = nmi, ncol = 2)
  mivar_mediator <-matrix(data = NA, nrow = nmi, ncol = 2)
  mibeta_mediator_x_treatment <- matrix(data = NA, nrow = nmi, ncol = 2)
  mivar_mediator_x_treatment <- matrix(data = NA, nrow = nmi, ncol = 2)
  mibeta_post_cv <- matrix(data = NA, nrow = nmi, ncol = 2)
  mivar_post_cv <- matrix(data = NA, nrow = nmi, ncol = 2)
  
  # Calculate estimates and variances
  for (i in 1:nmi) {
    cat(i, "...", sep = "")
    analytic_sample <- analytic_sample_mi[which(analytic_sample_mi$"_mi_m" == i), ]

    # Add variables of mediator-by-treatment interaction
    analytic_sample$mediator1_x_treatment <- analytic_sample[[mediator1]] * analytic_sample[[treatment]]
    analytic_sample$mediator2_x_treatment <- analytic_sample[[mediator2]] * analytic_sample[[treatment]]

    design <- svydesign(data = analytic_sample, ids = ~scid, weights = ~gswgt2)
    
    # Percentile calculations
    pc_treatment <- quantile(analytic_sample[[treatment]], probs=c(0.2, 0.8), na.rm=T)
    pc_mediator1 <- quantile(analytic_sample[[mediator1]], probs=c(0.25, 0.5, 0.75), na.rm=T)
    pc_mediator2 <- quantile(analytic_sample[[mediator2]], probs=c(0.25, 0.5, 0.75), na.rm=T)
    m_mediator1 <- pc_mediator1[2]
    m_mediator2 <- pc_mediator2[2]

    
    # Fit Model 1: Mediator on treatment
    mediator1_on_treatment <- svyglm(as.formula(paste(mediator1, "~", treatment, "+", cv, "+", peercv)), data = analytic_sample, design = design)
    mediator2_on_treatment <- svyglm(as.formula(paste(mediator2, "~", treatment, "+", cv, "+", peercv)), data = analytic_sample, design = design)

    # RWR calculations for post-treatment confounder
    post_cv1_on_treatment <- svyglm(as.formula(paste(post_cv1, "~", treatment, "+", cv, "+", peercv)), data = analytic_sample, design = design)
    post_cv2_on_treatment <- svyglm(as.formula(paste(post_cv2, "~", treatment, "+", cv, "+", peercv)), data = analytic_sample, design = design)
  
    analytic_sample$post_cv1_r <- residuals(post_cv1_on_treatment)
    analytic_sample$post_cv2_r <- residuals(post_cv2_on_treatment)
    if (min(analytic_sample$post_cv1_r) == 0 & max(analytic_sample$post_cv1_r) == 0) {
      analytic_sample$post_cv1_r <- post_cv1_on_treatment$residuals
    }
    if (min(analytic_sample$post_cv2_r) == 0 & max(analytic_sample$post_cv2_r) == 0) {
      analytic_sample$post_cv2_r <- post_cv2_on_treatment$residuals
    }
    
    design <- svydesign(data = analytic_sample, ids = ~scid, weights = ~gswgt2)
    
    # Fit Model 2: Outcome on treatment, mediator, treatment x mediator
    outcome_model_mediator1 <- svyglm(as.formula(paste(outcome, "~", treatment, "+", mediator1, "+", mediator1_x_treatment, "+", cv, "+", peercv, "+", "post_cv1_r")), data = analytic_sample, design = design)
    outcome_model_mediator2 <- svyglm(as.formula(paste(outcome, "~", treatment, "+", mediator2, "+", mediator2_x_treatment, "+", cv, "+", peercv, "+", "post_cv2_r")), data = analytic_sample, design = design)
    
    # Compute effects
    mibeta_cde[i, 1] <- (outcome_model_mediator1$coef[treatment] + outcome_model_mediator1$coef[mediator1_x_treatment] * m_mediator1) * (pc_treatment[2] - pc_treatment[1])
    mibeta_cde[i, 2] <- (outcome_model_mediator2$coef[treatment] + outcome_model_mediator2$coef[mediator2_x_treatment] * m_mediator2) * (pc_treatment[2] - pc_treatment[1])
    
    mibeta_rintref[i, 1] <- outcome_model_mediator1$coef[mediator1_x_treatment] * (mediator1_on_treatment$coef[1] + mediator1_on_treatment$coef[2] * pc_treatment[1] - m_mediator1) * (pc_treatment[2] - pc_treatment[1])
    mibeta_rintref[i, 2] <- outcome_model_mediator2$coef[mediator2_x_treatment] * (mediator2_on_treatment$coef[1] + mediator2_on_treatment$coef[2] * pc_treatment[1] - m_mediator2) * (pc_treatment[2] - pc_treatment[1])
    
    mibeta_rnde[i, 1] <- mibeta_cde[i, 1] + mibeta_rintref[i, 1]
    mibeta_rnde[i, 2] <- mibeta_cde[i, 2] + mibeta_rintref[i, 2]
    
    mibeta_rpie[i, 1] <- (mediator1_on_treatment$coef[treatment] * outcome_model_mediator1$coef[mediator1] + mediator1_on_treatment$coef[treatment] * outcome_model_mediator1$coef[mediator1_x_treatment] * pc_treatment[1]) * (pc_treatment[2]- pc_treatment[1])
    mibeta_rpie[i, 2] <- (mediator2_on_treatment$coef[treatment] * outcome_model_mediator2$coef[mediator2] + mediator2_on_treatment$coef[treatment] * outcome_model_mediator2$coef[mediator2_x_treatment] * pc_treatment[1]) * (pc_treatment[2]- pc_treatment[1])
    
    mibeta_rintmed[i, 1] <- mediator1_on_treatment$coef[treatment] * outcome_model_mediator1$coef[mediator1_x_treatment] * (pc_treatment[2]- pc_treatment[1])^2
    mibeta_rintmed[i, 2] <- mediator2_on_treatment$coef[treatment] * outcome_model_mediator2$coef[mediator2_x_treatment] * (pc_treatment[2]- pc_treatment[1])^2
    
    mibeta_rnie[i, 1] <- mibeta_rpie[i, 1] + mibeta_rintmed[i, 1]
    mibeta_rnie[i, 2] <- mibeta_rpie[i, 2] + mibeta_rintmed[i, 2]
    
    mibeta_rate[i, 1] <- mibeta_rnde[i, 1] + mibeta_rnie[i, 1]
    mibeta_rate[i, 2] <- mibeta_rnde[i, 2] + mibeta_rnie[i, 2]
    
    mibeta_mediator_on_treatment[i, 1] <- mediator1_on_treatment$coef[treatment]
    mibeta_mediator_on_treatment[i, 2] <- mediator2_on_treatment$coef[treatment]
    mibeta_mediator_on_treatment[i, 3] <- post_cv1_on_treatment$coef[treatment]
    mibeta_mediator_on_treatment[i, 4] <- post_cv2_on_treatment$coef[treatment]
    
    
    mibeta_treatment[i, 1] <- outcome_model_mediator1$coef[treatment]
    mibeta_treatment[i, 2] <- outcome_model_mediator2$coef[treatment]
    
    mibeta_mediator[i, 1] <- outcome_model_mediator1$coef[mediator1]
    mibeta_mediator[i, 2] <- outcome_model_mediator2$coef[mediator2]
    
    mibeta_mediator_x_treatment[i,1] <- outcome_model_mediator1$coef[mediator1_x_treatment]
    mibeta_mediator_x_treatment[i,2] <- outcome_model_mediator2$coef[mediator2_x_treatment]
    
    mibeta_post_cv[i,1] <- outcome_model_mediator1$coef["post_cv1_r"]
    mibeta_post_cv[i,2] <- outcome_model_mediator2$coef["post_cv2_r"]
    
    # Compute block bootstrap SEs
    set.seed(123)
    bootdist_rate <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    bootdist_rnde <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    bootdist_cde <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    bootdist_rintref <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    bootdist_rnie <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    bootdist_rpie <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    bootdist_rintmed <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    bootdist_mediator_on_treatment <- matrix(data = NA, nrow = nboot, ncol = 4)
    bootdist_treatment <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    bootdist_mediator <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    bootdist_mediator_x_treatment <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    bootdist_post_cv <- matrix(data = NA, nrow = nboot, ncol = nmediator)
    
    for (j in 1:nboot) {
      idboot.1 <- sample(unique(analytic_sample$aid), replace=T)
      idboot.2 <- table(idboot.1)
      analytic_sample.boot <- NULL
      
      for (k in 1:max(idboot.2)) {
        boot.data <- analytic_sample[analytic_sample$aid %in% names(idboot.2[idboot.2 %in% k]), ]
        
        for (l in 1:k) {
          analytic_sample.boot <- rbind(analytic_sample.boot, boot.data)
        }
      }
      
      # Add variables of mediator-by-treatment interaction
      analytic_sample.boot$mediator1_x_treatment <- analytic_sample.boot[[mediator1]] * analytic_sample.boot[[treatment]]
      analytic_sample.boot$mediator2_x_treatment <- analytic_sample.boot[[mediator2]] * analytic_sample.boot[[treatment]]
      
      design <- svydesign(data = analytic_sample.boot, ids = ~scid, weights = ~gswgt2)
      
      # Fit Model 1: Mediator on treatment
      # mediator1_on_treatment.boot <- svyglm(as.formula(paste(mediator1, "~", treatment, "+", cv, "+", peercv)), data = analytic_sample.boot, design = design)
      # mediator2_on_treatment.boot <- svyglm(as.formula(paste(mediator2, "~", treatment, "+", cv, "+", peercv)), data = analytic_sample.boot, design = design)
      
      mediator1_on_treatment.boot <- svyglm(as.formula(paste(mediator1, "~", treatment, "+", sub("scid \\+", "", cv), "+", peercv)), data = analytic_sample.boot, design = design)
      mediator2_on_treatment.boot <- svyglm(as.formula(paste(mediator2, "~", treatment, "+", sub("scid \\+", "", cv), "+", peercv)), data = analytic_sample.boot, design = design)
      
      # RWR calculations for post-treatment confounder
      # post_cv1_on_treatment.boot <- svyglm(as.formula(paste(post_cv1, "~", treatment, "+", cv, "+", peercv)), data = analytic_sample.boot, design = design)
      # post_cv2_on_treatment.boot <- svyglm(as.formula(paste(post_cv2, "~", treatment, "+", cv, "+", peercv)), data = analytic_sample.boot, design = design)
      post_cv1_on_treatment.boot <- svyglm(as.formula(paste(post_cv1, "~", treatment, "+", sub("scid \\+", "", cv), "+", peercv)), data = analytic_sample.boot, design = design)
      post_cv2_on_treatment.boot <- svyglm(as.formula(paste(post_cv2, "~", treatment, "+", sub("scid \\+", "", cv), "+", peercv)), data = analytic_sample.boot, design = design)
      
      analytic_sample.boot$post_cv1_r <- residuals(post_cv1_on_treatment.boot)
      analytic_sample.boot$post_cv2_r <- residuals(post_cv2_on_treatment.boot)
      design <- svydesign(data = analytic_sample.boot, ids = ~scid, weights = ~gswgt2)
      
      # Fit Model 2: Outcome on treatment, mediator, treatment x mediator
      outcome_model_mediator1.boot <- svyglm(as.formula(paste(outcome, "~", treatment, "+", mediator1, "+", mediator1_x_treatment, "+", cv, "+", peercv, "+", "post_cv1_r")), data = analytic_sample.boot, design = design)
      outcome_model_mediator2.boot <- svyglm(as.formula(paste(outcome, "~", treatment, "+", mediator2, "+", mediator2_x_treatment, "+", cv, "+", peercv, "+", "post_cv2_r")), data = analytic_sample.boot, design = design)
      
      bootdist_cde[j, 1] <- (outcome_model_mediator1.boot$coef[treatment] + outcome_model_mediator1.boot$coef[mediator1_x_treatment] * m_mediator1) * (pc_treatment[2] - pc_treatment[1])
      bootdist_cde[j, 2] <- (outcome_model_mediator2.boot$coef[treatment] + outcome_model_mediator2.boot$coef[mediator2_x_treatment] * m_mediator2) * (pc_treatment[2] - pc_treatment[1])
      
      bootdist_rintref[j, 1] <- outcome_model_mediator1.boot$coef[mediator1_x_treatment] * (mediator1_on_treatment.boot$coef[1] + mediator1_on_treatment.boot$coef[2] * pc_treatment[1] - m_mediator1) * (pc_treatment[2] - pc_treatment[1])
      bootdist_rintref[j, 2] <- outcome_model_mediator2.boot$coef[mediator2_x_treatment] * (mediator2_on_treatment.boot$coef[1] + mediator2_on_treatment.boot$coef[2] * pc_treatment[1] - m_mediator2) * (pc_treatment[2] - pc_treatment[1])
      
      bootdist_rnde[j, 1] <- bootdist_cde[j, 1] + bootdist_rintref[j, 1]
      bootdist_rnde[j, 2] <- bootdist_cde[j, 2] + bootdist_rintref[j, 2]
      
      bootdist_rpie[j, 1] <- (mediator1_on_treatment.boot$coef[treatment] * outcome_model_mediator1.boot$coef[mediator1] + mediator1_on_treatment.boot$coef[treatment] * outcome_model_mediator1.boot$coef[mediator1_x_treatment] * pc_treatment[1]) * (pc_treatment[2]- pc_treatment[1])
      bootdist_rpie[j, 2] <- (mediator2_on_treatment.boot$coef[treatment] * outcome_model_mediator2.boot$coef[mediator2] + mediator2_on_treatment.boot$coef[treatment] * outcome_model_mediator2.boot$coef[mediator2_x_treatment] * pc_treatment[1]) * (pc_treatment[2]- pc_treatment[1])
      
      bootdist_rintmed[j, 1] <- mediator1_on_treatment.boot$coef[treatment] * outcome_model_mediator1.boot$coef[mediator1_x_treatment] * (pc_treatment[2]- pc_treatment[1])^2
      bootdist_rintmed[j, 2] <- mediator2_on_treatment.boot$coef[treatment] * outcome_model_mediator2.boot$coef[mediator2_x_treatment] * (pc_treatment[2]- pc_treatment[1])^2
      
      bootdist_rnie[j, 1] <- bootdist_rpie[j, 1] + bootdist_rintmed[j, 1]
      bootdist_rnie[j, 2] <- bootdist_rpie[j, 2] + bootdist_rintmed[j, 2]
      
      bootdist_rate[j, 1] <- bootdist_rnde[j, 1] + bootdist_rnie[j, 1]
      bootdist_rate[j, 2] <- bootdist_rnde[j, 2] + bootdist_rnie[j, 2]
      
        
      bootdist_mediator_on_treatment[j, 1] <- mediator1_on_treatment.boot$coef[treatment]
      bootdist_mediator_on_treatment[j, 2] <- mediator2_on_treatment.boot$coef[treatment]
      bootdist_mediator_on_treatment[j, 3] <- post_cv1_on_treatment.boot$coef[treatment]
      bootdist_mediator_on_treatment[j, 4] <- post_cv2_on_treatment.boot$coef[treatment]
      
      
      bootdist_treatment[j, 1] <- outcome_model_mediator1.boot$coef[treatment]
      bootdist_treatment[j, 2] <- outcome_model_mediator2.boot$coef[treatment]
      
      bootdist_mediator[j, 1] <- outcome_model_mediator1.boot$coef[mediator1]
      bootdist_mediator[j, 2] <- outcome_model_mediator2.boot$coef[mediator2]
      
      bootdist_mediator_x_treatment[j, 1] <- outcome_model_mediator1.boot$coef[mediator1_x_treatment]
      bootdist_mediator_x_treatment[j, 2] <- outcome_model_mediator2.boot$coef[mediator2_x_treatment]
      
      bootdist_post_cv[j, 1] <- outcome_model_mediator1.boot$coef["post_cv1_r"]
      bootdist_post_cv[j, 2] <- outcome_model_mediator2.boot$coef["post_cv2_r"]
      
    }
    
    for (m in 1:2) { # 2: number of mediator
      mivar_rate[i,m] <- var(bootdist_rate[,m])
      mivar_rnde[i,m] <- var(bootdist_rnde[,m])
      mivar_cde[i,m] <- var(bootdist_cde[,m])
      mivar_rintref[i,m] <- var(bootdist_rintref[,m])
      mivar_rnie[i,m] <- var(bootdist_rnie[,m])
      mivar_rpie[i,m] <- var(bootdist_rpie[,m])
      mivar_rintmed[i,m] <- var(bootdist_rintmed[,m])
      
      mivar_treatment[i,m] <- var(bootdist_treatment[,m])
      mivar_mediator[i,m] <- var(bootdist_mediator[,m])
      mivar_mediator_x_treatment[i,m] <- var(bootdist_mediator_x_treatment[,m])
      mivar_post_cv[i,m] <- var(bootdist_post_cv[,m])
    }
    
    for (m in 1:4) { # 4: number of mediator + number of post-treatment cv
      mivar_mediator_on_treatment[i,m] <- var(bootdist_mediator_on_treatment[,m])
    }
    
    
    
  }
  
  # Combine MI estimates
  rate_est <- matrix(data = NA, nrow = nmediator, ncol = 4)
  rnde_est <- matrix(data = NA, nrow = nmediator, ncol = 4)
  cde_est <-matrix(data = NA, nrow = nmediator, ncol= 4)
  rintref_est <- matrix(data = NA, nrow = nmediator, ncol = 4)
  rnie_est <- matrix(data = NA, nrow = nmediator, ncol = 4)
  rpie_est <- matrix(data = NA, nrow = nmediator, ncol = 4)
  rintmed_est <- matrix(data = NA, nrow = nmediator, ncol = 4)
  
  mediator_on_treatment_est <- matrix(data = NA, nrow = 4, ncol = 4)
  
  treatment_est <- matrix(data = NA, nrow = nmediator, ncol = 4)
  mediator_est <- matrix(data = NA, nrow = nmediator, ncol = 4)
  mediator_x_treatment_est <- matrix(data = NA, nrow = nmediator, ncol = 4)
  post_cv_est <- matrix(data = NA, nrow = nmediator, ncol = 4)
    
  for (i in 1:nmediator) {
    rate_est[i, ] <- compute_estimate(i, mibeta_rate, mivar_rate, nmi)
    rnde_est[i, ] <- compute_estimate(i, mibeta_rnde, mivar_rnde, nmi)
    cde_est[i, ] <- compute_estimate(i, mibeta_cde, mivar_cde, nmi)
    rintref_est[i, ] <- compute_estimate(i, mibeta_rintref, mivar_rintref, nmi)
    rnie_est[i, ] <- compute_estimate(i, mibeta_rnie, mivar_rnie, nmi)
    rpie_est[i, ] <- compute_estimate(i, mibeta_rpie, mivar_rpie, nmi)
    rintmed_est[i, ] <- compute_estimate(i, mibeta_rintmed, mivar_rintmed, nmi)
    
    treatment_est[i, ] <- compute_estimate(i, mibeta_treatment, mivar_treatment, nmi)
    mediator_est[i, ] <- compute_estimate(i, mibeta_mediator, mivar_mediator, nmi)
    mediator_x_treatment_est[i, ] <- compute_estimate(i, mibeta_mediator_x_treatment, mivar_mediator_x_treatment, nmi)
    post_cv_est[i, ] <- compute_estimate(i, mibeta_post_cv, mivar_post_cv, nmi)
  }
  
  for (i in 1:4) {
    mediator_on_treatment_est[i, ] <- compute_estimate(i, mibeta_mediator_on_treatment, mivar_mediator_on_treatment, nmi)
  }
  
  
  # Store results
  sink( file.path(output_path, paste0("Table_mediation_", data_name, saved_data_add_name, "_", format(Sys.time(), "%m%d%H"), ".txt")))
  cat("====================================================\n")
  cat("Four-Way Decomposition of Mediation Effects:\n")
  cat("RATE\n")
  print(rate_est[1:2,])
  cat("\n", "RNDE\n")
  print(rnde_est[1:2,])
  cat("\n", "CDE(0)\n")
  print(cde_est[1:2,])
  cat("\n", "RINT_ref\n")
  print(rintref_est[1:2,])
  cat("\n", "RNIE\n")
  print(rnie_est[1:2,])
  cat("\n", "RPIE\n")
  print(rpie_est[1:2,])
  cat("\n", "RINT_med\n")
  print(rintmed_est[1:2,])
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("Note: Table Columes = Est / SE / Z / P-value\n\n")
  cat("nmi = ", nmi, "\n")
  cat("Data: ", data_name, "\n")
  cat("Observation = ", nrow(analytic_sample), "\n")
  cat("Variables: [1] Treatment | [2] Outcome | [3] Mediator 1 | [4] Mediator 2 | [5] Controls | [6] Peer-level controls | [7] Post-treatment controls 1 | [8] Post-treatment controls 2:", "\n")
  print(specified_vars)
  
  cat("====================================================\n")
  cat("Treatment on Mediators and Post-treatment Confounders:\n")
  cat("Mediator 1\n")
  print(mediator_on_treatment_est[1,])
  cat("\n", "Mediator 2\n")
  print(mediator_on_treatment_est[2,])
  cat("\n", "Post CV 1\n")
  print(mediator_on_treatment_est[3,])
  cat("\n", "Post CV 2\n")
  print(mediator_on_treatment_est[4,])
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("Note: Table Columes = Est / SE / Z / P-value\n\n")

  cat("====================================================\n")
  cat("Outcome Model:\n")
  cat("Treatment\n")
  print(treatment_est[1:2,])
  cat("\n", "Mediator\n")
  print(mediator_est[1:2,])
  cat("\n", "Mediator x Treatment\n")
  print(mediator_x_treatment_est[1:2,])
  cat("\n", "Post CV\n")
  print(post_cv_est[1:2,])
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("Note: Table Columes = Est / SE / Z / P-value\n")
  
  sink()
  
  # End
  end <- Sys.time()
  elapsed <- as.numeric(difftime(end, start, units = "secs"))
  h <- floor(elapsed / 3600)
  m <- floor((elapsed %% 3600) / 60)
  s <- round(elapsed %% 60)
  cat("Done! ", "\n", sprintf("Execution time: %02d:%02d:%02d", h, m, s))
}

setwd(workdata_path)
dir()

############## Main anlysis (set data and variables here!) ###############
treatment <- "peer_treatment_mean"
outcome <- "gpa_overall_2"
mediator1 <- "educexp_want_1"
mediator2 <- "educexp_likely_1"
cv <- "grade_8 + grade_9 + grade_10 + grade_11 + grade_12 + race_2 + race_3 + race_4 + immig_1 + immig_2 + family_2 + family_3 + family_4 + region_2 + region_3 + region_4 + pa_educ + assistance + sibsize + pvt + treatment_0 + n_peer + p_attachment_1 + s_attachment + family_income_ln_1 + fcesd_1 + club_n + skip_school_1 + hang_out_own_decision_1 + net_esrden + net_bcent10x + net_reach_ln"
peercv <- "female_peermean + black_peermean + hispanic_peermean + other_peermean + immig_1st_peermean + immig_2nd_peermean + family_one_peermean + family_other_peermean + pa_educ_peermean"
post_cv1 <- "educexp_want_1_peermean" 
post_cv2 <- "educexp_likely_1_peermean"
#|##########################################################################

specified_vars <- c(treatment, outcome, mediator1, mediator2, cv, peercv, post_cv1, post_cv2)
set.seed(123)


# Peer type 1: Main analysis
data_name <- "sample_mi_friend_all"
four_way_decompose(data_name, specified_vars)

# Peer type 2
data_name <- "sample_mi_grade"
four_way_decompose(data_name, specified_vars)

# Peer type 3
data_name <- "sample_mi_coursemate"
four_way_decompose(data_name, specified_vars)

# Peer type 4
data_name <- "sample_mi_club"
four_way_decompose(data_name, specified_vars)

# Peer type 5
data_name <- "sample_mi_friend_in"
four_way_decompose(data_name, specified_vars)

# Peer type 6
data_name <- "sample_mi_friend_out"
four_way_decompose(data_name, specified_vars)

# Peer type 7
data_name <- "sample_mi_friend_both"
four_way_decompose(data_name, specified_vars)



############## Testing mediator measured at Wave II ##############
treatment <- "peer_treatment_mean"
outcome <- "gpa_overall_2"
mediator1 <- "educexp_want_2"
mediator2 <- "educexp_likely_2"
cv <- "grade_8 + grade_9 + grade_10 + grade_11 + grade_12 + race_2 + race_3 + race_4 + immig_1 + immig_2 + family_2 + family_3 + family_4 + region_2 + region_3 + region_4 + pa_educ + assistance + sibsize + pvt + treatment_0 + n_peer + p_attachment_1 + s_attachment + family_income_ln_1 + fcesd_1 + club_n + skip_school_1 + hang_out_own_decision_1 + net_esrden + net_bcent10x + net_reach_ln"
peercv <- "female_peermean + black_peermean + hispanic_peermean + other_peermean + immig_1st_peermean + immig_2nd_peermean + family_one_peermean + family_other_peermean + pa_educ_peermean"
post_cv1 <- "educexp_want_1_peermean" 
post_cv2 <- "educexp_likely_1_peermean"
#|#############################################################

specified_vars <- c(treatment, outcome, mediator1, mediator2, cv, peercv, post_cv1, post_cv2)
set.seed(123)

data_name <- "sample_mi_friend_all_MW2"
four_way_decompose(data_name, specified_vars)


############## Distributional effect: q90 ##############
treatment <- "peer_treatment_q90"
outcome <- "gpa_overall_2"
mediator1 <- "educexp_want_1"
mediator2 <- "educexp_likely_1"
cv <- "grade_8 + grade_9 + grade_10 + grade_11 + grade_12 + race_2 + race_3 + race_4 + immig_1 + immig_2 + family_2 + family_3 + family_4 + region_2 + region_3 + region_4 + pa_educ + assistance + sibsize + pvt + treatment_0 + n_peer + p_attachment_1 + s_attachment + family_income_ln_1 + fcesd_1 + club_n + skip_school_1 + hang_out_own_decision_1 + net_esrden + net_bcent10x + net_reach_ln"
peercv <- "female_peermean + black_peermean + hispanic_peermean + other_peermean + immig_1st_peermean + immig_2nd_peermean + family_one_peermean + family_other_peermean + pa_educ_peermean"
post_cv1 <- "educexp_want_1_peermean" 
post_cv2 <- "educexp_likely_1_peermean"
#|############################################################

specified_vars <- c(treatment, outcome, mediator1, mediator2, cv, peercv, post_cv1, post_cv2)
set.seed(123)

data_name <- "sample_mi_friend_all_q90"
four_way_decompose(data_name, specified_vars)


############## Distributional effect: q10 ##############
treatment <- "peer_treatment_q10"
outcome <- "gpa_overall_2"
mediator1 <- "educexp_want_1"
mediator2 <- "educexp_likely_1"
cv <- "grade_8 + grade_9 + grade_10 + grade_11 + grade_12 + race_2 + race_3 + race_4 + immig_1 + immig_2 + family_2 + family_3 + family_4 + region_2 + region_3 + region_4 + pa_educ + assistance + sibsize + pvt + treatment_0 + n_peer + p_attachment_1 + s_attachment + family_income_ln_1 + fcesd_1 + club_n + skip_school_1 + hang_out_own_decision_1 + net_esrden + net_bcent10x + net_reach_ln"
peercv <- "female_peermean + black_peermean + hispanic_peermean + other_peermean + immig_1st_peermean + immig_2nd_peermean + family_one_peermean + family_other_peermean + pa_educ_peermean"
post_cv1 <- "educexp_want_1_peermean" 
post_cv2 <- "educexp_likely_1_peermean"
#|############################################################

specified_vars <- c(treatment, outcome, mediator1, mediator2, cv, peercv, post_cv1, post_cv2)
set.seed(123)

data_name <- "sample_mi_friend_all_q10"
four_way_decompose(data_name, specified_vars)


############## Adding school fixed-effect ##############
treatment <- "peer_treatment_mean"
outcome <- "gpa_overall_2"
mediator1 <- "educexp_want_1"
mediator2 <- "educexp_likely_1"
sch_FE <- paste(paste0("schid_", seq(2, 124)), collapse = " + ") 
cv <- "grade_8 + grade_9 + grade_10 + grade_11 + grade_12 + race_2 + race_3 + race_4 + immig_1 + immig_2 + family_2 + family_3 + family_4 + region_2 + region_3 + region_4 + pa_educ + assistance + sibsize + pvt + treatment_0 + n_peer + p_attachment_1 + s_attachment + family_income_ln_1 + fcesd_1 + club_n + skip_school_1 + hang_out_own_decision_1 + net_esrden + net_bcent10x + net_reach_ln"
cv <- paste(sch_FE, "+", cv) 
peercv <- "female_peermean + black_peermean + hispanic_peermean + other_peermean + immig_1st_peermean + immig_2nd_peermean + family_one_peermean + family_other_peermean + pa_educ_peermean"
post_cv1 <- "educexp_want_1_peermean" 
post_cv2 <- "educexp_likely_1_peermean"
#|############################################################

specified_vars <- c(treatment, outcome, mediator1, mediator2, cv, peercv, post_cv1, post_cv2)
set.seed(123)

data_name <- "sample_mi_friend_all"
four_way_decompose(data_name, specified_vars, saved_data_add_name = "_FE")

