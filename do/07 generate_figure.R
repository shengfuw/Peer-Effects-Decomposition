
library(haven)
library(dplyr)
library(survey)
library(splines)
library(ggplot2)
library(gridExtra)

workdata_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/workdata"
rawdata_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/rawdata"
output_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/output"
do_path = "P:/AddHealth/Contract/24082201-Lin,MJ/Work/SF - Decomposing Peer Effects/do"

nmi <- 30

treatment <- "peer_treatment_mean"
outcome <- "gpa_overall_2"
mediator1 <- "educexp_want_1"
mediator2 <- "educexp_likely_1"
vars <- c(treatment, outcome, mediator1, mediator2, cv, "_mi_m", "scid", "gswgt2")

data_name <- "sample_mi_friend_all_for_figure"
file_path = file.path(workdata_path, paste0(data_name, ".dta"))
analytic_sample_mi <- read_dta(file_path)
analytic_sample_mi <- analytic_sample_mi[which(analytic_sample_mi$"_mi_m" != 0), vars]

standardize_vars <- function(data, vars) { data |> mutate(across(all_of(vars), ~ (. - mean(., na.rm = T)) / sd(., na.rm = T))) } 
# analytic_sample_mi <- standardize_vars(analytic_sample_mi, c(treatment, mediator1, mediator2))


# Figure 1: treatment on mediators and outcome
granularity <- 50
treat_range <- range(analytic_sample_mi[[treatment]], na.rm = T)
treat_vals <- seq(from = treat_range[1], to = treat_range[2], length = granularity)

mediator1_range <- range(analytic_sample_mi[[mediator1]], na.rm = T)
mediator1_vals <- seq(from = mediator1_range[1], to = mediator1_range[2], length = granularity)

mediator2_range <- range(analytic_sample_mi[[mediator2]], na.rm = T)
mediator2_vals <- seq(from = mediator2_range[1], to = mediator2_range[2], length = granularity)

mediator1_pe_m <- matrix(data = NA, nrow = nmi, ncol = granularity)
mediator2_pe_m <- matrix(data = NA, nrow = nmi, ncol = granularity)
mediator1_se_m <- matrix(data = NA, nrow = nmi, ncol = granularity)
mediator2_se_m <- matrix(data = NA, nrow = nmi, ncol = granularity)
treatment_pe_m <- matrix(data = NA, nrow = nmi, ncol = granularity)
treatment_se_m <- matrix(data = NA, nrow = nmi, ncol = granularity)

for (i in 1:nmi) {
  analytic_sample <- analytic_sample_mi[which(analytic_sample_mi$"_mi_m" == i), ]
  design <- svydesign(data = analytic_sample, ids = ~scid, weights = ~gswgt2)
  
  mediator1_lm <- svyglm(as.formula(paste(outcome, "~ bs(", mediator1, ", df=2)")), data = analytic_sample, design = design)
  mediator2_lm <- svyglm(as.formula(paste(outcome, "~ bs(", mediator2, ", df=2)")), data = analytic_sample, design = design)
  outcome_lm <- svyglm(as.formula(paste(outcome, "~ bs(", treatment, ", df=3)")), data = analytic_sample, design = design)
  
  newdata_mediator1 <- data.frame(educexp_want_1 = mediator1_vals)
  newdata_mediator2 <- data.frame(educexp_likely_1 = mediator2_vals)
  newdata_treatment <- data.frame(peer_treatment_mean = treat_vals)
  
  mediator1_ds <- data.frame(predict(mediator1_lm, newdata_mediator1))
  mediator2_ds <- data.frame(predict(mediator2_lm, newdata_mediator2))
  treatment_ds <- data.frame(predict(outcome_lm, newdata_treatment))
  
  mediator1_pe <- mediator1_ds$link
  mediator2_pe <- mediator2_ds$link
  treatment_pe <- treatment_ds$link
  
  mediator1_se <- mediator1_ds$SE
  mediator2_se <- mediator2_ds$SE
  treatment_se <- treatment_ds$SE
  
  mediator1_pe_m[i, ] <- mediator1_pe
  mediator2_pe_m[i, ] <- mediator2_pe
  treatment_pe_m[i, ] <- treatment_pe
  
  
  mediator1_se_m[i, ] <- mediator1_se
  mediator2_se_m[i, ] <- mediator2_se
  treatment_se_m[i, ] <- treatment_se
}

mediator1_pe_combined <- mediator1_pe_m %>% colSums() / nmi
mediator2_pe_combined <- mediator2_pe_m %>% colSums() / nmi
treatment_pe_combined <- treatment_pe_m %>% colSums() / nmi

mediator1_se_part1 <- mediator1_se_m^2 %>% colSums() / nmi
mediator2_se_part1 <- mediator2_se_m^2 %>% colSums() / nmi
treatment_se_part1 <- treatment_se_m^2 %>% colSums() / nmi

mediator1_se_part2 <- apply(mediator1_se_m, 2, var)
mediator2_se_part2 <- apply(mediator2_se_m, 2, var)
treatment_se_part2 <- apply(treatment_se_m, 2, var)

mediator1_se_combined <- sqrt(mediator1_se_part1 + mediator1_se_part2)
mediator2_se_combined <- sqrt(mediator2_se_part1 + mediator2_se_part2)
treatment_se_combined <- sqrt(treatment_se_part1 + treatment_se_part2)

mediator1_lower <- mediator1_pe_combined - 1.96 * mediator1_se_combined
mediator2_lower <- mediator2_pe_combined - 1.96 * mediator2_se_combined
treatment_lower <- treatment_pe_combined - 1.96 * treatment_se_combined

mediator1_upper <- mediator1_pe_combined + 1.96 * mediator1_se_combined
mediator2_upper <- mediator2_pe_combined + 1.96 * mediator2_se_combined
treatment_upper <- treatment_pe_combined + 1.96 * treatment_se_combined

file_path = file.path(output_path, paste0('figure-1', ".tiff"))
tiff(file_path, width = 12, height = 9, units = 'in', res = 800)
data <- data.frame(mediator1_pe_combined = mediator1_pe_combined, mediator1_lower = mediator1_lower, mediator1_upper = mediator1_upper,
                   mediator2_pe_combined = mediator2_pe_combined, mediator2_lower = mediator2_lower, mediator2_upper = mediator2_upper,
                   treatment_pe_combined = treatment_pe_combined, treatment_lower = treatment_lower, outcome_uppper = treatment_upper,
                   mediator1_vals = mediator1_vals, mediator2_vals = mediator2_vals, treat_vals = treat_vals)
View(data)


figure1_mediator1 <- ggplot(data = data, aes(x = mediator1_vals, y = mediator1_pe_combined)) +
  geom_line() +
  geom_ribbon(aes(ymin = mediator1_lower, ymax = mediator1_upper), alpha = .25) +
  geom_hline(yintercept = mean(analytic_sample_mi$gpa_overall_2), linetype = "dashed") +
  xlab("Mediator 1") +
  ylab("Outcome") +
  labs("") +
  scale_y_continuous(breaks = seq(1, 4, 0.5), limits = c(1, 4))

figure1_mediator2 <- ggplot(data = data, aes(x = mediator2_vals, y = mediator2_pe_combined)) +
  geom_line() +
  geom_ribbon(aes(ymin = mediator2_lower, ymax = mediator2_upper), alpha = .25) +
  geom_hline(yintercept = mean(analytic_sample_mi$gpa_overall_2), linetype = "dashed") +
  xlab("Mediator 2") +
  ylab("Outcome") +
  labs("") +
  scale_y_continuous(breaks = seq(1, 4, 0.5), limits = c(1, 4))

figure1_outcome <- ggplot(data = data, aes(x = treat_vals, y = treatment_pe_combined)) +
  geom_line() +
  geom_ribbon(aes(ymin = treatment_lower, ymax = treatment_upper), alpha = .25) +
  geom_hline(yintercept = mean(analytic_sample_mi$gpa_overall_2), linetype = "dashed") +
  xlab("Treatment") +
  ylab("Outcome") +
  labs("") +
  scale_y_continuous(breaks = seq(1, 4, 0.5), limits = c(1, 4))

grid.arrange(figure1_mediator1, figure1_mediator2, figure1_outcome, nrow = 1, top = "Figure 1")

dev.off()


# Figure 2: density plot
treatment <- "peer_treatment_mean"
outcome <- "gpa_overall_2"
mediator1 <- "educexp_want_1"
mediator2 <- "educexp_likely_1"
cv <- "treatment_0"
vars <- c(treatment, outcome, mediator1, mediator2, cv, "_mi_m", "scid", "gswgt2")

data_name <- "sample_mi_friend_all"
file_path = file.path(workdata_path, paste0(data_name, ".dta"))
analytic_sample_mi <- read_dta(file_path)
analytic_sample_mi <- analytic_sample_mi[which(analytic_sample_mi$"_mi_m" != 0), vars]

cut1 <- quantile(analytic_sample_mi[[treatment]], probs=0.3333)
cut2 <- quantile(analytic_sample_mi[[treatment]], probs=0.6666)

analytic_sample_mi$treatmentTertile <- 2
analytic_sample_mi$treatmentTertile[analytic_sample_mi[[treatment]] <= cut1] <- 1
analytic_sample_mi$treatmentTertile[analytic_sample_mi[[treatment]] > cut2] <- 3

tertile1_mediator1 <- density(analytic_sample_mi[[mediator1]][analytic_sample_mi$treatmentTertile == 1])
tertile2_mediator1 <- density(analytic_sample_mi[[mediator1]][analytic_sample_mi$treatmentTertile == 2])
tertile3_mediator1 <- density(analytic_sample_mi[[mediator1]][analytic_sample_mi$treatmentTertile == 3])

tertile1_mediator2 <- density(analytic_sample_mi[[mediator2]][analytic_sample_mi$treatmentTertile == 1])
tertile2_mediator2 <- density(analytic_sample_mi[[mediator2]][analytic_sample_mi$treatmentTertile == 2])
tertile3_mediator2 <- density(analytic_sample_mi[[mediator2]][analytic_sample_mi$treatmentTertile == 3])


file_path = file.path(output_path, paste0('figure-2', ".tiff"))
tiff(file_path, width = 12, height = 9, units = 'in', res = 800)

par(mfrow=c(1,2))

plot(tertile1_mediator1, main="", xlab="Mediaotr 1 (Standardized)", ylab="Density", xlim=c(0,5), ylim=c(0,1), lwd=1.25, lty="solid")
par(new=T)
plot(tertile2_mediator1, main="", xlab="", ylab="", xlim=c(0,5), ylim=c(0,1), lwd=1.25, lty="dashed", axes=F)
par(new=T)
plot(tertile3_mediator1, main="", xlab="", ylab="", xlim=c(0,5), ylim=c(0,1), lwd=1.25, lty="dotted", axes=F)
title("Mediator 1", line=0.3, adj=0, cex.main=1)
legend("topleft", inset=0.025, c("1st tertile", "2nd Tertile", "3rd Tertile"), lty=c("solid", "dashed", "dotted"), lwd=1.25, seg=3, cex=0.8, title = "Treatment")

par(new=F)
plot(tertile1_mediator2, main="", xlab="Mediaotr 2 (Standardized)", ylab="Density", xlim=c(-5,5), ylim=c(0,1), lwd=1.25, lty="solid")
par(new=T)
plot(tertile2_mediator2, main="", xlab="", ylab="", xlim=c(0,5), ylim=c(0,1), lwd=1.25, lty="dashed", axes=F)
par(new=T)
plot(tertile3_mediator2, main="", xlab="", ylab="", xlim=c(0,5), ylim=c(0,1), lwd=1.25, lty="dotted", axes=F)
title("Mediator 2", line=0.3, adj=0, cex.main=1)
legend("topleft", inset=0.025, c("1st tertile", "2nd Tertile", "3rd Tertile"), lty=c("solid", "dashed", "dotted"), lwd=1.25, seg=3, cex=0.8, title = "Treatment")

dev.off()

# Figure 3
library(patchwork)

data_name <- "sample_friend_all_for_figure"
file_path = file.path(workdata_path, paste0(data_name, ".dta"))
analytic_sample <- read_dta(file_path)


analytic_sample$educexp_want_1_lev = factor(analytic_sample$educexp_want_1, 
                                            levels = c("5", "4", "3", "2", "1"))
analytic_sample$educexp_likely_1_lev = factor(analytic_sample$educexp_likely_1, 
                                            levels = c("5", "4", "3", "2", "1"))

p1 <- analytic_sample %>%
  mutate(treatment_tertile = ntile(peer_treatment_mean, n = 5)) %>% 
  ggplot(aes(x = treatment_tertile, fill = educexp_want_1_lev)) +
  geom_bar(position = "fill", color = "white") +
  scale_fill_brewer(direction = -1) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "none") +
  labs(y = "Proportion", x = "", title = "Educational // Aspiraction") 


p2 <- analytic_sample %>%
  mutate(treatment_tertile = ntile(peer_treatment_mean, n = 5)) %>%
  ggplot(aes(x = treatment_tertile, fill = educexp_likely_1_lev)) +
  geom_bar(position = "fill", color = "white") +
  scale_fill_brewer(direction = -1) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(y = "Proportion", x = "", title = "Educational Expectation")

p1 + p2

analytic_sample %>%
  filter(!is.na(educexp_likely_1)) %>% 
  mutate(treatment_tertile = ntile(peer_treatment_mean, n = 5)) %>% 
  group_by(treatment_tertile, educexp_likely_1_lev) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(treatment_tertile) %>% View()
  mutate(prop = n / sum(n))

  
# Figure 1
data_name <- "sample_friend_all_for_figure"
file_path = file.path(workdata_path, paste0(data_name, ".dta"))
analytic_sample <- read_dta(file_path)

design <- svydesign(data = analytic_sample, ids = ~scid, weights = ~gswgt2)
model <- svyglm(gpa_overall_2 ~ bs(peer_treatment_mean, df=4), data = analytic_sample, design = design)
granularity <- 50
treat_range <- range(analytic_sample$peer_treatment_mean, na.rm = T)
treat_vals <- seq(from = treat_range[1], to = treat_range[2], length = granularity)
newdata <- data.frame(peer_treatment_mean = treat_vals)
predicted_y <- predict(model, newdata = newdata)
plot(analytic_sample$peer_treatment_mean, analytic_sample$gpa_overall_2)
lines(treat_vals, predicted_y, col="red", lwd=5)
