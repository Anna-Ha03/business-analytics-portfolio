rm(list=ls())

library(haven)
library(stargazer)
library(ggplot2)
library(lattice)
library(sandwich)
library(lmtest)
library(lfe)
library(tidyr)

# DATA PREPARATION
perf_data <- read_dta("/users/hahai/Downloads/Working from home-20260504/Performance.dta")
perf <- data.frame(perf_data)
View(perf)
# Dropping missing outcome values
perf <- perf[!is.na(perf$perform1), ]

# Variable Definition
perf <- within(perf, {
  WFH           <- experiment_treatment 
  has_children  <- children             
  Label_WFH     <- ifelse(WFH == 1, "Home", "Office")
  Label_Child   <- ifelse(has_children == 1, "With Children", "No Children")
})

perf$personid  <- factor(perf$personid)
perf$year_week <- factor(perf$year_week)

# DESCRIPTIVE STATISTICS
# Table 1: Summary Statistics
perf_desc <- as.data.frame(lapply(
  perf[, c("perform1","WFH","has_children","tenure","age","grosswage","men","married","commute")],
  as.numeric))

stargazer(perf_desc, type="text", align=TRUE, digits=2, title="Table 1: Summary Statistics")

# GRAPHICAL ANALYSIS

# Figure 1: Performance Trend 
trend_data <- aggregate(perform1 ~ year_week + expgroup, data = perf, FUN = mean)
trend_data$Group <- ifelse(trend_data$expgroup == 1, "Treatment (Home)", "Control (Office)")

ggplot(trend_data, aes(x = as.numeric(year_week), y = perform1, color = Group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 49, linetype = "dashed", color = "darkred") +
  scale_color_manual(values = c("#e74c3c", "#2980b9")) +
  labs(title = "Figure 1: Performance Trend", x = "Week Number", y = "Average Performance") +
  theme_bw() + theme(legend.position = "top")

# Figure 2: Performance by Parent Status 
trend_child <- aggregate(perform1 ~ year_week + expgroup + Label_Child, data = perf, FUN = mean)
trend_child$Group <- ifelse(trend_child$expgroup == 1, "Home", "Office")

ggplot(trend_child, aes(x = as.numeric(year_week), y = perform1, color = Group)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 49, linetype = "dashed", color = "darkred") +
  facet_wrap(~ Label_Child) +
  scale_color_manual(values = c("#2980b9", "#e74c3c")) +
  labs(title = "Figure 1: Performance Trend by Parent Status", x = "Week Number", y = "Average Performance") +
  theme_bw() + theme(legend.position = "top")

# Figure 2: Commute Time distribution by Parent Status
ggplot(perf, aes(x = commute, y = perform1, color = Label_WFH)) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Label_Child) +
  scale_color_manual(values = c("Office" = "#e74c3c", "Home" = "#2980b9")) +
  labs(title = "Figure 2: Commute Time vs Performance by Parent Status",
       x = "Commute time (min)", y = "Performance Score",
       color = NULL) +
  theme_bw() + theme(legend.position = "top")

# REGRESSION ANALYSIS

# Table 2: OLS Models
m1 <- lm(data=perf, perform1 ~ WFH)
m2 <- lm(data=perf, perform1 ~ WFH + has_children + married + men + commute + logdaysworked)
m3 <- lm(data=perf, perform1 ~ WFH * has_children + married + men + commute + logdaysworked)

se_ols <- list(sqrt(diag(vcovHC(m1, type="HC1"))), sqrt(diag(vcovHC(m2, type="HC1"))), sqrt(diag(vcovHC(m3, type="HC1"))))
stargazer(m1, m2, m3, se=se_ols, type="text", title="Table 2: OLS Results", 
          covariate.labels=c("WFH","Has children","Married","Men","Commute", "Log Days Worked","WFH x Has children"))


# Table 3: Fixed Effects Models
m_fe1 <- felm(perform1 ~ WFH | personid + year_week | 0 | personid, data = perf)

m_fe2 <- felm(perform1 ~ WFH + logdaysworked | personid + year_week | 0 | personid, data = perf)

m_fe3 <- felm(perform1 ~ WFH * has_children + logdaysworked | 
                personid + year_week | 0 | personid, data = perf)

stargazer(m_fe1, m_fe2, m_fe3, type = "text",
          title = "Table 3: Fixed Effects Models for Productivity",
          add.lines = list(
            c("Individual FE", "Yes", "Yes", "Yes"),
            c("Time FE",       "Yes", "Yes", "Yes"),
            c("Controls logdaysworked", "No", "Yes", "Yes")
          ))

# Table 4: Interaction Model 
m_interaction <- felm(perform1 ~ WFH + WFH:has_children + logdaysworked | 
                        personid + year_week | 0 | personid, data = perf)
stargazer(m_interaction, type = "text",
          title = "Table 4: Interaction Model (Two-Way Fixed Effects)",
          column.labels = c("Main Performance Model"),
          add.lines = list(c("Individual FE", "Yes"), 
                           c("Time FE", "Yes")))

