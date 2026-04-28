library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### data set-up #######
library(dplyr)
library(haven) # to read .sav file
library(survey) # to incorporate survey weights
library(mice) # for imputation
library(mitools) # for imputation
library(openxlsx) # to export data
library(Hmisc) # for descriptive statistics
library(car) # multicollinearity checks
library(margins) # for finding average marginal effects
library(svyROC) # to incorporate survey weights into ROC plots, robustness check

# select relevant columns
bsa_raw <- read_sav("bsa2024.sav")
View(bsa_raw) # inspect data

# select relevant columns
bsa_select <- select(bsa_raw, serial_EUL, RespAge_Archive, DVsex24, Raceori4, 
                     HEdQual2, EMPSTAT, HHincome, ISSP_UrbRur, leftrigh, ISSP_Q22, 
                     ISSP_Q30, ISSP_Q31, ISSP_Q32, ISSP_Q33, ISSP_Q34, ISSP_Q16, 
                     Voted, ISSP_Q23_A_q, ISSP_Q23_B_q, ISSP_Q23_C_q, ISSP_Q23_D_q, 
                     ISSP_Q23_E_q, BSA24_final_wt, BSA24_strata)

# check which respondents did not receive the ISSP survey, create column indicating which ones did not
bsa_select <- bsa_select %>%
  mutate(
    missing_ISSP = if_all(contains("ISSP"), is.na),
    missing_ISSP = as.integer(missing_ISSP)
  ) 

# create missingness table
n_missing <- colSums(is.na(bsa_select))
n_total <- nrow(bsa_select)
prop_missing <- n_missing/n_total
initial_missing_table <- data.frame(variable = names(bsa_select), count = n_missing, prop = prop_missing)
initial_missing_table # view table

# export into excel
write.xlsx(initial_missing_table, "initial_missing.xlsx", asTable = TRUE)

# filter out missing ISSP responses
bsa_select <- bsa_select %>% 
  filter(missing_ISSP == 0) %>%
  select(-missing_ISSP)

nrow(bsa_select) # count number of remaining rows
View(bsa_select) # check if filtering was successful

# rename columns
colnames(bsa_select) <- c("serial_number", "age", "gender", "race", "education", 
                          "employment", "hh_income", "urban_rural", 
                          "left_right", "interest_politics", "gender_eq", 
                          "age_eq", "urb_rur_eq", "edu_eq", "richpoor_eq", 
                          "express_internet", "voted", "sign_petition", 
                          "participate_protest", "contact_politician", "organise_protest", 
                          "join_group", "weights", "strata")
View(bsa_select) # check that the columns were correctly renamed

# re-code invalid answers as NA
bsa_select <- bsa_select %>% 
  mutate(
    age = if_else(age %in% c(-1, 998, 999), NA, age),
    gender = if_else(gender %in% c(-1, 8, 9), NA, gender),
    race = if_else(race %in% c(-1, 8, 9), NA, race),
    education = if_else(education %in% c(-1, 8, 9, 906), NA, education),
    employment = if_else(employment %in% c(-1, 8, 9), NA, employment),
    hh_income = if_else(hh_income %in% c(-1, 8, 9), NA, hh_income),
    urban_rural = if_else(urban_rural %in% c(-1, 9, 906, 907), NA, urban_rural),
    left_right = if_else(left_right %in% c(-1, 9), NA, left_right),
    interest_politics = if_else(interest_politics %in% c(-1, 6, 9), NA, interest_politics),
    gender_eq = if_else(gender_eq %in% c(-1, 5, 9), NA, gender_eq),
    age_eq = if_else(age_eq %in% c(-1, 5, 9), NA, age_eq),
    urb_rur_eq = if_else(urb_rur_eq %in% c(-1, 5, 9), NA, urb_rur_eq),
    edu_eq = if_else(edu_eq %in% c(-1, 5, 9), NA, edu_eq),
    richpoor_eq = if_else(richpoor_eq %in% c(-1, 5, 9), NA, richpoor_eq),
    express_internet = if_else(express_internet %in% c(-1, 7, 9), NA, express_internet),
    voted = if_else(voted %in% c(-1, 3, 8, 9), NA, voted),
    sign_petition = if_else(sign_petition %in% c(-1, 5, 9), NA, sign_petition),
    participate_protest = if_else(participate_protest %in% c(-1, 5, 9), NA, participate_protest),
    contact_politician = if_else(contact_politician %in% c(-1, 5, 9), NA, contact_politician),
    organise_protest = if_else(organise_protest %in% c(-1, 5, 9), NA, organise_protest),
    join_group = if_else(join_group %in% c(-1, 5, 9), NA, join_group)
  )

# check for missingness: create data frame with count and proportion of each column's missingness
missing <- data.frame(
  full_var_names <- c(
    "serial_number","age","gender", "race", "education", "employment", "hh_income",
    "urban_rural", "left_right", "interest_politics", "gender_eq", "age_eq", "urb_rur_eq",
    "edu_eq","richpoor_eq", "express_internet", "voted", "sign_petition", 
    "participate_protest", "contact_politician", "organise_protest", "join_group",
    "weights", "strata" 
  ), # include all variable names
  missing_count <- sapply(bsa_select, function(i) sum(is.na(i))), # number of missing values
  missing_prop <- colMeans(is.na(bsa_select)) # proportion of missing values
)

# missingness patterns in IVs, DVs
names(missing) <- c("variable", "number of missing values", "proportion of missing values")
write.xlsx(missing, file = "missingness_patterns.xlsx", asTable = TRUE) # export data


# re-code voted variable as binary for logistic regression
bsa_select$voted <- ifelse(bsa_select$voted == 1, 1, 0) # yes = 1, no = 0


# political participation variables: re-code offline and online into only yes or no
bsa_select$sign_petition <- ifelse(bsa_select$sign_petition == 1 | 
                                     bsa_select$sign_petition == 2 | 
                                     bsa_select$sign_petition == 3, 1, 0) # signed = 1, not signed = 0

bsa_select$participate_protest <- ifelse(bsa_select$participate_protest == 1 | 
                                           bsa_select$participate_protest == 2 | 
                                           bsa_select$participate_protest == 3, 1, 0)

bsa_select$contact_politician <- ifelse(bsa_select$contact_politician == 1 | 
                                          bsa_select$contact_politician == 2 | 
                                          bsa_select$contact_politician == 3, 1, 0)

bsa_select$organise_protest <- ifelse(bsa_select$organise_protest == 1 | 
                                        bsa_select$organise_protest == 2 | 
                                        bsa_select$organise_protest == 3, 1, 0)

bsa_select$join_group <- ifelse(bsa_select$join_group == 1 | 
                                  bsa_select$join_group == 2 | 
                                  bsa_select$join_group == 3, 1, 0)

# recode into levels based on frequency of expression
bsa_select$express_internet <- ifelse(bsa_select$express_internet == 1 | 
                                        bsa_select$express_internet == 2
                                      | bsa_select$express_internet == 3 | 
                                        bsa_select$express_internet == 4
                                      | bsa_select$express_internet == 5, 1, 0)

# create data frame summarising number and percentage of occurrences of "neither" in each inequality variable
eq_vars <- c("gender_eq", "age_eq", "edu_eq", "urb_rur_eq", "richpoor_eq")
neither_summary <- data.frame(
  variable  = eq_vars,
  count_4   = sapply(eq_vars, function(v)
    sum(bsa_select[[v]] == 4, na.rm = TRUE)
  ),
  percent_4 = sapply(eq_vars, function(v)
    sum(bsa_select[[v]] == 4, na.rm = TRUE) /
      sum(!is.na(bsa_select[[v]])) * 100
  )
)
# rename columns
names(neither_summary) <- c("variable", "number of 'neither'", 
                            "percentage of 'neither'")
# export into excel
write.xlsx(neither_summary, file = "inequality_neither.xlsx", asTable = TRUE)

# re-code equality variables to indicate "neither benefit" also count as "equal benefit"
bsa_select$gender_eq <- ifelse(bsa_select$gender_eq == 4, 2, bsa_select$gender_eq)
bsa_select$age_eq <- ifelse(bsa_select$age_eq == 4, 2, bsa_select$age_eq)
bsa_select$edu_eq <- ifelse(bsa_select$edu_eq == 4, 2, bsa_select$edu_eq)
bsa_select$urb_rur_eq <- ifelse(bsa_select$urb_rur_eq == 4, 2, bsa_select$urb_rur_eq)
bsa_select$richpoor_eq <- ifelse(bsa_select$richpoor_eq == 4, 2, bsa_select$richpoor_eq)

#### re-code categorical variable level names ####
bsa_select <- bsa_select %>%
  mutate(
    gender = factor(gender,
                    levels = c(1, 2),
                    labels = c("Female", "Male")),
    race = factor(race,
                  levels = c(1, 2, 3, 4, 5),
                  labels = c("Black", "Asian", "White", "Mixed", "Other")),
    education = factor(education,
                       levels = c(1, 2, 3, 4, 6),
                       labels = c("Degree and above", "Other Higher Education", 
                                  "A-levels and equivalent", "GCSE and equivalent",
                                  "No qualifications")),
    hh_income = factor(hh_income,
                       levels = c(1, 2, 3, 4),
                       labels = c("< £431", "£431 - £700", 
                                  "£701 - £1,060",
                                  "£1,061+")),
    employment = factor(employment,
                        levels = c(1, 2, 3, 4),
                        labels = c("Employed", "Self-Employed with employees", 
                                   "Self-employed without employees",
                                   "No job")),
    urban_rural = factor(urban_rural,
                         levels = c(1, 2, 3, 4, 5),
                         labels = c("Big city", "Suburbs", "Small city", 
                                    "Country village", "Farm")),
    interest_politics = factor(interest_politics,
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Very", "Fairly", "Somewhat", "Not very",
                                          "Not at all")),
    gender_eq = factor(gender_eq, 
                       levels = c(1, 2, 3),
                       labels = c("Women", "Equal", "Men")),
    age_eq = factor(age_eq,
                    levels = c(1, 2, 3),
                    labels = c("Older", "Equal", "Younger")),
    urb_rur_eq = factor(urb_rur_eq, 
                        levels = c(1, 2, 3),
                        labels = c("Countryside", "Equal", "Cities")),
    edu_eq = factor(edu_eq,
                    levels = c(1, 2, 3),
                    labels = c("Highly educated", "Equal", "Less educated")),
    richpoor_eq = factor(richpoor_eq, 
                         levels = c(1, 2, 3),
                         labels = c("Rich", "Equal", "Poor")),
    express_internet = factor(express_internet,
                              levels = c(0, 1),
                              labels = c("No", "Yes")),
    voted = factor(voted,
                   levels = c(0, 1),
                   labels = c("No", "Yes")),
    sign_petition = factor(sign_petition,
                           levels = c(0, 1),
                           labels = c("No", "Yes")),
    participate_protest = factor(participate_protest,
                                 levels = c(0, 1),
                                 labels = c("No", "Yes")),
    contact_politician = factor(contact_politician,
                                levels = c(0, 1),
                                labels = c("No", "Yes")),
    organise_protest = factor(organise_protest,
                              levels = c(0, 1),
                              labels = c("No", "Yes")),
    join_group = factor(join_group,
                        levels = c(0, 1),
                        labels = c("No", "Yes"))
  )


# filter for only complete cases
bsa <- bsa_select[complete.cases(bsa_select), ]
View(bsa)
nrow(bsa) # 724 rows compared to 1529



#### robustness checks: multicollinearity ####

# checking for multicollinearity: express on internet
m1 <- glm(express_internet ~ interest_politics + gender_eq + age_eq + urb_rur_eq + edu_eq + 
            richpoor_eq + age + gender + race + education + employment + 
            hh_income + urban_rural + left_right, data = bsa_select,
          family = binomial(link = "logit"))

vif_express <- vif(m1) # store vif values into a variable
adj_vif <- vif_express[, "GVIF^(1/(2*Df))"] # extract adjusted GVIF 
tolerance <- 1 / adj_vif # calculate tolerance from adjusted VIF

# create table to organise values
vif_express_table <- data.frame(
  Adjusted_VIF = adj_vif,
  Adjusted_Tolerance = tolerance
)
vif_express_table # view table
write.xlsx(vif_express_table, "vifExpress.xlsx", asTable = TRUE) # export table to excel

# voted
m2 <- glm(voted ~ interest_politics + gender_eq + age_eq + urb_rur_eq + edu_eq + 
            richpoor_eq + age + gender + race + education + employment + 
            hh_income + urban_rural + left_right, data = bsa_select,
          family = binomial(link = "logit"))

# repeat same process to create table of GVIF and tolerances
vif_voted <- vif(m2)
adj_vif2 <- vif_voted[, "GVIF^(1/(2*Df))"]
tolerance2 <- 1 / adj_vif2 

vif_voted_table <- data.frame(
  Adjusted_VIF = adj_vif2,
  Adjusted_Tolerance = tolerance2
)
vif_voted_table
write.xlsx(vif_voted_table, "vifVoted.xlsx", asTable = TRUE)


# same for sign_petition
m3 <- glm(sign_petition ~ interest_politics + gender_eq + age_eq + urb_rur_eq + edu_eq + 
            richpoor_eq + age + gender + race + education + employment + 
            hh_income + urban_rural + left_right, data = bsa_select,
          family = binomial(link = "logit"))

vif_sign <- vif(m3)
adj_vif3 <- vif_sign[, "GVIF^(1/(2*Df))"]
tolerance3 <- 1 / adj_vif3

vif_sign_table <- data.frame(
  Adjusted_VIF = adj_vif3,
  Adjusted_Tolerance = tolerance3
)
vif_sign_table
write.xlsx(vif_sign_table, "vifSign.xlsx", asTable = TRUE)

# participate_protest
m4 <- glm(participate_protest ~ interest_politics + gender_eq + age_eq + urb_rur_eq + edu_eq + 
            richpoor_eq + age + gender + race + education + employment + 
            hh_income + urban_rural + left_right, data = bsa_select,
          family = binomial(link = "logit"))

vif_participate <- vif(m4)
adj_vif4 <- vif_participate[, "GVIF^(1/(2*Df))"]
tolerance4 <- 1 / adj_vif4

vif_participate_table <- data.frame(
  Adjusted_VIF = adj_vif4,
  Adjusted_Tolerance = tolerance4
)
vif_participate_table
write.xlsx(vif_participate_table, "vifParticipate.xlsx", asTable = TRUE)

# contact_politician
m5 <- glm(contact_politician ~ interest_politics + gender_eq + age_eq + urb_rur_eq + edu_eq + 
            richpoor_eq + age + gender + race + education + employment + 
            hh_income + urban_rural + left_right, data = bsa_select,
          family = binomial(link = "logit"))

vif_contact <- vif(m5)
adj_vif5 <- vif_contact[, "GVIF^(1/(2*Df))"] 
tolerance5 <- 1 / adj_vif5

vif_contact_table <- data.frame(
  Adjusted_VIF = adj_vif5,
  Adjusted_Tolerance = tolerance5
)
vif_contact_table
write.xlsx(vif_contact_table, "vifContact.xlsx", asTable = TRUE)

# organise_protest
m6 <- glm(organise_protest ~ interest_politics + gender_eq + age_eq + urb_rur_eq + edu_eq + 
            richpoor_eq + age + gender + race + education + employment + 
            hh_income + urban_rural + left_right, data = bsa_select,
          family = binomial(link = "logit"))

vif_organise <- vif(m6)
adj_vif6 <- vif_organise[, "GVIF^(1/(2*Df))"]
tolerance6 <- 1 / adj_vif6

vif_organise_table <- data.frame(
  Adjusted_VIF = adj_vif6,
  Adjusted_Tolerance = tolerance6
)
vif_organise_table
write.xlsx(vif_organise_table, "vifOrganise.xlsx", asTable = TRUE)

# join_group
m7 <- glm(join_group ~ interest_politics + gender_eq + age_eq + urb_rur_eq + edu_eq + 
            richpoor_eq + age + gender + race + education + employment + 
            hh_income + urban_rural + left_right, data = bsa_select,
          family = binomial(link = "logit"))

vif_join <- vif(m7)
adj_vif7 <- vif_join[, "GVIF^(1/(2*Df))"]
tolerance7 <- 1 / adj_vif7

vif_join_table <- data.frame(
  Adjusted_VIF = adj_vif7,
  Adjusted_Tolerance = tolerance7
)
vif_join_table
write.xlsx(vif_join_table, "vifJoin.xlsx", asTable = TRUE)


#### descriptive statistics: complete cases ####

# create variable containing all relevant variables
varNames <- c(
  "gender_eq", "age_eq", "edu_eq", "richpoor_eq", "urb_rur_eq",
  "age", "gender", "race", "education", "employment", "hh_income",
  "urban_rural", "interest_politics", "left_right",
  "express_internet", "voted", "sign_petition", "participate_protest",
  "organise_protest", "contact_politician", "join_group"
)

# store categorical variable names only, to calculate proportions
varNames_categorical <- c(
  "gender_eq", "age_eq", "edu_eq", "richpoor_eq", "urb_rur_eq",
  "gender", "race", "education", "employment", "hh_income",
  "urban_rural", "interest_politics", "express_internet",
  "voted", "sign_petition", "participate_protest",
  "organise_protest", "contact_politician", "join_group"
)

# proper labels to label bar charts
full_labels <- c(
  gender = "Gender",
  race = "Race",
  education = "Education level",
  employment = "Employment status",
  hh_income = "Household income",
  urban_rural = "Urban–rural residence",
  interest_politics = "Interest in politics",
  gender_eq = "Inequality: gender",
  age_eq = "Inequality: age",
  urb_rur_eq = "Inequality: urban/rural",
  edu_eq = "Inequality: education",
  richpoor_eq = "Inequality: wealth",
  express_internet = "Expressing political views online",
  voted = "Voted",
  sign_petition = "Signed a petition",
  participate_protest = "Participated in a protest",
  contact_politician = "Contacted a politician",
  organise_protest = "Organised a protest",
  join_group = "Joined a political group"
)


# create design variable
bsa_design <- svydesign(ids = ~serial_number, strata = ~strata, weights = ~weights, 
                        data = bsa, nest = TRUE)

# weighted quantiles and means for age, left_right
quant_age <- svyquantile(
  ~ age,
  design = bsa_design,
  quantiles = c(0, 0.25, 0.5, 0.75, 1),
  ci = FALSE
)

quant_LR <- svyquantile(
  ~ left_right,
  design = bsa_design,
  quantiles = c(0, 0.25, 0.5, 0.75, 1),
  ci = FALSE
)

# view weighted quantiles
quant_age
quant_LR

# view weighted means
svymean(~ age + left_right, design = bsa_design, na.rm = TRUE)


# categorical variables: svymean and svytable
# create data frame that aggregates proportions for each variable

tab_list <- lapply(varNames_categorical, function(v) {
  # svytable for each categorical variable name
  t <- svytable(as.formula(paste0("~", v)), design = bsa_design)
  
  # convert to data frame
  df <- as.data.frame(t)
  names(df) <- c("category", "count")
  df$variable <- v
  df[, c("variable", "category", "count")] # specify columns
})
names(tab_list) <- varNames_categorical # set names to the categorical variables

# export one excel file, one sheet per variable
write.xlsx(x = tab_list, file = "proportions.xlsx")

# weighted proportions: similar process for svymean, for both numeric and categorical variables
# store svymean values for each variable
means <- svymean(
  as.formula(paste0("~", paste(varNames_categorical, collapse = "+"))),
  design = bsa_design,
  na.rm = TRUE
)

# create data frame that aggregates weighted proportions and SE for each variable
df_means <- data.frame(
  variable = names(coef(means)),
  means_proportions = as.numeric(coef(means)),
  SE = as.numeric(SE(means))
)

# export excel file, one page only
write.xlsx(df_means, file = "weighted_proportions.xlsx")

# afterwards, on excel, add weighted proportion values to each sheet on proportions.xlsx 


# plot bar charts to visualise proportions
library(ggplot2)
library(scales)
library(stringr)

# create function needing arguments of 1) variable names and 2) survey design
plot_svy_bar <- function(var_name, design) {
  x_label <- full_labels[[var_name]]
  v <- as.formula(paste0("~", var_name)) # to give the output ~gender, for example
  m <- svymean(v, design = design, na.rm = TRUE) # store svymean output for each variable name
  
  df <- data.frame(
    category = names(coef(m)),
    prop = as.numeric(coef(m))
  )
  
  # remove variable name prefix from category labels
  df$category <- str_remove(df$category, paste0("^", var_name))
  
  # create bar chart
  ggplot(df, aes(x = category, y = prop)) +
    geom_col() +
    scale_y_continuous(labels = percent_format()) +
    labs(x = x_label, y = "Weighted proportion") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plots <- lapply(varNames_categorical, plot_svy_bar, design = bsa_design)
names(plots) <- varNames_categorical

# view bar chart for each variable
library(patchwork)

n_per_page <- 6 # 6 plots per page
n <- length(plots)

# split into groups
split_groups <- split(seq_len(n), ceiling(seq_along(plots) / n_per_page))

# loop over each group
for (g in split_groups) {
  page_plots <- plots[g]
  p <- wrap_plots(page_plots, nrow = 2, ncol = 3)
  print(p)
}

# print out separate bar charts
for (v in varNames) {
  print(plots[[v]])
}


#### compare complete cases vs non-complete cases ####

# create column indicating whether row is a complete case or not
bsa_select$complete <- ifelse(complete.cases(bsa_select[ , varNames]), 1, 0)

# set survey design for descriptive statistics
descriptive <- svydesign(ids = ~serial_number, strata = ~strata, weights = ~weights, data = bsa_select)


# numeric variables
# select names of numeric variables
numeric_vars <- c("age", "left_right") 

# create function looped across numeric variables
num_results <- lapply(numeric_vars, function(v) {
  fmla <- as.formula(paste0("~", v)) # create formula e.g. "~age"
  # store svyby() output of the formula in "results" variable
  results <- svyby(fmla, ~complete, design = descriptive, svymean, na.rm = TRUE)
  # extract and store standard error from "results" as a vector
  se_vec <- SE(results)
  
  # create dataframe
  data.frame(variable = v, 
             complete = results$complete, 
             mean = results[[v]], 
             se = se_vec)
})
# bind smaller data frames into one data frame: df_numeric
df_numeric <- do.call(rbind, num_results)


# categorical: similar process
categorical_results <- lapply(varNames_categorical, function(v) {
  fmla <- as.formula(paste0("~", v, " + complete")) # create 2-way formula: e.g. gender + complete
  # store svytable() for cross-tabulation between variable(e.g. gender) and "complete"
  tab <- svytable(fmla, design = descriptive)
  prop <- prop.table(tab, margin = 2) # create proportions table
  
  # create small data frames for each component of a larger data frame
  df_tab <- as.data.frame(tab)
  df_prop <- as.data.frame(prop)
  name_cols <- names(df_tab)[1:2] # stores names of first 2 columns for merging
  
  # create merged data frame
  df <- merge(df_tab, df_prop, by = name_cols)
  names(df) <- c("category", "complete", "count", "proportion")
  df$variable <- v # store variable name
  
  df[, c("variable", "category", "complete", "count", "proportion")]
})

# bind smaller data frames into one
df_categorical <- do.call(rbind, categorical_results)

# export excel file with both numeric and categorical descriptive comparisons
write.xlsx(
  x = list(numeric_descriptives = df_numeric, categorical_descriptives = df_categorical),
  file = "complete_vs_noncomplete_descriptives.xlsx", asTable = TRUE
)


#### regressions: set-up ####
# set reference levels
bsa$gender_eq <- relevel(bsa$gender_eq, ref = "Equal")
bsa$age_eq <- relevel(bsa$age_eq, ref = "Equal")
bsa$urb_rur_eq <- relevel(bsa$urb_rur_eq, ref = "Equal")
bsa$edu_eq <- relevel(bsa$edu_eq, ref = "Equal")
bsa$richpoor_eq <- relevel(bsa$richpoor_eq, ref = "Equal")

bsa$gender <- relevel(bsa$gender, ref = "Male")
bsa$race <- relevel(bsa$race, ref = "White")
bsa$education <- relevel(bsa$education, ref = "No qualifications")
bsa$gender <- relevel(bsa$gender, ref = "Male")
bsa$employment <- relevel(bsa$employment, ref = "No job")
bsa$hh_income <- relevel(bsa$hh_income, ref = "< £431") 
bsa$urban_rural <- relevel(bsa$urban_rural, ref = "Big city")
bsa$interest_politics <- relevel(bsa$interest_politics, ref = "Not at all")

# set survey design: complete cases only
bsa_design <- svydesign(ids = ~serial_number, strata = ~strata, weights = ~weights, data = bsa, nest = TRUE)

#### stage 1: baseline regressions for each DV ####

# express opinion on internet
baseline_express <- svyglm(express_internet ~ interest_politics + age + gender + race + education + employment + 
                         hh_income + urban_rural + left_right, 
                       data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# voted
baseline_voted <- svyglm(voted ~ interest_politics + age + gender + race + education + employment + 
                       hh_income + urban_rural + left_right, 
                     data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# sign petition
baseline_sign <- svyglm(sign_petition ~ interest_politics + age + gender + race + education + employment + 
                      hh_income + urban_rural + left_right, 
                    data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# participate protest
baseline_participate <- svyglm(participate_protest ~ interest_politics + age + gender + race + education + employment + 
                             hh_income + urban_rural + left_right, 
                           data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# contact politician
baseline_contact <- svyglm(contact_politician ~ interest_politics + age + gender + race + education + employment + 
                         hh_income + urban_rural + left_right, 
                       data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# organise protest
baseline_organise <- svyglm(organise_protest ~ interest_politics + age + gender + race + education + employment + 
                          hh_income + urban_rural + left_right, 
                        data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# join group
baseline_join <- svyglm(join_group ~ interest_politics + age + gender + race + education + employment + 
                      hh_income + urban_rural + left_right, 
                    data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)


# average marginal effects
tab_base_express <- summary(margins(baseline_express, design = bsa_design))
tab_base_voted <- summary(margins(baseline_voted, design = bsa_design))
tab_base_sign <- summary(margins(baseline_sign, design = bsa_design))
tab_base_participate <- summary(margins(baseline_participate, design = bsa_design))
tab_base_contact <- summary(margins(baseline_contact, design = bsa_design))
tab_base_organise <- summary(margins(baseline_organise, design = bsa_design))
tab_base_join <- summary(margins(baseline_join, design = bsa_design))

# export to excel spreadsheets
write.xlsx(tab_base_express, "base_express.xlsx", asTable = TRUE)
write.xlsx(tab_base_voted, "base_voted.xlsx", asTable = TRUE)
write.xlsx(tab_base_sign, "base_sign.xlsx", asTable = TRUE)
write.xlsx(tab_base_participate, "base_participate.xlsx", asTable = TRUE)
write.xlsx(tab_base_contact, "base_contact.xlsx", asTable = TRUE)
write.xlsx(tab_base_organise, "base_organise.xlsx", asTable = TRUE)
write.xlsx(tab_base_join, "base_join.xlsx", asTable = TRUE)



#### stage 2: full model for each DV ####
# express opinion on internet
full_express <- svyglm(express_internet ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                       richpoor_eq + interest_politics + age + gender + race + education + employment + 
                       hh_income + urban_rural + left_right, 
                     data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)

# voted: this has more statistically significant results for IVs
full_voted <- svyglm(voted ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                     richpoor_eq + interest_politics + age + gender + race + education + employment + 
                     hh_income + urban_rural + left_right, 
                   data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)

# sign petition
full_sign <- svyglm(sign_petition ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                    richpoor_eq + interest_politics + age + gender + race + education + employment + 
                    hh_income + urban_rural + left_right, 
                  data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)

# participate protest
full_participate <- svyglm(participate_protest ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                           richpoor_eq + interest_politics + age + gender + race + education + employment + 
                           hh_income + urban_rural + left_right, 
                         data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)

# contact politician
full_contact <- svyglm(contact_politician ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                       richpoor_eq + interest_politics + age + gender + race + education + employment + 
                       hh_income + urban_rural + left_right, 
                     data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)

# organise protest
full_organise <- svyglm(organise_protest ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                        richpoor_eq + interest_politics + age + gender + race + education + employment + 
                        hh_income + urban_rural + left_right, 
                      data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# join group
full_join <- svyglm(join_group ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                    richpoor_eq + interest_politics + age + gender + race + education + employment + 
                    hh_income + urban_rural + left_right, 
                  data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)


# average marginal effects
tab_full_express <- summary(margins(full_express, design = bsa_design))
tab_full_voted <- summary(margins(full_voted, design = bsa_design))
tab_full_sign <- summary(margins(full_sign, design = bsa_design))
tab_full_participate <- summary(margins(full_participate, design = bsa_design))
tab_full_contact <- summary(margins(full_contact, design = bsa_design))
tab_full_organise <- summary(margins(full_organise, design = bsa_design))
tab_full_join <- summary(margins(full_join, design = bsa_design))

# export to excel
write.xlsx(tab_full_express, "full_express.xlsx", asTable = TRUE)
write.xlsx(tab_full_voted, "full_voted.xlsx", asTable = TRUE)
write.xlsx(tab_full_sign, "full_sign.xlsx", asTable = TRUE)
write.xlsx(tab_full_participate, "full_participate.xlsx", asTable = TRUE)
write.xlsx(tab_full_contact, "full_contact.xlsx", asTable = TRUE)
write.xlsx(tab_full_organise, "full_organise.xlsx", asTable = TRUE)
write.xlsx(tab_full_join, "full_join.xlsx", asTable = TRUE)


