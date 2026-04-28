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

