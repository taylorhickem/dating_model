# conversion model for leads sourced through USC

# library import ---------------------------------------------------------------------------------------
library(readxl)
library(ggplot2)


# constants --------------------------------------------------------------------------------------------
data_filename = "sex_20241201.xlsx"
sheets <- list(
  events = "sexual_contacts",
  contacts = "contacts",
  usc_contacts = "tags",
  usc_usernames = "usc_first_contact"
)
ranges <- list(
  usc_contacts = "AC1:AL500",
  usc_usernames = "A1:B500"
)
key_fields <- list(
  events = "date",
  contacts = "contact_name",
  usc_contacts = "contact_tag",
  usc_usernames = "username"
)


# helper functions -------------------------------------------------------------------------------------
remove_na_rows <- function(df, column) {
  clean <- df[!is.na(df[[column]]), ]
  return(clean)
}


# main  ------------------------------------------------------------------------------------------------

# load data from Excel 
events <<- read_excel(data_filename, sheet = sheets$events)
contacts <<- read_excel(data_filename, sheet = sheets$contacts)
usc_contacts <<- read_excel(data_filename, sheet = sheets$usc_contacts, range = ranges$usc_contacts)
usc_usernames <<- read_excel(data_filename, sheet = sheets$usc_usernames, range = ranges$usc_usernames)

# collect into a list
sources <- list(
  events = events,
  contacts = contacts,
  usc_contacts = usc_contacts,
  usc_usernames = usc_usernames
)

# remove na 
for (t in names(sources)) {
  sources[[t]] <- remove_na_rows(sources[[t]], key_fields[[t]])
}

# join into contacts table with dates for each stage
users_j1 <- merge(
  x = sources$usc_usernames,
  y = sources$usc_contacts[, c("contact_tag", "usc_username", "start")],
  by.x = "username",
  by.y = "usc_username",
  all.x = TRUE 
)

# drop time convert timestamp to date
users_j1$start <- as.Date(users_j1$start) 

# order by first contact date
users_j1 <- users_j1[order(users_j1$first_contact), ]

users_j1$user_id <- seq_along(users_j1$username)

users_j2 <- merge(
  x = users_j1[, c("user_id", "contact_tag", "first_contact", "start")],
  y = sources$contacts[, c("contact_name", "first_sex", "pre_date_hrs")],
  by.x = "contact_tag",
  by.y = "contact_name",
  all.x = TRUE 
)

# rename fields
rename_fields_j2 <- list(
  "start" = "contact_date",
  "first_contact" = "bid_date",
  "first_sex" = "sex_date"
)

for (f in names(rename_fields_j2)) {
  colnames(users_j2)[colnames(users_j2) == f] <- rename_fields_j2[[f]]
}

# order by bid date
users <- users_j2[order(users_j2$bid_date),]

# add date difference calculated fields - contact days
users$contact_days <- ifelse(
  !is.na(users$contact_date), 
  as.numeric(difftime(users$contact_date, users$bid_date, units = "days")),
  NA
)

# add date difference calculated fields - sex days
users$sex_days <- ifelse(
  !is.na(users$sex_date), 
  as.numeric(difftime(users$sex_date, users$contact_date, units = "days")),
  NA
)

# keep analysis fields
users <- users[, c(
  "user_id",
  "contact_days",
  "sex_days",
  "pre_date_hrs"
)]

# create tables for contacts and sex
sex_by_stage <- users[!is.na(users$sex_days),]
no_sex_by_stage <- users[is.na(users$sex_days),]
contacts_by_stage <- users[!is.na(users$contact_days),]

#plot hrs and days in contact
ggplot(data = sex_by_stage, aes(x = sex_days, y = pre_date_hrs)) +
  geom_point() + # Scatter plot
  labs(
    title = "Plot of sex_days vs. pre_date_hrs",
    x = "lead days",
    y = "lead hrs"
  ) +
  theme_minimal() # A clean theme

# histogram days in contact
sex_days_bins <- c(0, 7, 14, 21, 28, 35)
sex_by_stage$sex_days_bins <- cut(sex_by_stage$sex_days, breaks = sex_days_bins, include.lowest = TRUE)
sex_days_hist <- as.data.frame(table(sex_by_stage$sex_days_bins))

# convert days to log(days)
colnames(sex_days_hist) <- c("sex_days_bins", "counts")
bin_edges <- as.numeric(gsub(".*,(\\d+\\.?\\d*)\\]", "\\1", levels(sex_days_hist$sex_days_bins))) # Clean and extract numbers
log_bin_edges <- log(bin_edges) # Apply natural log transformation
sex_days_hist$log_bins <- log_bin_edges

# plot histogram
ggplot(sex_days_hist, aes(x = log_bins, y = counts)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "days in contact",
    x = "log(days)",
    y = "count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# model probability of conversion given days in contact

stage_counts <- list(
  users = nrow(users),
  contacts = nrow(contacts_by_stage),
  sex = nrow(sex_by_stage)
)

# get normalization constant
p_cs <- stage_counts$sex / stage_counts$contacts

# normalize the counts to marginal probability
sex_days_hist$dp_dt <- (sex_days_hist$counts / stage_counts$contacts) * p_cs

# apply the linear regression
model <- lm(log_bins ~ dp_dt, data = sex_days_hist)
coefficients <- coef(model)
d_max <- exp(-coefficients[1] / coefficients[2])

# lead conversion probability function vs days in contact
p_resid <- function(d) {
  if (d <= 0) {
    stop("d must be greater than 0")
  }
  resid <- p_cs * ((log(d) / log(d_max))^2 - 2 * log(d) / log(d_max) + 1)
  return(resid)
}

# plot conversion probability over range 0-45 days in contact
days_in_contact <- seq(1, 60, length.out = 500)
p_cnv_resid <- sapply(days_in_contact, function(d) p_resid(d))
cnv_vs_dic <- data.frame(days = days_in_contact, prob_cnv = p_cnv_resid)

ggplot(cnv_vs_dic, aes(x = days, y = prob_cnv)) +
  geom_line(color = "blue") +
  labs(
    title = "residual prob of lead conversion vs days in contact",
    x = "days",
    y = "prob"
  ) +
  theme_minimal()