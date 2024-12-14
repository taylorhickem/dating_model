# conversion model for leads sourced through USC

# library import ---------------------------------------------------------------------------------------
library(readxl)


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

# order by first contact date
users_j1 <- users_j1[order(users_j1$first_contact), ]

users_j1$user_id <- seq_along(users_j1$username)

users_j2 <- merge(
  x = users_j1[, c("user_id", "contact_tag", "first_contact", "start")],
  y = sources$contacts[, c("contact_name", "first_sex")],
  by.x = "contact_tag",
  by.y = "contact_name",
  all.x = TRUE 
)

# drop contact name
users_j2 <- users_j2[, c(
  "user_id",
  "start",
  "first_contact",
  "first_sex"
)]

# rename fields
rename_fields_j2 <- list(
  "start" = "contact_date",
  "first_contact" = "bid_date",
  "first_sex" = "sex_date"
)

for (f in names(rename_fields_j2)) {
  colnames(users_j2)[colnames(users_j2) == f] <- rename_fields_j2[[f]]
}

# reorder fields
users_j2 <- users_j2[, c(
  "user_id",
  "bid_date",
  "contact_date",
  "sex_date"
)]

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