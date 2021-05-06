# Libraries

library(tidyverse)
library(vroom)
library(data.table)
library(lubridate)

# Load the data

patent_col_types <- list(
 id         = col_character(),
 date       = col_date("%Y-%m-%d"),
 num_claims = col_double()
)

patent_dt <- vroom(
  file      = "00_data/Patent_data_reduced/patent.tsv",
  delim     = "\t",
  col_types = patent_col_types,
  na        = c("", "NA", "NULL")
)

setDT(patent_dt)

patent_assignee_col_types <- list(
  patent_id   = col_character(),
  assignee_id = col_character()
)

patent_assignee_dt <- vroom(
  file      = "00_data/Patent_data_reduced/patent_assignee.tsv",
  delim     = "\t",
  col_types = patent_assignee_col_types,
  na        = c("", "NA", "NULL")
)

setDT(patent_assignee_dt)

assignee_col_types <- list(
  id           = col_character(),
  type         = col_character(),
  organization = col_character()
)

assignee_dt <- vroom(
  file      = "00_data/Patent_data_reduced/assignee.tsv",
  delim     = "\t",
  col_types = assignee_col_types,
  na        = c("", "NA", "NULL")
)

setDT(assignee_dt)

uspc_col_types <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_double()
)

uspc_dt <- vroom(
  file      = "00_data/Patent_data_reduced/uspc.tsv",
  delim     = "\t",
  col_types = uspc_col_types,
  na        = c("", "NA", "NULL")
)

setDT(uspc_dt)

# Question 1

us_top_10_companies_dt <- merge(x = patent_assignee_dt, y = assignee_dt, by.x = "assignee_id", by.y = "id")[
  type == 2,][!is.na(patent_id) || !is.na(organization),][
    , .(count = .N), by = organization
  ][order(-count), ][1:10,][]

us_top_10_companies_dt

# Question 2

us_top_10_new_august <- merge(x = patent_dt, y = patent_assignee_dt, by.x = "id", by.y = "patent_id")[
  assignee_dt, on = c(assignee_id = "id")
][type == 2,][
  !is.na(id) || !is.na(organization),][, month := month(ymd(date))][month == 8, ][
    , .(count = .N), by = organization
  ][order(-count), ][1:10,][]

us_top_10_new_august
  
# Question 3

top_10_worldwide <- merge(x = patent_assignee_dt, y = assignee_dt, by.x = "assignee_id", by.y = "id")[
  !is.na(patent_id) || !is.na(organization),][, .(count = .N), by = organization][order(-count),][
    !is.na(organization),][1:10,][]

top_10_worldwide


top_5_uspto <- merge(x = uspc_dt, y = patent_assignee_dt, by = "patent_id")[
  assignee_dt, on = c(assignee_id = "id")][top_10_worldwide, on = "organization"][
    , .(count = .N), by = mainclass_id][order(-count),][1:5,][]

top_5_uspto
  