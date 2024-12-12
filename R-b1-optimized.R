library(dplyr)

# Initialize an empty data frame
base_brevets <- data.frame(
  firm_name = character(),     
  n_patents = integer(),       
  ipc_main_code = character(), 
  ipc_main_desc = character(), 
  ipc_second_code = character(),
  ipc_second_desc = character(),
  addr_city_main = character(),
  addr_dept_main = character(),
  stringsAsFactors = FALSE
)

### Combine IPC files into a single data frame
files <- list.files(pattern = "^EN_ipc.*\\.txt$", recursive = TRUE)
ipc_combined <- files %>%
  lapply(read.table, sep = "\t", header = FALSE, stringsAsFactors = FALSE) %>%
  bind_rows() %>%
  setNames(c("IPC_code14", "Description"))

# Extract IPC4 codes and remove incomplete ones
ipc_combined <- ipc_combined %>%
  mutate(IPC_code4 = substr(IPC_code14, 1, 4)) %>%
  filter(nchar(IPC_code4) == 4)

# Keep unique IPC4 with first description
ipc_unique <- ipc_combined %>%
  distinct(IPC_code4, .keep_all = TRUE) %>%
  select(IPC_code4, Description)

### Read and preprocess OECD data
data_OECD <- read.csv("202202_EPO_App_reg_small.txt", sep = ",", stringsAsFactors = FALSE)
data_OECD <- data_OECD %>%
  mutate(year = as.numeric(substr(app_nbr, 3, 6))) %>%
  filter(ctry_code == "FR", year >= 2010, year <= 2020)

data_IPC <- read.csv("202202_EPO_IPC_small.txt", sep = ",", stringsAsFactors = FALSE) %>%
  mutate(IPC_code4 = substr(IPC, 1, 4))

# Merge OECD and IPC data
data_combined <- data_OECD %>%
  left_join(data_IPC, by = "appln_id") %>%
  select(-IPC)

### Normalize company names
data_combined <- data_combined %>%
  mutate(app_name = tolower(app_name),
         app_name = gsub(",? sa| inc\\.| sarl|\\(société par actions simplifiée\\)|\\(s\\.?a\\.?s\\.?\\)", "", app_name, ignore.case = TRUE),
         app_name = iconv(app_name, from = "UTF-8", to = "ASCII//TRANSLIT"),
         app_name = gsub("[-–]", "", app_name),
         app_name = gsub(",", "", app_name),
         app_name = trimws(app_name))

### Aggregate data for base_brevets
base_brevets <- data_combined %>%
  group_by(app_name) %>%
  summarize(
    n_patents = n(),
    addr_city_main = names(which.max(table(city))),
    addr_dept_main = names(which.max(table(reg_code))),
    ipc_main_code = names(which.max(table(IPC_code4))),
    ipc_second_code = names(sort(table(IPC_code4), decreasing = TRUE)[2]),
    .groups = 'drop'
  ) %>%
  mutate(
    ipc_main_desc = ipc_unique$Description[match(ipc_main_code, ipc_unique$IPC_code4)],
    ipc_second_desc = ipc_unique$Description[match(ipc_second_code, ipc_unique$IPC_code4)]
  )

# Inspect the resulting data frame
edit(base_brevets)

