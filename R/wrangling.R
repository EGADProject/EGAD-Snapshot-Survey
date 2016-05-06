# ---- data ----
library(rio)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(reshape2)
library(stringr)
library(maps)
library(maptools)
library(ggrepel)
library(ggplot2)
library(rgdal)
library(knitr)
library(wesanderson)
library(gridExtra)
library(grid)
library(likert)
library(viridis)
library(treemap)
library(rvest)
library(rdom)

raw_data <- import(list.files("data", pattern = "responses", full.names = TRUE)) 

cleaned_data <- raw_data %>% 
  set_colnames(tolower(names(.))) %>% 
  split(.$collector) %>% 
  map_df(function(x) {x <- melt(x, id.vars = c(1:3), variable.name = "item")}) %>% 
  set_names(c("year","name","institution","item","value")) %>% 
  mutate(q_num = str_extract(item, "\\d+[abc]*\\."),
         q_text = str_sub(item, 2 + str_length(str_match(item, "\\d+[abc]*\\.")), str_length(item)),
         q_item = ifelse(str_detect(q_text,"\\|"), str_extract(q_text, "\\|(.*)"), str_extract(q_text, "\\[(.*?)\\]"))) %>% 
  mutate(q_item = ifelse(str_detect(q_item, "\\|"), str_replace(q_item, "\\|", ""), q_item)) %>% 
  mutate(q_item = str_replace(q_item, "\\[", "")) %>% 
  mutate(q_item = str_replace(q_item, "\\]", "")) %>% 
  mutate(q_num = str_replace(q_num, "\\.", "")) %>% 
  mutate_each(funs(str_trim), q_num, q_text, q_item) %>% 
  select(-item) %>% 
  select(year, name, institution, q_num, q_text, q_item, value) %>% 
  filter(!is.na(value)) %>% 
  mutate_each(funs(str_trim))
  
# ---- Web scraping for program numbers ----                       
schools <- data_frame(institution = c("McMaster University", "Université de Moncton", "BCIT ", "University of Saskatchewan", 
  "UBC", "University of Manitoba", "Memorial University", "York University", 
  "University of Guelph", "Concordia University", "Queen's University", 
  "University of Regina", "Conestoga ITAL", "University of Victoria", 
  "Université de Sherbrooke", "UNB", "University of Windsor", 
  "UQAR", "University of Western Ontario", "Ryerson University", 
  "University of Ottawa", "Lakehead University", "McGill University", 
  "University of Alberta", "Université du Québec à Trois-Rivières", 
  "University of Waterloo", "École de technologie supérieure (ETS)", "Dalhousie University", "Polytechnique Montréal", 
  "University of Calgary"),
  name = c("Hamilton ON","Moncton NB","Burnaby BC","Saskatoon SK", "Vancouver BC", "Winnipeg MN", "St. John's NL", "Toronto ON", "Guelph ON", "Montreal QC", "Kingston ON", "Regina MN","Kitchener ON", "Victoria BC", "Sherbrooke QC", "Sackville NB", "Windsor ON", "Rimouski QC", "London ON", "Toronto ON", "Ottawa ON", "Thunder Bay ON", "Montreal QC", "Edmonton AB", "Trois-Rivières QC", "Waterloo ON", "Montreal QC", "Halifax NS", "Montreal QC", "Calgary AB"))

national_frame <- schools %>% 
  left_join(canada.cities) %>% 
  mutate_each(funs(str_trim), institution)

# ---- scraping ----
if(!file.exists("accredited_programs.html"))
  rdom::rdom("http://www.engineerscanada.ca/accredited-programs/accredited-programs-by-institution", all = TRUE, css = ".accreditation-wrapper", filename = "accredited_programs.html")

name_switch = data_frame(school = c("Alberta", "Carleton", "British Columbia", "British Columbia - Okanagan", 
                                    "British Columbia Institute of Technology", "Calgary", "Concordia", 
                                    "Conestoga College Institute of Technology and Advanced Learning", 
                                    "Dalhousie", "Ã\u0089cole de technologie supÃ©rieure", "Guelph", 
                                    "Lakehead", "Laurentian", "Laval", "Manitoba", "McGill", "McMaster", 
                                    "Memorial Newfoundland", "Moncton", "New Brunswick", "Northern British Columbia", 
                                    "Ontario Institute of Technology", "Ottawa", "Ã\u0089cole Polytechnique", 
                                    "QuÃ©bec Ã  Chicoutimi", "QuÃ©bec Ã  MontrÃ©al", "QuÃ©bec Ã  Rimouski", 
                                    "QuÃ©bec Ã  Trois-RiviÃ¨res", "QuÃ©bec en Abitibi-TÃ©miscamingue", 
                                    "QuÃ©bec en Outaouais", "Queenâ\u0080\u0099s", "Regina", "Royal Military", 
                                    "Ryerson", "Saskatchewan", "Sherbrooke", "Simon Fraser", "Toronto", 
                                    "Victoria", "Waterloo", "Western Ontario", "Windsor"),
                         institution = c("University of Alberta", "Carelton University", "UBC","UBC - Okanagan","BCIT","University of Calgary","Concordia University","Conestoga ITAL","Dalhousie University","École de technologie supérieure (ETS)", "University of Guelph", "Lakehead University", "Laurentian University", "Université Laval", "University of Manitoba", "McGill University","McMaster University","Memorial University","Université de Moncton","UNB","UNBC","UOIT","University of Ottawa","École Polytechnique de Montréal","UQAC","UQAM","UQAR","Université du Québec à Trois-Rivières","UQAT","UQAO","Queen's University","University of Regina","RMC","Ryerson University","University of Saskatchewan","Université de Sherbrooke","Simon Fraser University","University of Toronto","University of Victoria","University of Waterloo","University of Western Ontario","University of Windsor"))

ceab_progs <- read_html("accredited_programs.html") %>% 
  html_nodes(".accreditation-wrapper") %>% 
  map_df(function(x){
    
    name <- str_extract(html_text(html_nodes(x, "h3")),"(?<=\\:\\s).*")
    
    programs <- html_text(html_nodes(x, ".field-content"))
    
    count <- length(programs[grepl("\\(", programs)])
    
    df <- list(school = name, prog_count = count)
    
    return(df)
    
  }) %>% 
  filter(prog_count > 0) %>% 
  left_join(name_switch)

national_progs <- left_join(national_frame,ceab_progs) 

national_progs$prog_count[national_progs$institution=="UQAR"] <- 3
national_progs$prog_count[national_progs$institution=="York University"] <- 3
national_progs$prog_count[grepl("Trois", national_progs$institution)] <- 4
national_progs$prog_count[grepl("Polytechnique", national_progs$institution)] <- 12

# ---- mapdata ----

devtools::source_gist("https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df")

url <- "http://www.nws.noaa.gov/geodata/catalog/national/data/province.zip"
fil <- basename(url)
if (!file.exists(fil)) download.file(url, fil)
fils <- grep("shp", unzip(fil), ignore.case=TRUE, value=TRUE)
ca <- readOGR(fils, ogrListLayers(fils)[1], verbose = FALSE)

ca_map <- fortify(ca, region="NAME")



