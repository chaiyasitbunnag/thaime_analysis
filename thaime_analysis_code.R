library(dplyr)
library(ggplot2)
library(xml2)
library(httr)
library(magrittr)
library(jsonlite)
library(stringr)
library(tidyr)
library(readr)
library(reshape2)
extrafont::loadfonts(device = "win")

# --- set local settings --- #
options(scipen = 999)
Sys.setlocale("LC_ALL", "Thai")

# --- test json to df --- #
x <- "http://nscr.nesdb.go.th/wp-admin/admin-ajax.php?action=wp_ajax_ninja_tables_public_action&table_id=12653&target_action=get-all-data&default_sorting=old_first&skip_rows=0&limit_rows=0&chunk_number=0"
x <- fromJSON(x)
x <- x[2]

# store to df
data.frame(project_name = x$value[2] %>% unname(),
           budget = x$value[3] %>% unname(),
           agency_name = x$value[4] %>% unname(),
           ministry_name = x$value[5] %>% unname(),
           province = x$value[6] %>% unname()) %>%
   tibble()


# --- get all data --- #
# tables stored in ajax (urls found in Network tab)
# create chunk urls 1-15 to loop over
chunk_urls <- paste0("http://nscr.nesdb.go.th/wp-admin/admin-ajax.php?action=wp_ajax_ninja_tables_public_action&table_id=12653&target_action=get-all-data&default_sorting=old_first&skip_rows=0&limit_rows=0&chunk_number=",0:15)
# create empty table to store data after loop is finished
collected_tables <- data.frame()

for(i in 1:length(chunk_urls)) {
   print(i)
   x <- chunk_urls[i]
   x <- fromJSON(x)
   x <- x[2]
   data_temp <- data.frame(project_name = x$value[2] %>% unname(),
                           budget = x$value[3] %>% unname(),
                           agency_name = x$value[4] %>% unname(),
                           ministry_name = x$value[5] %>% unname(),
                           province = x$value[6] %>% unname())
   collected_tables <- rbind(collected_tables, data_temp)
}

# --- clean data --- #
# assign to new var
collected_tables_2 <- unique(collected_tables)

# names
# 1. ministry
collected_tables_2$ministry_name <- trimws(collected_tables_2$ministry_name)
collected_tables_2$ministry_name <- gsub("กระทรวง", "", collected_tables_2$ministry_name)
collected_tables_2 <- collected_tables_2[collected_tables_2$ministry_name != "", ]

# 2. agency
collected_tables_2$agency_name <- trimws(collected_tables_2$agency_name)
collected_tables_2$agency_name[str_detect(collected_tables_2$agency_name, "ท้องถิ่น$") & !str_detect(collected_tables_2$agency_name, "\\/")] <- "กรมส่งเสริมการปกครองท้องถิ่น"

# 3. province
collected_tables_2$province[str_detect(collected_tables_2$province, "^จังหวัด")] <- gsub("^จังหวัด", "", collected_tables_2$province)
collected_tables_2$province <- trimws(collected_tables_2$province)
collected_tables_2$province[collected_tables_2$province == "กรุงเทพมหานคร"] <- "กรุงเทพฯ"


#  4. budget
collected_tables_2$budget <- trimws(collected_tables_2$budget)
collected_tables_2$budget <- gsub(",", "", collected_tables_2$budget)
collected_tables_2$budget <- parse_number(collected_tables_2$budget)


# top proposed ministry
n_m <- 
   collected_tables_2 %>% 
   count(ministry_name, sort = T) %>% 
   head(20)

# plot top proposed ministry
collected_tables_2 %>% 
   count(ministry_name, sort = T) %>% 
   head(20) %>% 
   ggplot(., aes(x = reorder(ministry_name, n), y = n))+
   geom_col(width = 0.7, fill = "#03e8fc")+
   geom_text(aes(label = n), size = 3, family = "Consolas", fontface = "bold")+
   coord_flip()+
   theme_minimal()+
   theme(panel.grid = element_blank(),
         axis.title = element_text(family = "Consolas", face = "bold"),
         axis.text.x = element_blank())+
   labs(x = "Ministry Name", y = "Number of Project Briefs")

# 1. top ten proposed ministry by agency
top10_ministry <-
   collected_tables_2 %>% 
   count(ministry_name, sort = T) %>% 
   head(5)

top10_ministry_data <- collected_tables_2[collected_tables_2$ministry_name %in% top10_ministry$ministry_name, ]

preprocessed <- 
top10_ministry_data %>%
   dplyr::group_by(ministry_name, agency_name) %>% 
   dplyr::summarise(n = n()) %>% 
   top_n(10, n) %>%
   ungroup() %>% 
   dplyr::arrange(ministry_name, n) %>%
   dplyr::mutate(r = row_number())

# Plot the data
ggplot(preprocessed, aes(x = r, y = n)) +
   geom_segment(aes(xend = r, yend = 0)) +
   geom_col(width = 0.5, fill = "#03e8fc")+
   geom_text(aes(label = n), size = 3, family = "Consolas", fontface = "bold", hjust = 0.1)+
   scale_x_continuous(breaks = preprocessed$r, labels = preprocessed$agency_name) +
   facet_wrap(~ ministry_name, scales = "free_y", nrow = 10) +
   coord_flip()+
   theme_minimal()+
   theme(panel.grid = element_blank(),
         axis.title.x = element_text(family = "Consolas",
                                     face = "bold"),
         axis.text.x = element_blank())+
   labs(y = "Number of Projects")

# 2. top provinces (budget) from local (dla)
collected_tables_2 %>% 
   count(province, sort = T) %>% 
   View()

collected_tables_2$province[str_detect(collected_tables_2$province, "^จังหวัด")] <- gsub("^จังหวัด", "", collected_tables_2$province)
collected_tables_2$province <- trimws(collected_tables_2$province)

dla <- collected_tables_2 %>% filter(agency_name == "กรมส่งเสริมการปกครองท้องถิ่น")

dla_top20_province <- 
dla %>% 
   group_by(province) %>% 
   summarise(budget = sum(budget)/1000000) %>% 
   arrange(-budget) %>% 
   head(20)

ggplot(dla_top20_province, aes(x = reorder(province, budget), y = budget)) +
   geom_col(width = 0.5, fill = "#03e8fc") +
   coord_flip() +
   theme_minimal() +
   theme(panel.grid = element_blank(),
         axis.title.x = element_text(family = "Consolas",
                                   face = "bold"),
         axis.title.y = element_blank()) +
   labs(y = "Total Budget Proposed to DLA (million)")

# 3. Bangkok
collected_tables_2 %>%
   filter(str_detect(province, "กรุงเทพ")) %>% 
   View()

collected_tables_2$province[collected_tables_2$province == "กรุงเทพมหานคร"] <- "กรุงเทพฯ"
collected_tables_2$province[collected_tables_2$province == "กทม."] <- "กรุงเทพฯ"
collected_tables_2$province[collected_tables_2$province == "กทม"] <- "กรุงเทพฯ"
collected_tables_2$province[collected_tables_2$province == "กรุงเทพ"] <- "กรุงเทพฯ"
collected_tables_2$province[collected_tables_2$province == "กรุงเทพมหานครและปริมณฑล"] <- "กรุงเทพฯ และปริมณฑล"

bkk <- collected_tables_2[str_detect(collected_tables_2$province, "กรุงเทพ"), ]
bkk$budget <- round(bkk$budget / 1000000, 2)
bkk$project_name <- gsub("โครงการ", "", bkk$project_name)

bkk %>% group_by(province, project_name) %>% summarise(budget = sum(budget)) %>% arrange(-budget) %>% select(project_name)
ggplot(., aes(x = reorder(project_name, budget), y = budget)) +
   geom_col(width = 0.3, fill = "#03e8fc") +
   coord_flip() +
   scale_y_continuous(breaks = seq(0, 1000, 100)) +
   geom_text(aes(label = budget, col = province), size = 4, family = "Consolas", fontface = "bold", hjust = 0.2)+
   #facet_wrap(~ province, scales = "free_y") +
   theme_minimal() +
   theme(panel.grid = element_blank(),
         axis.title.x = element_text(family = "Consolas",
                                     face = "bold"),
         axis.title.y = element_blank(),
         axis.text.x = element_blank(),
         legend.title = element_blank()) +
   labs(y = "Project Budget Proposed for Bangkok (million)")

# 4. top proposed budget project from all
# summary statistic
collected_tables_2 %>% 
   mutate(budget = round(budget/1000000, 0)) %>% 
   summarise(min_budget = min(budget, na.rm = T),
             max_budget = max(budget, na.rm = T),
             mean_budget = mean(budget, na.rm = T),
             median_budget = median(budget, na.rm = T))
   # there is project budgeted 240 billion

# see the max budget details
collected_tables_2[which.max(collected_tables_2$budget), ] %>%
   mutate(budget = round(budget/1000000,0)) %>%
   melt(id.vars = c("project_name")) %>%
   View()

# check how many >= 100 billions
collected_tables_2 %>%
   filter(budget >= 10^11) %>% 
   mutate(budget = round(budget/1000000, 0)) %>% 
   melt(id.vars = c("project_name")) %>%
   arrange(project_name) %>% 
   View()

# check how many >= 10 billions
mega <- 
collected_tables_2 %>%
   filter(budget >= 10^10) %>% 
   mutate(budget = round(budget/1000000, 0))

# top budget ex mega
collected_tables_2 %>%
   filter(budget <= 10^9) %>%
   mutate(budget = round(budget/1000000, 0)) %>% 
   ggplot(., aes(x = budget)) +
   geom_histogram(bins = 40, binwidth = 10) +
   xlim(c(0, 300)) +
   ylim(c(0, 10000))
   #scale_x_continuous(breaks = seq(0, 300, 50))

# bucketing budgets
collected_tables_2 %>%
   filter(!is.na(budget), budget > 0) %>% 
   mutate(budget_bin = case_when(budget < 10^7 ~ "< 10 Millions",
                                 budget >= 10^7 & budget < 10^8 ~ "10 - 99.99 Millions",
                                 budget >= 10^8 & budget < 10^9 ~ "100 - 999.99 Millions",
                                 budget >= 10^9 & budget < 10^10 ~ "1 - 9.9 Billions",
                                 budget >= 10^10 & budget < 10^11 ~ "10 - 99.99 Billions",
                                 budget >= 10^11 ~ ">= 100 Billions",
                                 TRUE ~ "Others")) %>%
   count(budget_bin, sort = T)

# save data
write.csv(collected_tables_2, "project_briefs_data.csv", row.names = F)
# import data
#x <- read.csv("https://raw.githubusercontent.com/chaiyasitbunnag/thaime_analysis/master/project_briefs_data.csv", header = T, stringsAsFactors = F)
