# ---- constants ----
n_2013 <- cleaned_data %>% 
  group_by(year, institution) %>% 
  distinct() %>% 
  tally %>% 
  tally %>% 
  filter(year == "2013-2014") %>% 
  .$n
  
  
n_2015 <- cleaned_data %>% 
  group_by(year, institution) %>% 
  distinct() %>% 
  tally %>% 
  tally %>% 
  filter(year == "2015-2016") %>% 
  .$n

# ---- functions ----
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

preProcess <- function(df, labels) {
  
  df <- ungroup(df)
  
  df <- complete(df, year, value, fill = list(n = 0))
  
  labels <- tolower(labels)
  
  q_dummy <- data_frame(year = rep(c("2013-2014","2015-2016"), times = length(labels)),
                        value = rep(labels, each = 2),
                        n = rep(0, times = length(labels), each = 2))
  
  rank_data <- bind_rows(df, anti_join(q_dummy, df, by = c("year","value"))) 
  
  ordr <- rank_data %>% 
    group_by(value) %>% 
    summarize(avg=mean(as.numeric(n))) %>% 
    ungroup %>% 
    distinct(value) %>% 
    arrange(avg) %>% 
    .$value
  
  plot_data <- rank_data %>%  
    group_by(year) %>% 
    mutate(value = factor(value, ordr)) %>% 
    mutate(percentage = ifelse(year=="2013-2014", n/n_2013, n/n_2015))
  
  return(plot_data)
  
}


ggSlope <- function(df, wrap = 30, nudge = 0, size = 14){
  
  df <- mutate(df, percent_label = scales::percent(percentage))
  
  labels <- select(df, year, value, percentage) %>% 
    spread(year, percentage) %>% 
    select(value, `2013-2014`) %>% 
    gather(year,percentage, -value) %>% 
    mutate(value = str_wrap(value, wrap)) 
    
  ggplot(df, aes(x = year, y = percentage)) +
    geom_path(aes(group = value)) +
    geom_label(aes(label = percent_label),label.size = 0, fill = "white", family = "Oswald Light") +
    geom_text(aes(label = value), data = labels, nudge_x = nudge, family = "Oswald Light") +
    labs(x = NULL, y = NULL) +
    jkmisc::theme_jk(grid = FALSE, base_size = size) +
    theme(axis.text.y = element_blank())
     
  
}

gghBarPlot <- function(df, type = NULL, t_size = 3){
  
  max <- max(df$n)
  
  #Zissou_2 <- c(wes_palette("Zissou",5)[1],wes_palette("Zissou",5)[5])
  Viridis_2 <- c(viridis(3)[1],viridis(3)[2])
  
  if(!is.null(type)){
    plot <- ggplot(df, aes(x = value, y = percentage, fill = year)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = scales::percent(percentage), fontface = "bold"), color = "white", position = position_dodge(width = 1), hjust = 1.1, size = t_size, family = "Oswald-Light") +
      scale_y_continuous(labels = scales::percent, limits = c(0,1))
  } else {
    plot <- ggplot(df, aes(x = value, y = n, fill = year)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = n, fontface = "bold"), color = "white", position = position_dodge(width =1), hjust = 1.5, size = t_size) 
  }

   plot <- plot + scale_x_discrete(labels = function(x) str_wrap(str_to_title(x),30)) +
    scale_fill_manual(name = "Survey Year", values = Viridis_2) +
    labs(y = NULL, x = NULL) +
    coord_flip() +
    theme(
      text = element_text(color = "black", size = 20),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.background = element_rect(fill = "white"),
      strip.text.y = element_text(angle = 180),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey80", size = 0.1),
      panel.margin.y = unit(1, "lines"),
      axis.line = element_line(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      strip.background = element_blank(),
      plot.background = element_blank(),
      plot.caption = element_text(size = 10, face = "italic"))

  plot
  }

likert_plot <- function(df, flevels, p_arrange = NULL, t_size = 3, p_center = NULL, p_neutral = TRUE){
  
  factor_fun <- function(x) {
    factor(x, levels = flevels)
  }
  
  if(is.null(p_center)){
    p_center <- (length(flevels) - 1)/2 + 1
  }
  
  scale_color <- viridis(length(flevels)) 

  data <- df %>% 
    split(.$year) %>% 
    map_df(~ mutate_each(.x, funs(factor_fun), -year)) %>% 
    set_colnames(str_to_title(names(.))) %>% 
    as.data.frame()
  
  if (length(unique(data$Year)) < 2) {
    plot_data <-
      likert(data[, 2, drop = FALSE],
             nlevels = length(flevels))
    g_order <-  NULL
  } else if (ncol(data) < 2 & length(unique(data$Year)) >= 2)
  {
    plot_data <-
      likert(data[, 2, drop = FALSE],
             grouping = data$Year,
             nlevels = length(flevels))
    g_order <-  c("2015-2016","2013-2014")
  } else
  {
    plot_data <-
      likert(data[2:ncol(data)],
             grouping =  data$Year,
             nlevels = length(flevels))
    g_order <-  c("2015-2016","2013-2014")
  }
  
  plot(plot_data, colors = scale_color, panel.arrange = p_arrange, center = p_center, text.size = t_size, group.order = g_order, wrap = 30, plot.percent.neutral = p_neutral) +
    theme(
      text = element_text(color = "black", size = 20),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.background = element_rect(fill = "white"),
      strip.text.y = element_text(angle = 180),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey80", size = 0.1),
      panel.margin.y = unit(1, "lines"),
      axis.line = element_line(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      strip.background = element_blank(),
      plot.background = element_blank(),
      plot.caption = element_text(size = 10, face = "italic"))
}

ggHlollipop  <- function(df, t_size = 3){
  
  max <- max(df$n)
  
  df <- mutate(df, h_just = ifelse(year =="2013-2014", "right", "left"))
  
  #Zissou_2 <- c("yes"=wes_palette("Zissou",5)[1],"no"=wes_palette("Zissou",5)[5])
  Viridis_2 <- c("yes"=viridis(3)[1],"no"=viridis(3)[2])
  
  plot <- ggplot(df, aes(x = year, y = percentage, fill = year)) +
    geom_path(aes(x = year, y = percentage, color = value, group = value)) +
    # geom_point(aes(x = year, y = percentage, color = value), pch = 21, fill = "white") +
    geom_label(aes(label = scales::percent(percentage), fontface = "bold", hjust = h_just, color = value),fill = "white", size = t_size, label.size = 0, show.legend = FALSE) +
    annotate("text", x = 2.2, y = filter(df, year == "2015-2016", value == "no")$percentage, label = "No", color = Viridis_2[2], fontface = "bold") +
    annotate("text", x = 2.2, y = filter(df, year == "2015-2016", value == "yes")$percentage, label = "Yes", color = Viridis_2[1], fontface = "bold") +
    scale_y_continuous(labels = scales::percent, limits = c(-0.3,1.2), expand = c(0,0)) +
    scale_x_discrete(expand = c(0.1,0)) +
    scale_color_manual(name = NULL, values = Viridis_2) +
    labs(y = NULL, x = NULL) +
    theme(
      text = element_text(color = "black", size = 20),
      legend.position = "none",
      legend.key = element_blank(),
      legend.background = element_rect(fill = "white"),
      strip.text.y = element_text(angle = 180),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey80", size = 0.1),
      panel.margin.y = unit(1, "lines"),
      axis.line = element_line(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      strip.background = element_blank(),
      plot.background = element_blank(),
      plot.caption = element_text(size = 10, face = "italic"))
  
  plot
}
  
  
# ---- map ----
ggplot(ca_map) +
  geom_map(map = ca_map, aes(x = long, y = lat,map_id = id), color = "grey90", fill = "grey50") +
  # geom_label_repel(data = national_frame, aes(x = long, y = lat, label = str_wrap(institution,10)), segment.size = 0.2, size = 3, segment.color = "grey50", box.padding = unit(0, "lines")) +
  geom_point(data = national_frame, color = "grey10", fill = "blue", aes(x=long, y=lat), size = 3, pch = 21) +
  labs(x = NULL, y = NULL, title = "Responding Institutions") +
  coord_map("lambert", 44, 85) +
  theme_map() 


# ---- q4 ----

cleaned_data %>% 
    filter(q_num == "4") %>% 
    mutate(value = tolower(value)) %>% 
    mutate(class = ifelse(grepl("dean|chair|head", value), "Associate Dean", ifelse(grepl("director|directeur|program coordinator|academic coordinator", value), "Director", "Specialized Position"))) %>% 
  group_by(year, class) %>% 
  tally %>% 
  dcast(year ~ class, value.var = "n") %>% 
  rename(Year = year) %>% 
  kable(caption = "Table 1: Roles of Survey Respondents", format = "pandoc")

# ---- q5 ----

cleaned_data %>% 
  filter(q_num == "5") %>% 
  mutate(value = tolower(value)) %>% 
  mutate(value = cut(as.numeric(value), c(0,1000,3000,8000), labels = c("Small (0-<1000)","Medium (1000-3000)","Large (3000+)"))) %>% 
  group_by(year,value) %>% 
  tally %>% 
  dcast(year ~ value, value.var = "n") %>% 
  rename(Year = year) %>% 
  kable(caption = "Table 2: Relative Size of Engineering Programs", format = "pandoc")

# ---- q6 ----

cleaned_data %>% 
  group_by(year,institution) %>% 
  select(year, institution) %>% 
  distinct %>% 
  left_join(national_progs) %>% 
  select(year, institution, prog_count) %>% 
  filter(!is.na(prog_count)) %>%
  mutate(value = cut(prog_count, c(0,4,10,15), labels = c("1-4","5-10","10+"))) %>% 
  group_by(year,value) %>% 
  tally %>% 
  dcast(year ~ value, value.var = "n") %>% 
  rename(Year = year) %>% 
  kable(caption = "Table 3: Number of Engineering Programs per Institution", format = "pandoc")

# ---- q7 ----
cleaned_data %>% 
  filter(q_num == "7") %>% 
  mutate(value = tolower(value)) %>% 
  mutate(value = cut(as.numeric(value), c(0,1,2,3,4,6), labels = c("Within a Year", "Two Years","Three Years","Four Years","Five Years or More"))) %>% 
  group_by(year, value) %>% 
  tally %>% 
  dcast(year ~ value, value.var = "n", fill = 0) %>% 
  rename(Year = year) %>% 
  kable(caption = "Table 4: Time to Next CEAB Visit", format = "pandoc")

# ---- q8 ----

labels_8<- tolower(c("A full-time non-faculty assessment or accreditation co-ordinator",
"An associate dean or equivalent",
"The dean",
"A faculty member who is given relief time",
"A faculty member who is not given relief time",
"A assessment or accreditation committee", 
"Other, please specify..."))


cleaned_data %>% 
  filter(q_num == "8",is.na(q_item)) %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, value) %>% 
  tally %>% 
  preProcess(labels_8) %>% 
  gghBarPlot("p", t_size = 5)

# ---- q9 ----
labels_9 <- c("An ineffective use of time and resources",
"An exercise to be completed, something that has to be done",
"A way to help assess and improve program quality",
"A way to introduce a cultural shift in engineering education",
"Other, please specify...")

cleaned_data %>% 
  filter(q_num == "9",is.na(q_item)) %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year,value) %>% 
  tally %>% 
  preProcess(labels_9) %>% 
  gghBarPlot("p", t_size = 5)



# ---- q10 ----
labels_10 <- c("A single approach for the institution",
            "Each department is responsible for its own approach",
            "A single approach for the institution where possible but with flexibility for individual departments to tailor this approach to their needs",
            "Other, please specify...")


cleaned_data %>% 
  filter(q_num == "10",is.na(q_item)) %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year,value) %>% 
  tally %>% 
  preProcess(labels_10) %>% 
  gghBarPlot("p", t_size = 5)

# ---- q11 ----

labels_11 <- c("Identifying people to be involved",
            "Established objectives and indicators",
            "Mapped the curriculum",
            "Faculty engagement activities",
            "Assessment & data collection",
            "Analysis & interpretation of data",
            "Curriculum & program improvement",
            "Closing the loop",
            "Other, please specify...")


cleaned_data %>% 
  filter(q_num == "11") %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year,q_item) %>% 
  tally %>% 
  rename(value = q_item) %>% 
  preProcess(labels_11) %>% 
  gghBarPlot("p", t_size = 4)


# ---- q12 ----

labels_12 <- c("yes","no")

p12 <- cleaned_data %>% 
  filter(q_num == "12") %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, value) %>% 
  tally %>% 
  preProcess(labels_12) %>% 
  ggHlollipop + labs(title = "Have you collaborated with colleagues outside of your program/faculty?")
  
labels_12b <- c("Colleagues from the Faculty of Education",
                "Colleagues from the Centre for Teaching & Learning",
                "Colleagues from the Faculty of Medicine",
                "Colleagues from the School of Business",
                "Non-enginerring colleagues from another institution",
                "Engineering colleagues from another institution",
                "EGAD Project Members",
                "An Educational Developer",
                "Other, please specify...")

labels_12c <- c("Identifying people to be involved",
                "Established objectives and indicators",
                "Mapped the curriculum",
                "Faculty engagement activities (retreats, professional development regarding outcomes, etc.)",
                "Assessment & data collection",
                "Analysis & interpretation of data",
                "Curriculum & program improvement",
                "Other, please specify...")

subtitle_12b <- "Who Engineering Programs Collaborate With:"
subtitle_12c <- "Focus of Collaborative Activities:" 

q12_p <- cleaned_data %>% 
  filter(grepl("12\\w", q_num)) %>% 
  mutate(value = tolower(value)) %>% 
  split(.$q_num) %>%
    map_df(function(x){
    x %>% 
      filter(value == 1) %>% 
      group_by(year, q_item) %>% 
      tally %>% 
      rename(value = q_item) %>% 
      preProcess(., names(get(paste("labels",unique(x$q_num), sep = "_")))) %>% 
      ungroup() %>% 
      do(plots = gghBarPlot(.,"p", t_size = 3.5) + labs(title = get(paste("subtitle",unique(x$q_num), sep = "_"))))
  }) %>% 
  .$plots

legend <- g_legend(q12_p[[1]])

q12_p <- lapply(q12_p, function(x) x + theme(legend.position = "none"))


grid.arrange(p12,arrangeGrob(grobs=q12_p, nrow = 1), legend, nrow = 3,heights=c(5, 10, 1))



# ---- q13 ----
labels_13 <- c("yes","no")

p13 <- cleaned_data %>% 
  filter(q_num == "13") %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, value) %>% 
  tally %>% 
  preProcess(labels_13) %>% 
  ggHlollipop(t_size = 4) + ylim(c(0.2,0.9))

labels_13b <- c("Current students",
               "Alumni",
                "Faculty",
                "Staff",
                "Administration",
                "Professional associations",
                "Government",
                "Employers of Students (e.g. Co-op, internships)",
                "Employers of Graduates",
                "Other, please specify...")

p13b <- cleaned_data %>% 
  filter(grepl("13\\w", q_num), value == 1) %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, q_item) %>% 
  tally %>% 
  rename(value = q_item) %>% 
  preProcess(labels_13b) %>% 
  gghBarPlot("p", t_size = 5)

grid.arrange(p13, p13b, nrow = 2, heights=c(5, 10))

# ---- q14 ----
# Highlight grad attribute lecturers

levels_14 <- c("very much", "quite a bit","some","not at all","not applicable")

cleaned_data %>% 
  filter(grepl("14", q_num), !is.na(q_item)) %>% 
  mutate(value = tolower(value)) %>% 
  select(year,institution,q_item,value) %>% 
  spread(q_item, value, fill = "not applicable") %>% 
  select(-institution) %>% 
  likert_plot(levels_14, p_arrange = NULL, t_size = 5, p_center = 3, p_neutral = FALSE) + theme(text = element_text(size=18))
 

# ---- q15 ----

labels_15 <- c("More professional development for faculty and staff",
"Greater assessment staff capacity",
"Increased student participation in assessment activities",
"More faculty involved in assessing student learning outcomes",
"Stronger administrative and leadership support",
"Additional financial or staff resources",
"Technologies and analytics for outcomes assessment results at various levels to represent institutional performance",
"Greater sharing and access to assessment results across programs",
"More valid and reliable assessment measures",
"External funding",
"More opportunities for collaboration with other institutions",
"More opportunities for internal collaboration",
"Information on best practise approaches",
"Other, please specify:")


df<-cleaned_data %>% 
  filter(q_num == 15, value == 1) %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, q_item) %>% 
  tally %>% 
  rename(value = q_item) %>% 
  preProcess(labels_15) %>% 
  gghBarPlot("p", 4) + theme(text = element_text(size = 16))

# ---- q16 ----

levels_16 <- c("Intend to/have purchased",
                       "Intend to/have developed in house",
                       "Intend to/have use open-source solution",
                       "Discussing the use of",
                       "Do not intend to use",
                       "Not Applicable")

cleaned_data %>% 
  filter(q_num=="16") %>% 
  select(year,institution,q_item,value) %>% 
  spread(q_item, value, fill = "Not Applicable") %>% 
  select(-institution) %>% 
  likert_plot(levels_16, p_center = 4,t_size = 4,p_arrange = "v")

# ---- q18 ----
# Highlight others in text
levels_18 <- c("Very Satisfied",
               "Satisfied",
               "Neutral",
               "Unsatisfied",
               "Very Unsatisfied",
               "N/A")

cleaned_data %>% 
  filter(q_num=="18", is.na(q_item), !grepl("Other",value)) %>% 
  select(year,value) %>% 
  likert_plot(levels_18, p_center = 3, t_size = 5) + theme(text = element_text(size = 20), strip.text = element_blank())

# ---- q19 ----

labels_19 <- c("Every semester or term",
               "Every year",
               "Every other year",
               "Other, please specify...")

 

cleaned_data %>% 
  filter(q_num=="19", is.na(q_item)) %>%
  mutate(value = tolower(value)) %>% 
  group_by(year, value) %>% 
  tally %>% 
  preProcess(labels_19) %>% 
  gghBarPlot("p", t_size = 5)


# ---- q20 ----

labels_20 <- c("Use sampling",
               "Do not use sampling",
               "Use sampling only as necessary to achieve goals",
               "Other, please specify...")

cleaned_data %>% 
  filter(q_num=="20", is.na(q_item)) %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, value) %>% 
  tally %>% 
  preProcess(labels_20) %>% 
  gghBarPlot("p", t_size = 5)
 
# ---- q21 ----

labels_21 <- c("Yes","No")

cleaned_data %>% 
  filter(q_num=="21", is.na(q_item)) %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, value) %>% 
  tally %>% 
  preProcess(labels_21) %>% 
  ggHlollipop(t_size = 5) + expand_limits(y = c(-0.3,1))

# ---- q22 ----

labels_22 <- c("Short answer questions",
               "Essay questions",
               "Multiple choice questions",
               "Direct observation",
               "Portfolios (electronic or otherwise)",
               "Logbooks",
               "Projects",
               "Oral Presentations",
               "Open-ended problems",
               "Performance-based question (i.e. -  tasks that require students to demonstrate their  knowledge, skills, and strategies by creating a response or a product )",
               "Exams",
               "Extracurricular activities (clubs & teams)",
               "Co-op programs",
               "Laboratories",
               "Interviews",
               "Other, please specify...")

cleaned_data %>% 
  filter(q_num=="22", value == 1) %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, q_item) %>% 
  tally %>% 
  rename(value = q_item) %>% 
  preProcess(labels_22) %>% 
  gghBarPlot("p", t_size = 5)

# ---- q23 ----

levels_23 <- c("Very Much",
               "Quite a Bit",
               "Some",
               "Not at All")

cleaned_data %>% 
  filter(q_num=="23", !is.na(q_item)) %>% 
  select(year, institution, q_item, value) %>% 
  spread(q_item, value) %>%
  select(-institution) %>% 
  likert_plot(levels_23, p_center = 3.5, p_arrange = "v",t_size = 5) + theme(text = element_text(size = 20))

# ---- q24 ----

labels_24 <- c("Yes","No")

p24 <- cleaned_data %>% 
  filter(q_num=="24") %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, value) %>% 
  tally %>% 
  preProcess(labels_24) %>% 
  ggHlollipop(t_size = 4) + labs(title = str_wrap("Do you have any evidence that outcome-based continuous program improvement has made a difference in student learning at your institution? "),25) +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
  # gghBarPlot("p")

labels_24b <- c("Graduate employment rates",
                "Increased quality of learning",
                "Resultant curriculum changes",
                "National student surveys (NSSE, CLASSE, etc)",
                "Alumni surveys, focus groups or interviews",
                "Employer surveys, focus groups or interviews",
                "Student statements of learning outcomes achievement",
                "Portfolios (purposeful collection of student work showcasing achievement of graduate attributes)",
                "Other, please specify...")

p24_b <- cleaned_data %>% 
  filter(q_num=="24b", value == 1) %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, q_item) %>% 
  tally %>% 
  rename(value = q_item) %>% 
  preProcess(labels_24b) %>% 
  gghBarPlot("p", t_size = 5) + scale_y_continuous(labels = scales::percent, breaks = c(0,1)) + labs(title = "What is the nature of the evidence collected?") +
  theme(text = element_text(size = 16),plot.title = element_text(size = 14, hjust = 0.5))

grid.arrange(p24, p24_b, layout_matrix = rbind(c(1),c(2),c(2)))

# ---- q25 ----
labels_25 <- c("Student learning outcomes statements", 
"Assessment plans",
"Assessment resources",
"Current assessment activities",
"Evidence of student learning",
"Improvement plans",
"Examples of use of evidence of student leanring",
"Impact of use of assessment data",
"Other, please specify...")

cleaned_data %>% 
  filter(q_num=="25", value == 1) %>% 
  mutate(value = tolower(value)) %>% 
  group_by(year, q_item) %>% 
  tally %>% 
  rename(value = q_item) %>% 
  preProcess(labels_25) %>% 
  gghBarPlot("p", t_size = 5)

# ---- q26 ----
# Definitely look at the other
levels_26 <- data_frame(category = c("none","0.25","0.5","1",">1"),
                        label = c("0 FTE","0.25 FTE","0.5 FTE","1 FTE",">1 FTE"))

plot_data <- cleaned_data %>% 
  filter(q_num=="26", value == 1) %>% 
  mutate(category = str_extract(q_text,"\\[(.*?)\\]")) %>% 
  mutate(category = str_replace(category, "\\[", "")) %>% 
  mutate(category = str_replace(category, "\\]", "")) %>% 
  mutate(item = str_extract(q_text,"\\|(.*?)\\[")) %>% 
  mutate(item = str_replace(item, "\\|", "")) %>% 
  mutate(item = str_replace(item, "\\[", "")) %>% 
  mutate(item = str_trim(item)) %>% 
  group_by(item, category) %>% 
  tally %>% 
  ungroup %>% 
  complete(item,category) %>% 
  filter(category != "none")

colors <- data_frame(item = unique(plot_data$item),
                     color = viridis(6))

plot_data <- plot_data %>% 
  left_join(colors, by="item") %>% 
  left_join(levels_26, by="category") %>% 
  mutate(item = str_wrap(str_to_title(item), 10))

treemap(
  plot_data,
  index = c("item", "label", "n"),
  vSize = "n",
  vColor = "color",
  type = "color",
  title="",
  title.legend="",
  fontsize.labels = c(20,18,16),
  fontcolor.labels = "white",
  lowerbound.cex.labels = 0.1,
  bg.labels = 0,
  border.col = "white",
  border.lwds = c(5,0.5,0.5),
  position.legend = "none",
  align.labels = list(c("left","top"),c("center","bottom"), c("right", "bottom")),
  drop.unused.levels = TRUE)

# ---- q28 ----

labels_28 <- c("Very Much",
               "Quite a Bit",
               "Some",
               "Not at All",
               "NA")

cleaned_data %>% 
  filter(q_num=="28") %>% 
  select(year,value) %>%
  likert_plot(labels_28, p_center = 3.5, t_size = 5, p_neutral = FALSE)

