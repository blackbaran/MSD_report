# install.packages("openxlsx")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("scales")
library(openxlsx)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
source("plot_themes.R")

# reading data

Raport_AE1 = openxlsx::read.xlsx(xlsxFile = "./CASE.xlsx",sheet = 2)

Raport_AE2 = bind_rows(openxlsx::read.xlsx(xlsxFile = "./CASE.xlsx",sheet = 3)) %>%
  filter(TEMPLATE.RECODTYPE == "Email_Template_vod")

data_to_analize = Raport_AE1 %>%
  filter(`Sent.Email:.Sent.Email.Name` %in% Raport_AE2$EMAIL.NAME)

list_of_graphs = list()

# prepare graphs
# plot one
# Przedstaw najczęściej wysyłany AE
data_plot_1 = data_to_analize %>%
  group_by(Email.Template) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ungroup() %>%
  mutate(Email.Template = str_replace(Email.Template, "Approved email ", "#")) %>%
  mutate(Email.Template = factor(Email.Template, levels = Email.Template))

plot_1 = ggplot(data = data_plot_1,
                aes(x = Email.Template, y = n, 
                    fill=factor(ifelse(Email.Template == "#61", "Highlighted","Normal")))) +
  geom_bar(stat = "identity",
           width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = prettyNum(n, big.mark = " ")),
            family = 'Lato bold',
            size = 4,
            colour = 'grey29',
            vjust = -0.25) + 
  labs(
    title = "Top 10 most sent email templates",
    x = "E-mail template",
    y = "Number of sent emails template") +
  theme_barowy_siatka +
  scale_fill_manual(name = "Email.Template", values=c("#F97A02","grey50"))+
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = " "), limits = c(0, 750))

list_of_graphs[["plot_1"]] =  plot_1


# plot 2 
# Przedstaw najczęściej wysłany i dostarczony AE
data_plot_2 = data_to_analize %>%
  filter(Status == "Delivered") %>%
  group_by(Email.Template) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ungroup() %>%
  mutate(Email.Template = str_replace(Email.Template, "Approved email ", "#")) %>%
  mutate(Email.Template = factor(Email.Template, levels = Email.Template))


plot_2 = ggplot(data = data_plot_2,
                aes(x = Email.Template, y = n, 
                    fill=factor(ifelse(Email.Template == "#61", "Highlighted","Normal")))) +
  geom_bar(stat = "identity",
           width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = prettyNum(n, big.mark = " ")),
            family = 'Lato bold',
            size = 4,
            colour = 'grey29',
            vjust = -0.25) + 
  labs(
    title = "Top 10 most sent & delivered email templates",
    subtitle = ("Status of email templates is Delivered"), 
    x = "E-mail template",
    y = "Number of sent emails template") +
  theme_barowy_siatka +
  scale_fill_manual(name = "Email.Template", values=c("#F97A02","grey50"))+
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = " "), limits = c(0, 750))

list_of_graphs[["plot_2"]] =  plot_2


# plot 3
# Przedstaw AE z największym OR (licz spośród emaili z ponad 10 dostarczonymi mailami) - unikatowe otwarcia

data_plot_3 = data_to_analize %>%
  filter(Status == "Delivered") %>%
  group_by(Email.Template) %>%
  dplyr::summarise(
    n_sended = n(),
    n_open = sum(Opened)) %>%
  filter(n_sended > 10) %>%
  mutate(open_rate = n_open / n_sended) %>%
  arrange(desc(open_rate)) %>%
  head(10) %>%
  ungroup() %>%
  mutate(Email.Template = str_replace(Email.Template, "Approved email ", "#")) %>%
  mutate(Email.Template = factor(Email.Template, levels = Email.Template))


plot_3 = ggplot(data = data_plot_3,
                aes(x = Email.Template, y = open_rate, 
                    fill=factor(ifelse(Email.Template == "#167", "Highlighted","Normal")))) +
  geom_bar(stat = "identity",
           width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(open_rate * 100, 0), "%")),
            family = 'Lato bold',
            size = 4,
            colour = 'grey29',
            vjust = -0.25) + 
  labs(
    title = "Top 10 most email templates\nwith the highest open rate",
    subtitle = ("from more than 10 delivered email templates "), 
    x = "E-mail template",
    y = "Open rate") +
  theme_barowy_siatka +
  scale_fill_manual(name = "Email.Template", values=c("#F97A02","grey50")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

list_of_graphs[["plot_3"]] =  plot_3


# plot 4
# Przedstaw AE z największym CTOR (licz spośród emaili, które są "klikalne" i mają więcej niż 5 otwarć) - unikatowe kliknięcia

# data_plot_4 = data_to_analize %>%
#   filter(Status == "Delivered") %>%
#   filter(Clicked > 0) %>%
#   group_by(Email.Template) %>%
#   dplyr::summarise(
#     n_sended = n(),
#     n_unique_open = sum(Opened),
#     n_unique_clicks = sum(Clicked))
#   # mutate(ctor = n_unique_clicks / n_unique_open)
# 
# 
#   # arrange(desc(open_rate)) %>%
#   # head(10) %>%
#   # ungroup() %>%
#   # mutate(Email.Template = str_replace(Email.Template, "Approved email ", "#")) %>%
#   # mutate(Email.Template = factor(Email.Template, levels = Email.Template))
# 


# plot 5
# Przedstaw najrzadziej wysyłane AE
# dla wszystkich wysyłek i do Specialty 10 jako Primary Specialty

data_plot_5 = data_to_analize %>%
  group_by(Email.Template) %>%
  count() %>%
  arrange(desc(n)) %>%
  tail(15) %>%
  ungroup() %>%
  arrange(n) %>%
  mutate(Email.Template = str_replace(Email.Template, "Approved email ", "#")) %>%
  mutate(Email.Template = factor(Email.Template, levels = Email.Template))



plot_5 = ggplot(data = data_plot_5,
                aes(x = Email.Template, y = n, 
                    fill=factor(ifelse(Email.Template %in% c("#153", "#163", "#166", "#48", "#53"), "Highlighted","Normal")))) +
  geom_bar(stat = "identity",
           width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = prettyNum(n, big.mark = " ")),
            family = 'Lato bold',
            size = 4,
            colour = 'grey29',
            vjust = -0.25) + 
  labs(
    title = "The least frequently\nsended E-mail templates",
    subtitle = "In general",
    x = "E-mail template",
    y = "Number of sent emails template") +
  theme_barowy_siatka +
  scale_fill_manual(name = "Email.Template", values=c("#F97A02","grey50"))+
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = " "), limits = c(0, 3.5))

list_of_graphs[["plot_5"]] =  plot_5

# dla primary specjalist
Primary_Specialty = Raport_AE2 %>%
  filter(ACCOUNT.PRIMARY.SPECIALTY == "Specialty 10")

data_to_primary_specialist = data_to_analize %>%
  filter(`Sent.Email:.Sent.Email.Name` %in% Primary_Specialty$EMAIL.NAME) %>%
  group_by(Email.Template) %>%
  count() %>%
  arrange(desc(n)) %>%
  tail(8) %>%
  ungroup() %>%
  arrange(n) %>%
  mutate(Email.Template = str_replace(Email.Template, "Approved email ", "#")) %>%
  mutate(Email.Template = factor(Email.Template, levels = Email.Template))
  


plot_6 = ggplot(data = data_to_primary_specialist,
       aes(x = Email.Template, y = n, 
           fill=factor(ifelse(Email.Template %in% c("#60", "#67", "#85", "#95", "#97"), "Highlighted","Normal")))) +
  geom_bar(stat = "identity",
           width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = prettyNum(n, big.mark = " ")),
            family = 'Lato bold',
            size = 4,
            colour = 'grey29',
            vjust = -0.25) + 
  labs(
    title = "The least frequently\nsended E-mail templates",
    subtitle = "Sended to Specialty 10 as a Primary Speciality",
    x = "E-mail template",
    y = "Number of sent emails template") +
  theme_barowy_siatka +
  scale_fill_manual(name = "Email.Template", values=c("#F97A02","grey50"))+
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = " "), limits = c(0, 3.5))

list_of_graphs[["plot_6"]] =  plot_6

# Przedstaw wizualnie funnel dla wszystkich AE w BU2
# - ile wysłano
# - jaki % został dostarczony
# - jaki % został otwarty
# - jaki % został kliknięty

data_to_funnel = Raport_AE2 %>%
  filter(`EMPLOYEE.BUSINESS.UNIT.(C)` == "BU 1")
  
n_send = nrow(data_to_funnel)
n_delivered = nrow(data_to_funnel %>% filter(STATUS == "Delivered"))
n_opended = nrow(data_to_funnel %>% filter(TOTAL.OPENS > 0))
n_clicks = nrow(data_to_funnel %>% filter(TOTAL.CLICKS > 0))

data_to_plot_funnel = data_frame(n_send = n_send,
                                 n_delivered = n_delivered,
                                 n_opended = n_opended,
                                 n_clicks = n_clicks) %>%
  gather(key = "key", value = "value", n_send:n_clicks) %>%
  mutate(proc = value / 11170)

legend_data = data_frame(key = c("Number of sended emails",
                                 "% delivered emails",
                                 "% opended emails",
                                 "% clicked emails"),
                         value = c(0, 0, 0, 0)) %>%
  mutate(key = factor(key, levels = c("% clicked emails", "% opended emails", "% delivered emails", "Number of sended emails")))

plot_7 = ggplot()+
  geom_bar(data = data_to_plot_funnel %>% filter(key == "n_send"),
           aes(x = "x", y = value), stat = "identity", 
           width = 0.6, fill = "red", alpha = 0.2) +
  annotate(geom = "text", x = "x", y = 11200, 
           label = paste0("Number of sended\nemails:\n11 170"), hjust = 0) +
  geom_bar(data = data_to_plot_funnel %>% filter(key == "n_delivered"),
           aes(x = "x", y = value), stat = "identity", 
           width = 0.58, fill = "green", alpha = 0.4) +
  geom_text(data = data_to_plot_funnel %>% filter(key == "n_delivered"),
            aes(x = "x", y = value, label = paste0(round(proc * 100, 0), "%")),
            hjust = 1, 
            family = 'Lato bold',
            size = 4,
            colour = 'grey29') +
  geom_bar(data = data_to_plot_funnel %>% filter(key == "n_opended"),
           aes(x = "x", y = value), stat = "identity", 
           width = 0.22, fill = "yellow", alpha = 0.6) +
  geom_text(data = data_to_plot_funnel %>% filter(key == "n_opended"),
            aes(x = "x", y = value, label = paste0(round(proc * 100, 0), "%")),
            hjust = 1, 
            family = 'Lato bold',
            size = 4,
            colour = 'grey29') +
  geom_bar(data = data_to_plot_funnel %>% filter(key == "n_clicks"),
           aes(x = "x", y = value), stat = "identity",
           width = 0.03, fill = "blue", alpha = 0.8) +
  geom_text(data = data_to_plot_funnel %>% filter(key == "n_clicks"),
            aes(x = "x", y = value, label = paste0(round(proc * 100, 0), "%")),
            hjust = 1, 
            family = 'Lato bold',
            size = 4,
            colour = 'white') +
  coord_flip() +
  theme_barowy_siatka +
  labs(
    title = "Funnel of the process of email life") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_bar(data = legend_data, aes(x = "x", y = value, fill = key), 
           stat = "identity") +
  guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
  scale_fill_manual(values = c("Number of sended emails" = "red",
                               "% delivered emails" = "green",
                               "% opended emails" = "yellow",
                               "% clicked emails" = "blue")) +
  scale_y_continuous(limits = c(0, 14000))

list_of_graphs[["plot_7"]] =  plot_7

saveRDS(list_of_graphs, "list_of_plots.rds")
