###################################################################################################
### Mock draft data
###################################################################################################
###################################################################################################

library("tidyverse")
library("lubridate")
library("theme538")

mocks <- read_csv("data/2019_mocks.csv") %>%
   mutate(date = mdy(date))

mocks_date_grouped <- mocks %>%
   group_by(date, name, position, school) %>%
   summarise(ADP = mean(pick),
             drafts = n()) %>%
   ungroup()

mocks_date_grouped_min_max <- mocks_date_grouped %>%
   group_by(name, position, school) %>%
   summarize(min_date = min(date),
             max_date = max(date),
             drafts = sum(drafts))

mocks_date_joined <- mocks_date_grouped %>%
   inner_join(mocks_date_grouped_min_max, by = c("name", "position", "school")) %>%
   mutate(days_from_min = date - min_date,
          days_from_max = max_date - date,
          diff_in_days = days_from_max - days_from_min)

mocks_ten_day_min_adp <- mocks_date_joined %>%
   filter(days_from_min <= 10) %>%
   group_by(name, position, school) %>%
   summarize(min_ADP = round(mean(ADP), 1))

mocks_ten_day_max_adp <- mocks_date_joined %>%
   filter(days_from_max <= 20) %>%
   group_by(name, position, school) %>%
   summarize(max_ADP = round(mean(ADP), 1))

mock_differences <- mocks_ten_day_min_adp %>%
   left_join(mocks_ten_day_max_adp, by =  c("name", "position", "school")) %>%
   mutate(ADP_diff = min_ADP - max_ADP)

risers <- mock_differences %>%
   filter(max_ADP <= 32)

write_csv(risers, "data/risers_and_fallers.csv")

### AJ Brown plot

aj_brown <- mocks %>%
   filter(name == "A.J. Brown")

### DK Metcalf plot

metcalf <- mocks %>%
   filter(name == "D.K. Metcalf")

ggplot(metcalf, aes(x = date, y = pick)) +
   theme_minimal() +
   theme(plot.title=element_text(size=16, hjust=0, vjust=-1),
         plot.subtitle=element_text(size=12, hjust=0)) +
   geom_point(data = metcalf,
              size = 4,
              alpha = .4,
              color = '#E65B2D') +
   geom_point(data = aj_brown,
              size = 4,
              alpha = .4,
              color = '#36B5BD') +
   geom_smooth(
      fill = '#CCCCCC',
      alpha = .1,
      color = '#000000',
      size = 1.5,
      span = .8,
      se = FALSE
   )  +
   geom_smooth(
      data = aj_brown,
      fill = '#CCCCCC',
      alpha = .1,
      color = '#000000',
      size = 1.5,
      span = .8,
      se = FALSE
   )  +
   scale_y_reverse(
      limit = c(70, 0),
      breaks = c(seq(
         from = 70,
         to = 0,
         by = -10
      )),
      minor_breaks = NULL
   ) +
   labs(x = "Date", y = "Pick",
        title = "A.J. Brown's draft stock has fallen while\nteammate D.K. Metcalf's has been up and down",
        subtitle = "Based on 1262 expert, media and fan mock drafts from Sept. '18 - April '19",
        caption = "Mock draft data compiled by Benjamin Robinson, @benj_robinson") +
   theme_538

### TJ Hockenson

hockenson <- mocks %>%
   filter(name == "T.J. Hockenson")

### Noah Fant

fant <- mocks %>%
   filter(name == "Noah Fant")

ggplot(fant, aes(x = date, y = pick)) +
   theme_minimal() +
   theme(plot.title=element_text(size=16, hjust=0, vjust=-1),
         plot.subtitle=element_text(size=12, hjust=0)) +
   geom_point(data = fant,
              size = 4,
              alpha = .4,
              color = '#E65B2D') +
   geom_point(data = hockenson,
              size = 4,
              alpha = .4,
              color = '#36B5BD') +
   geom_smooth(
      fill = '#CCCCCC',
      alpha = .1,
      color = '#000000',
      size = 1.5,
      span = .8,
      se = FALSE
   )  +
   geom_smooth(
      data = hockenson,
      fill = '#CCCCCC',
      alpha = .1,
      color = '#000000',
      size = 1.5,
      span = .8,
      se = FALSE
   )  +
   scale_y_reverse(
      limit = c(70, 0),
      breaks = c(seq(
         from = 70,
         to = 0,
         by = -10
      )),
      minor_breaks = NULL
   ) +
   labs(x = "Date", y = "Pick",
        title = "Hockenson is surging while Fant \nis clinging to the first round",
        subtitle = "Based on mock drafts from July '18 - April '19",
        caption = "Mock draft data compiled by Benjamin Robinson, @benj_robinson") +
   theme_538

### Josh Allen plot

allen <- mocks %>%
   filter(name == "Josh Allen")

### Quinnen Williams plot

qw <- mocks %>%
   filter(name == "Quinnen Williams")

ggplot(qw, aes(x = date, y = pick)) +
   theme_minimal() +
   theme(plot.title=element_text(size=16, hjust=0, vjust=-1),
         plot.subtitle=element_text(size=12, hjust=0)) +
   geom_point(data = qw,
              size = 4,
              alpha = .4,
              color = '#E65B2D') +
   geom_point(data = allen,
              size = 4,
              alpha = .4,
              color = '#36B5BD') +
   geom_smooth(
      fill = '#CCCCCC',
      alpha = .1,
      color = '#000000',
      size = 1.5,
      span = .8,
      se = FALSE
   )  +
   geom_smooth(
      data = allen,
      fill = '#CCCCCC',
      alpha = .1,
      color = '#000000',
      size = 1.5,
      span = .8,
      se = FALSE
   )  +
   scale_y_reverse(
      limit = c(30, 0),
      breaks = c(seq(
         from = 30,
         to = 0,
         by = -10
      )),
      minor_breaks = NULL
   ) +
   labs(x = "Date", y = "Pick",
        title = "Quinnen Williams and Josh Allen appear\nto be locks for the top 10",
        subtitle = "Based on mock drafts from Oct '18 - April '19",
        caption = "Mock draft data compiled by Benjamin Robinson, @benj_robinson") +
   theme_538


### Dexter Lawrence plot
#
# dexter <- mocks %>%
#    filter(name == "Dexter Lawrence")
#
# ggplot(dexter, aes(x = date, y = pick)) +
#    theme_minimal() +
#    theme(plot.title=element_text(size=16, hjust=0, vjust=-1),
#          plot.subtitle=element_text(size=12, hjust=0)) +
#    geom_point(size = 4,
#               alpha = .4,
#               color = '#E65B2D') +
#    geom_smooth(
#       fill = '#CCCCCC',
#       alpha = .1,
#       color = '#000000',
#       size = 1.5,
#       span = .8,
#       se = FALSE
#    )  +
#    scale_y_reverse(
#       limit = c(70, 0),
#       breaks = c(seq(
#          from = 70,
#          to = 0,
#          by = -10
#       )),
#       minor_breaks = NULL
#    ) +
#    labs(x = "Date", y = "Pick",
#         title = "Dexter Lawrence was once a high first round prospect",
#         subtitle = "Based on 1177 expert, media and fan mock drafts from July '18 - April '19",
#         caption = "Mock draft data compiled by Benjamin Robinson, @benj_robinson") +
#    theme_538
#
# ### Ed Oliver
#
# oliver <- mocks %>%
#    filter(name == "Ed Oliver")
#
# ggplot(oliver, aes(x = date, y = pick)) +
#    theme_minimal() +
#    theme(plot.title=element_text(size=16, hjust=0, vjust=-1),
#          plot.subtitle=element_text(size=12, hjust=0)) +
#    geom_point(size = 4,
#               alpha = .4,
#               color = '#E65B2D') +
#    geom_smooth(
#       fill = '#CCCCCC',
#       alpha = .1,
#       color = '#000000',
#       size = 1.5,
#       span = .8,
#       se = FALSE
#    )  +
#    scale_y_reverse(
#       limit = c(70, 0),
#       breaks = c(seq(
#          from = 70,
#          to = 0,
#          by = -10
#       )),
#       minor_breaks = NULL
#    ) +
#    labs(x = "Date", y = "Pick",
#         title = "Despite fueding with his coach, \nEd Oliver may still crack the top 10",
#         subtitle = "Based on 1595 expert, media and fan mock drafts from June '18 - April '19",
#         caption = "Mock draft data compiled by Benjamin Robinson, @benj_robinson") +
#    theme_538
