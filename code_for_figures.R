library(tidyverse)

# On ubuntu, to install Cairo, you need to install the following libraries:
# sudo apt-get install libcairo2-dev
# sudo apt-get install libxt-dev    
# However, cairo is only needed to save the figures as pdfs. You can remove
# `device=cairo_pdf` from the ggsave command and change the extension to png
# and it should still work.


library(Cairo)



Color <- list()
Color$Matlab_Red <- "#A2142F"
Color$Matlab_Yellow <- "#EDB120"
Color$Matlab_Blue <- "#0072BD"
Color$Matlab_Green <- "#77AC30"
Color$Matlab_Orange <- "#D95319"
Color$Matlab_LightBlue <- "#4DBEEE"
Color$Matlab_Purple <- "#7E2F8E"
Color$VT_MAROON <- "#861F41"
Color$VT_ORANGE <- "#E87722"
Color$Hokie_Stone <- "#75787B"
Color$Sustainable_Teal <- "#508590"
Color$Vibrant_Turquoise <- "#2CD5C4"
Color$Triumphant_Yellow <- "#F7EA48"
Color$Cadet_Blue <- "#003C71"
Color$Boundless_Pink <- "#CE0058"
Color$Pylon_Purple <- "#642667"
Color$Skipper_Smoke <- "#E5E1E6"
Color$Federal_Blue <- "#004791"
Color$Postal_Blue <- "#21435f"
Color$Federal_Sky_Blue <- "#7CBCE0"


# change to the directory location on your local machine.
# setwd("/home/gali/Work/acceleration-quantification-aaap-figures-code")




# Figure 1 code -----------------------------------------------------------

# load data for figure 1:
Long_data_summarized <- readRDS("Long_data_summarized.RDS")
Long_data_summarized_long <- readRDS("Long_data_summarized_long.RDS")

ggplot(data = Long_data_summarized[Long_data_summarized$CONDITION!= "Total" &
                                     Long_data_summarized$CONDITION!= "Unknown" & 
                                     Long_data_summarized$Threshold != "Not applicable" &
                                     Long_data_summarized$Threshold_Int <= 0.7 & 
                                     !is.na(Long_data_summarized$Threshold_Int),]
) + 
  geom_errorbar(aes(
    x = Threshold_New, 
    ymin = `05th`, 
    ymax = `95th`,
    # fill = Type,
    color = Type),
    stat = "identity",
    position = position_dodge(width=0.8),
    size = 0.5,
    width = 0
  ) + 
  geom_crossbar(aes(
    x = Threshold_New, 
    ymin = `25th`, 
    y = `50th`, 
    ymax = `75th`,
    # fill = Type,
    color = Type),
    fill = "white",
    stat = "identity",
    position = position_dodge(width=0.8),
    size = 1,
    width = 0.5
  ) +
  geom_point(data = Long_data_summarized_long[Long_data_summarized_long$CONDITION!= "Total" &
                                                Long_data_summarized_long$CONDITION!= "Unknown" & 
                                                Long_data_summarized_long$Threshold != "Not applicable" &
                                                Long_data_summarized_long$Threshold_Int <= 0.7 & 
                                                !is.na(Long_data_summarized_long$Threshold_Int) &
                                                Long_data_summarized_long$percentiles == "25th",],
             aes(
               x = x_position,
               y = percentile_values,
               fill = Type,
               shape = percentiles),
             stat = "identity",
             size = 2
  ) +
  geom_point(data = Long_data_summarized_long[Long_data_summarized_long$CONDITION!= "Total" &
                                                Long_data_summarized_long$CONDITION!= "Unknown" & 
                                                Long_data_summarized_long$Threshold != "Not applicable" &
                                                Long_data_summarized_long$Threshold_Int <= 0.7 & 
                                                !is.na(Long_data_summarized_long$Threshold_Int) &
                                                Long_data_summarized_long$percentiles == "75th",],
             aes(
               x = x_position,
               y = percentile_values,
               fill = Type,
               shape = percentiles),
             stat = "identity",
             size = 2
  ) +
  geom_point(data = Long_data_summarized_long[Long_data_summarized_long$CONDITION!= "Total" &
                                                Long_data_summarized_long$CONDITION!= "Unknown" & 
                                                Long_data_summarized_long$Threshold != "Not applicable" &
                                                Long_data_summarized_long$Threshold_Int <= 0.7 & 
                                                !is.na(Long_data_summarized_long$Threshold_Int) &
                                                Long_data_summarized_long$percentiles == "05th",],
             aes(
               x = x_position,
               y = percentile_values,
               fill = Type,
               shape = percentiles),
             stat = "identity",
             size = 2
  ) +
  geom_point(data = Long_data_summarized_long[Long_data_summarized_long$CONDITION!= "Total" &
                                                Long_data_summarized_long$CONDITION!= "Unknown" & 
                                                Long_data_summarized_long$Threshold != "Not applicable" &
                                                Long_data_summarized_long$Threshold_Int <= 0.7 & 
                                                !is.na(Long_data_summarized_long$Threshold_Int) &
                                                Long_data_summarized_long$percentiles == "95th",],
             aes(
               x = x_position,
               y = percentile_values,
               fill = Type,
               shape = percentiles),
             stat = "identity",
             size = 2
  ) +
  geom_point(data = Long_data_summarized_long[Long_data_summarized_long$CONDITION!= "Total" &
                                                Long_data_summarized_long$CONDITION!= "Unknown" & 
                                                Long_data_summarized_long$Threshold != "Not applicable" &
                                                Long_data_summarized_long$Threshold_Int <= 0.7 & 
                                                !is.na(Long_data_summarized_long$Threshold_Int) &
                                                Long_data_summarized_long$percentiles == "50th",],
             aes(
               x = x_position,
               y = percentile_values,
               fill = Type,
               shape = percentiles),
             stat = "identity",
             size = 2.5
  ) +
  
  facet_grid(CONDITION~.) + 
  scale_fill_manual(values = c("Acceleration" = Color$VT_ORANGE,
                               "Deceleration" = Color$VT_MAROON,
                               "Lateral negative (left)" = Color$Matlab_Blue,
                               "Lateral positive (right)" = Color$Matlab_Green),
                    labels = c("Acceleration", "Deceleration", "Lateral left", "Lateral right")
  )+
  scale_color_manual(values = c("Acceleration" = Color$VT_ORANGE,
                                "Deceleration" = Color$VT_MAROON,
                                "Lateral negative (left)" = Color$Matlab_Blue,
                                "Lateral positive (right)" = Color$Matlab_Green),
                     labels = c("Acceleration", "Deceleration",  "Lateral left", "Lateral right")
  ) +
  scale_shape_manual(values = c(
    "05th" = "triangle down filled",
    "25th" = "circle filled",
    "50th" = "diamond filled",
    "75th" = "square filled",
    "95th" = "triangle filled"
  )
  ) +
  theme_light(base_size = 12)  +
  scale_y_continuous(breaks = c(0, 1e-3, 1e-1, 1e1),
                     labels = c("0", "0.001", "0.1", "10"),
                     trans = "log10") +
  ylab("Rate of event per mile")  +
  xlab("Acceleration threshold")  +
  theme(legend.position="top", legend.box = "vertical") + 
  guides(fill = guide_legend(title = NULL, order = 0, nrow = 1, override.aes = list(shape = 22, alpha = 1, size = 5, color = "white"))) +
  guides(color = FALSE) +
  guides(shape = guide_legend(title = "Key driver percentiles:", order = 1, nrow = 1, override.aes = list(fill = "gray", color = "black", size = 3))) +
  annotation_logticks(color = "gray",
                      sides = "l",
                      size = 0.5,
                      alpha = 01,
                      # linetype = "dashed"
                      mid = unit(0.2, "cm"),
                      long = unit(0.3, "cm")
  ) +
  ggsave("plots/figure_1.pdf",
         width = 6.5, height = 8,device=cairo_pdf, units = "in")




# Figure 2, 3, 4, 5, 6 ----------------------------------------------------------------

# loading figure data

Model_Dataframe2 <- readRDS("Models_Dataframe.RDS")

Model_Dataframe <- Model_Dataframe2[Model_Dataframe2$Threshold != "0.9 g",]

Model_Dataframe$Estimate_Exp <- exp(Model_Dataframe$Estimate)

Model_Dataframe$Threshold_New <- paste0("≥ ",Model_Dataframe$Threshold)



# Roadway condition -------------------------------------------------------


data_set <- Model_Dataframe[Model_Dataframe$Category_Type == "CONDITION" &
                              Model_Dataframe$Category != "Unknown" &
                              !(Model_Dataframe$Threshold %in% c("0.8 g", "0.7 g")),]

data_set$Category <- str_replace(data_set$Category, " mph", "")
data_set$label_positon <- if_else(exp(1*data_set$Estimate)>0.75, exp(1*data_set$Estimate)-0.15, exp(1*data_set$Estimate)+0.15)
data_set$label_text <- if_else(round(exp(1*data_set$Estimate),1)==0, round(exp(1*data_set$Estimate), 2), round(exp(1*data_set$Estimate),1))
data_set$Category[data_set$Category == "< 30"] <- "≤ 30"


ggplot(data = data_set) +
  geom_hline(aes(yintercept = 1),
             color = "black",
             linetype = "dashed") + 
  geom_errorbar(aes(x = Category, 
                    ymin = exp(LCL), 
                    ymax = exp(UCL),
                    color = Variable_Type_Full
  ), position = position_dodge(width = 0.85),
  size = 01,
  width = 0.8) + 
  geom_point(aes(x = Category, y = exp(1*Estimate),
                 color = Variable_Type_Full
  ), position = position_dodge(width = 0.85),
  size = 2) + 
  geom_label(aes(x = Category, y = label_positon,
                 label = label_text,
                 color = Variable_Type_Full
  ), size = 3,
  fill = "#ffffff80",
  show_guide  = F,
  label.size=NA,
  position = position_dodge(width = 0.85)
  ) +
  ylim(0,1) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_grid(Threshold_New~Variable_Type_Full, labeller = label_wrap_gen(width=25)) +
  ylab(paste0("Rate ratios of acceleration events with respect to '< 30 mph' roadways")) +
  xlab(paste0("Roadway speed category (mph)")) +
  theme_light(base_size = 12) +
  scale_color_manual(values = c("Acceleration" = Color$VT_ORANGE,
                                "Deceleration" = Color$VT_MAROON,
                                "Lateral acceleration - Right (Positive)" = Color$Matlab_Green,
                                "Lateral acceleration - Left (Negative)" = Color$Matlab_Blue),
                     labels = c("Acceleration", "Deceleration", "Lateral left", "Lateral right")
  ) +
  scale_fill_manual(values = c("Acceleration" = "white",
                               "Deceleration" = "white",
                               "Lateral acceleration - Right (Positive)" = "white",
                               "Lateral acceleration - Left (Negative)" = "white"),
                    labels = c("Acceleration", "Deceleration", "Lateral left", "Lateral right")
  ) +
  theme_light(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position="top", legend.box = "horizontal") +
  guides(fill = guide_legend(title = "",
                             order = 1,
                             nrow = 1,
                             override.aes = list()
  )
  ) +
  guides(color = guide_legend(title = "",
                              order = 1,
                              nrow = 1,
                              override.aes = list()
  )
  ) +
  ggsave("plots/figure_2_speed_category.pdf", width = 7, height = 8.5,device=cairo_pdf, units = "in")


# Vehicle class -------------------------------------------------------


data_set <- Model_Dataframe[Model_Dataframe$Category_Type == "Vehicle_Class" &
                              Model_Dataframe$Category != "Unknown" &
                              !(Model_Dataframe$Threshold %in% c("0.8 g", "0.7 g")),]

data_set$label_positon <- if_else(exp(1*data_set$Estimate)>0.5, exp(1*data_set$Estimate)-0.25, exp(1*data_set$Estimate)+0.25)
data_set$label_positon[data_set$Threshold %in% c("0.5 g")] <- exp(1*data_set$Estimate[data_set$Threshold %in% c("0.5 g")])+0.4
data_set$label_positon[data_set$Threshold %in% c("0.6 g")] <- exp(1*data_set$Estimate[data_set$Threshold %in% c("0.6 g")])+.7


data_set$label_text <- if_else(round(exp(1*data_set$Estimate),1)==0, round(exp(1*data_set$Estimate), 2), round(exp(1*data_set$Estimate),1))


ggplot(data = data_set) +
  geom_hline(aes(yintercept = 1),
             color = "black",
             linetype = "dashed") + 
  geom_errorbar(aes(x = Category, 
                    ymin = exp(LCL), 
                    ymax = exp(UCL),
                    color = Variable_Type_Full
  ), position = position_dodge(width = 0.85),
  size = 01,
  width = 0.8) + 
  geom_point(aes(x = Category, y = exp(1*Estimate),
                 color = Variable_Type_Full
  ), position = position_dodge(width = 0.85),
  size = 2) + 
  geom_label(aes(x = Category, y = label_positon,
                 label = label_text,
                 color = Variable_Type_Full
  ), size = 3,
  fill = "#ffffff80",
  show_guide  = F,
  label.size=NA,
  position = position_dodge(width = 0.85)
  ) +
  ylim(0,NA) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_grid(Threshold_New~Variable_Type_Full, scales = "free", labeller = label_wrap_gen(width=25)) +
  ylab("Rate ratios of acceleration events with respect to 'car' vehicle class") +
  xlab("Vehicle class") +
  theme_light(base_size = 12) +
  scale_color_manual(values = c("Acceleration" = Color$VT_ORANGE,
                                "Deceleration" = Color$VT_MAROON,
                                "Lateral acceleration - Right (Positive)" = Color$Matlab_Green,
                                "Lateral acceleration - Left (Negative)" = Color$Matlab_Blue),
                     labels = c("Acceleration", "Deceleration", "Lateral left", "Lateral right")
  ) +
  scale_fill_manual(values = c("Acceleration" = "white",
                               "Deceleration" = "white",
                               "Lateral acceleration - Right (Positive)" = "white",
                               "Lateral acceleration - Left (Negative)" = "white"),
                    labels = c("Acceleration", "Deceleration", "Lateral left", "Lateral right")
  ) +
  theme_light(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position="top", legend.box = "horizontal") +
  guides(fill = guide_legend(title = "",
                             order = 1,
                             nrow = 1,
                             override.aes = list()
  )
  ) +
  guides(color = guide_legend(title = "",
                              order = 1,
                              nrow = 1,
                              override.aes = list()
  )
  ) +
  ggsave("plots/figure_4_vehicle_class.pdf", width = 7, height = 8.5, device=cairo_pdf,units = "in")




# Age range -------------------------------------------------------


data_set <- Model_Dataframe[Model_Dataframe$Category_Type == "Age_Range_Mod" &
                              Model_Dataframe$Category != "Unknown" &
                              Model_Dataframe$Category != "Did not specify" &
                              !(Model_Dataframe$Threshold %in% c("0.8 g", "0.7 g")),]
data_set$label_positon <- if_else(exp(1*data_set$Estimate)>0.5, exp(1*data_set$Estimate)-0.15, exp(1*data_set$Estimate)+0.15)

data_set$label_text <- if_else(round(exp(1*data_set$Estimate),1)==0, round(exp(1*data_set$Estimate), 2), round(exp(1*data_set$Estimate),1))
data_set$label_text_fake <- " "

ggplot(data = data_set) +
  geom_hline(aes(yintercept = 1),
             color = "black",
             linetype = "dashed") + 
  geom_vline(xintercept = c(unique(data_set$Category)[str_detect(unique(data_set$Category), "0")]),
             color = "gray80",
             lwd = 0.1,
             linetype = "solid") +
  geom_vline(xintercept = c(unique(data_set$Category)[!str_detect(unique(data_set$Category), "0")]),
             color = "gray80",
             lwd = 0.1,
             linetype = "dashed") +
  geom_errorbar(aes(x = Category, 
                    ymin = exp(LCL), 
                    ymax = exp(UCL),
                    color = Variable_Type_Full
  ), position = position_dodge(width = 0.85),
  size = 0.5,
  width = 0.7) + 
  geom_point(aes(x = Category, y = exp(1*Estimate),
                 color = Variable_Type_Full
  ), position = position_dodge(width = 0.85),
  size = 1) + 
  # geom_label(aes(x = Category, y = label_positon,
  #               label = label_text_fake,
  #               color = Variable_Type_Full,
  #               fill = Variable_Type_Full
  # ), size = 2,
  # fill = "#ffffff80",
  # show_guide  = F,
  # label.size=NA,
  # angle = 90,
  # show_guide  = F,
  # position = position_dodge(width = 0.85)
# ) +
# geom_text(aes(x = Category, y = label_positon,
#                label = label_text,
#                color = Variable_Type_Full,
#                fill = Variable_Type_Full
# ), size = 2,
# angle = 90,
# show_guide  = F,
# position = position_dodge(width = 0.85)
# ) +
ylim(0,NA) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_grid(Threshold_New~Variable_Type_Full, scales = "fixed", labeller = label_wrap_gen(width=25)) +
  ylab("Rate ratios of acceleration events with respect to '16-19' age range") +
  xlab("Age range") +
  theme_light(base_size = 12) +
  scale_color_manual(values = c("Acceleration" = Color$VT_ORANGE,
                                "Deceleration" = Color$VT_MAROON,
                                "Lateral acceleration - Right (Positive)" = Color$Matlab_Green,
                                "Lateral acceleration - Left (Negative)" = Color$Matlab_Blue),
                     labels = c("Acceleration", "Deceleration", "Lateral left", "Lateral right")
  ) +
  scale_fill_manual(values = c("Acceleration" = "white",
                               "Deceleration" = "white",
                               "Lateral acceleration - Right (Positive)" = "white",
                               "Lateral acceleration - Left (Negative)" = "white"),
                    labels = c("Acceleration", "Deceleration", "Lateral left", "Lateral right")
  ) +
  scale_y_continuous(breaks = seq(0,1.25,0.25),
                     minor_breaks = seq(0,1,0.1)) + 
  theme_light(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "white")) +
  theme(legend.position="top", legend.box = "horizontal") +
  guides(fill = guide_legend(title = "Accelerations type:",
                             order = 1,
                             nrow = 2,
                             override.aes = list()
  )
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =  0.5 , size = 8)
  ) +
  # scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.text.y = element_text(size = 8)
  ) +
  theme(axis.title.y = element_text(size = 10)
  ) +
  theme(strip.switch.pad.grid = unit(0, "cm")
  ) +
  theme(strip.switch.pad.grid = unit('0.0', "cm")) +
  guides(color = guide_legend(title = "",
                              order = 1,
                              nrow = 1,
                              override.aes = list()
  )
  ) +
  ggsave("plots/figure_3_age_range.pdf", width = 7, height = 8.5,device=cairo_pdf, units = "in")




# Location -------------------------------------------------------


data_set <- Model_Dataframe[Model_Dataframe$Category_Type == "Location_Code" &
                              Model_Dataframe$Category != "Unknown" &
                              Model_Dataframe$Category != "Did not specify" &
                              !(Model_Dataframe$Threshold %in% c("0.8 g", "0.7 g")),]

data_set$label_positon <- if_else(exp(1*data_set$Estimate)>0.5, exp(1*data_set$Estimate)-0.25, exp(1*data_set$Estimate)+0.25)

location_names <- data.frame(Category = c("New York","North Carolina", "Washington", "Indiana", "Pennsylvania","Florida"),
                             Category_New = c("Buffalo, NY","Raleigh, NC", "Seattle, WA", "Bloomington, IN", "State College, PA","Tampa, FL")
)
location_names$Category_New <- factor(location_names$Category_New, levels = c("Tampa, FL", "Buffalo, NY","Raleigh, NC", "Seattle, WA", "Bloomington, IN", "State College, PA")
)
data_set <- inner_join(data_set, location_names, by = "Category")

data_set$label_text <- if_else(round(exp(1*data_set$Estimate),1)==0, round(exp(1*data_set$Estimate), 2), round(exp(1*data_set$Estimate),1))


ggplot(data = data_set) +
  geom_hline(aes(yintercept = 1),
             color = "black",
             linetype = "dashed") + 
  geom_errorbar(aes(x = Category_New, 
                    ymin = exp(LCL), 
                    ymax = exp(UCL),
                    color = Variable_Type_Full
  ), position = position_dodge(width = 0.85),
  size = 0.5,
  width = 0.7) + 
  geom_point(aes(x = Category_New, y = exp(1*Estimate),
                 color = Variable_Type_Full
  ), position = position_dodge(width = 0.85),
  size = 1) + 
  geom_label(aes(x = Category_New, y = label_positon,
                 label = label_text,
                 color = Variable_Type_Full
  ), size = 2.75,
  fill = "#ffffff80",
  show_guide  = F,
  label.size=NA,
  position = position_dodge(width = 0.85)
  ) +
  ylim(0,NA) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  facet_grid(Threshold_New~Variable_Type_Full, scales = "free", labeller = label_wrap_gen(width=25)) +
  ylab("Rate ratios of acceleration events with respect to 'Tampa, FL' collection site") +
  xlab("Collection site") +
  theme_light(base_size = 12) +
  scale_color_manual(values = c("Acceleration" = Color$VT_ORANGE,
                                "Deceleration" = Color$VT_MAROON,
                                "Lateral acceleration - Right (Positive)" = Color$Matlab_Green,
                                "Lateral acceleration - Left (Negative)" = Color$Matlab_Blue),
                     labels = c("Acceleration", "Deceleration", "Lateral right", "Lateral left")
  ) +
  scale_fill_manual(values = c("Acceleration" = "white",
                               "Deceleration" = "white",
                               "Lateral acceleration - Right (Positive)" = "white",
                               "Lateral acceleration - Left (Negative)" = "white"),
                    labels = c("Acceleration", "Deceleration", "Lateral right", "Lateral left")
  ) +
  theme_light(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 01, vjust = 0.5)) +
  theme(legend.position="top", legend.box = "horizontal") +
  guides(fill = guide_legend(title = "",
                             order = 1,
                             nrow = 1,
                             override.aes = list()
  )
  ) +
  guides(color = guide_legend(title = "",
                              order = 1,
                              nrow = 1,
                              override.aes = list()
  )
  ) +
  ggsave("plots/figure_5_location.pdf", width = 7, height = 8.5,device=cairo_pdf, units = "in")





# Gender -------------------------------------------------------


data_set <- Model_Dataframe[Model_Dataframe$Category_Type == "Gender" &
                              Model_Dataframe$Category != "Unknown" &
                              Model_Dataframe$Category != "Did not specify" &
                              !(Model_Dataframe$Threshold %in% c("0.8 g", "0.7 g")),]

data_set$label_positon <- if_else(exp(1*data_set$Estimate)>0.5, exp(1*data_set$Estimate)-0.25, exp(1*data_set$Estimate)+0.25)


data_set$label_text <- if_else(round(exp(1*data_set$Estimate),1)==0, round(exp(1*data_set$Estimate), 2), round(exp(1*data_set$Estimate),1))


ggplot(data = data_set) +
  geom_hline(aes(yintercept = 1),
             color = "black",
             linetype = "dashed") + 
  geom_errorbar(aes(x = Category, 
                    ymin = exp(LCL), 
                    ymax = exp(UCL),
                    color = Variable_Type_Full
  ), position = position_dodge(width = 0.85),
  size = 0.5,
  width = 0.7) + 
  geom_point(aes(x = Category, y = exp(1*Estimate),
                 color = Variable_Type_Full
  ), position = position_dodge(width = 0.85),
  size = 1) + 
  geom_label(aes(x = Category, y = label_positon,
                 label = label_text,
                 color = Variable_Type_Full
  ), size = 3,
  fill = "#ffffff80",
  show_guide  = F,
  label.size=NA,
  position = position_dodge(width = 0.85)
  ) +
  ylim(0,NA) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_grid(Threshold_New~Variable_Type_Full, scales = "free", labeller = label_wrap_gen(width=25)) +
  ylab("Rate ratios of acceleration events with respect to Females") +
  xlab("Gender") +
  theme_light(base_size = 12) +
  scale_color_manual(values = c("Acceleration" = Color$VT_ORANGE,
                                "Deceleration" = Color$VT_MAROON,
                                "Lateral acceleration - Right (Positive)" = Color$Matlab_Green,
                                "Lateral acceleration - Left (Negative)" = Color$Matlab_Blue),
                     labels = c("Acceleration", "Deceleration", "Lateral right", "Lateral left")
  ) +
  scale_fill_manual(values = c("Acceleration" = "white",
                               "Deceleration" = "white",
                               "Lateral acceleration - Right (Positive)" = "white",
                               "Lateral acceleration - Left (Negative)" = "white"),
                    labels = c("Acceleration", "Deceleration", "Lateral right", "Lateral left")
  ) +
  theme_light(base_size = 12) +
  theme(legend.position="top", legend.box = "horizontal") +
  guides(fill = guide_legend(title = "",
                             order = 1,
                             nrow = 1,
                             override.aes = list()
  )
  ) +
  guides(color = guide_legend(title = "",
                              order = 1,
                              nrow = 1,
                              override.aes = list()
  )
  ) +
  ggsave("plots/figure_6_gender.pdf", width = 7, height = 8.5,device=cairo_pdf, units = "in")



# Figure 7 - Effect size --------------------------------------------------

library(readxl)
library(cowplot)

rate_ratio_data <- readxl::read_excel("rate_ratio_dataset.xlsx")

rate_ratio_data_summary <- rate_ratio_data[!(rate_ratio_data$Subcategory %in% c("Unknown", "Did not specify")),] %>% group_by(Factor, `Acceleration type`, Threshold) %>% 
  summarise(max_rr = max(`Rate ratio`),
            max_rr_cat = Subcategory[which.max(`Rate ratio`)],
            min_rr = min(`Rate ratio`),
            min_rr_cat = Subcategory[which.min(`Rate ratio`)]
  )

rate_ratio_data_summary_use <- rate_ratio_data_summary[rate_ratio_data_summary$Threshold=="≥ 0.5 g",]
rate_ratio_data_summary_use$rr_rr <- rate_ratio_data_summary_use$max_rr/rate_ratio_data_summary_use$min_rr

rate_ratio_data_summary_use <- arrange(rate_ratio_data_summary_use, rr_rr)

rate_ratio_data_summary_use$Factor_mod <- paste0(rate_ratio_data_summary_use$Factor, "\n", rate_ratio_data_summary_use$max_rr_cat, " / ", rate_ratio_data_summary_use$min_rr_cat)
order = data.frame(Factor = c("Roadway speed category", "Driver age range", "Collection site location", "Vehicle class",   "Driver gender"),
                   Order = 1:5)

rate_ratio_data_summary_use <- left_join(rate_ratio_data_summary_use, order)
rate_ratio_data_summary_use$Acceleration_type_mod <- str_replace(rate_ratio_data_summary_use$`Acceleration type`, " - ", "\n")

p1 <- ggplot(data = rate_ratio_data_summary_use) +
  geom_label(aes(y = reorder(Factor_mod, -Order), x = 1.5*rr_rr, label = round(rr_rr,1)),
             size = 3,
             fill = "#ffffff80",
             show_guide  = F,
             label.size=NA) + 
  geom_errorbar(aes(y = reorder(Factor_mod, -Order), xmax = rr_rr, xmin = 1, color = `Acceleration type`), size = 1, width = 0.1) +
  geom_point(aes(y = reorder(Factor_mod, -Order), x = rr_rr, color = `Acceleration type`), size = 3) +
  geom_point(aes(y = reorder(Factor_mod, -Order), x = 1, color = `Acceleration type`), size = 3) +
  facet_grid(Acceleration_type_mod~., scales = "free_y") +
  scale_x_continuous(trans = "log10", minor_breaks = c(seq(1,10), seq(10,100, by = 10), seq(100, 1000, by = 100)), limits = c(1, 1000)) + 
  scale_color_manual(values = c("Acceleration" = Color$VT_ORANGE,
                                "Deceleration" = Color$VT_MAROON,
                                "Lateral acceleration - Right (Positive)" = Color$Matlab_Green,
                                "Lateral acceleration - Left (Negative)" = Color$Matlab_Blue),
                     labels = c("Acceleration", "Deceleration", "Lateral left", "Lateral right")
  ) + 
  theme_light(base_size = 12) +
  theme(legend.position="top",
        legend.box = "horizontal"
  ) +
  annotation_logticks(sides = "tb", color = "gray80") +
  theme(panel.grid.minor.x = element_line(color = "gray94")) +
  guides(color = guide_legend(title = "",
                              order = 1,
                              nrow = 1,
                              override.aes = list(shape = 16, alpha = 1, size = 3, linetype = 0)
  )
  ) +
  scale_linetype(guide = F) +
  xlab("Ratio of maximum rate by minimum rate within the fixed effect (log)")+
  ylab("Fixed effect with maximum ratio subcategories") 
p2 <- p1 + theme(legend.position = "none")
le1 <- cowplot::get_legend(p1)
cowplot::plot_grid(le1,p2, nrow = 2, rel_heights = c( 0.04, 1)) +ggsave("plots/figure_7_comparing_effect_size.pdf", width = 7, height = 8.5, device=cairo_pdf,units = "in")





