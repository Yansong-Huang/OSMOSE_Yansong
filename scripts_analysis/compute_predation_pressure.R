library(ggplot2)
library(dplyr)
library(viridis)
library(RColorBrewer)

# load data
results_path <- file.path("outputs/results_1111","Base_simu","Base", "output", "CIEM")
predation_path <- file.path(results_path,"/Trophic/Yansong_predatorPressure_Simu0.csv")
pred_pressure_example <- readr::read_csv(predation_path, skip = 1)
# transform to long format
pred_pressure_example_long <- pred_pressure_example %>%
  tidyr::pivot_longer(cols = c(3:18), names_to = "Predator", values_to = "Biomass_t")
# create colomn "year"
pred_pressure_example_long <- pred_pressure_example_long %>%
  dplyr::mutate(Year=Time+2001) %>%
  select(-Time)

# set species order
pred_pressure_example_long$Predator <- factor(pred_pressure_example_long$Predator, levels = unique(pred_pressure_example_long$Predator))

# set colour palette
colour_palette <- c(viridis_pal(option = "C")(4),brewer.pal(12, "Paired"))

pred_pressure_plot <- ggplot(pred_pressure_example_long, aes(x = Year, y = Biomass_t, fill = Predator)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Prey, scales = "free") + 
  scale_fill_manual(values = colour_palette) +
  theme_minimal()+
  ggtitle("predation pressure")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"))
ggsave(
  file.path("figures", "publication", "predation_pressure.png"),
  pred_pressure_plot,
  width = 16, height = 12, dpi = 600
)
