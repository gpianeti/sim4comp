############## #################################################################
### CHARTS ### #################################################################
############## #################################################################

final.results <- readRDS(file = "final.results")

charts.data <- as.list(rep(NA, 8))
for (i in 1:8) {
  charts.data[[i]] <- pivot_longer(as.data.frame(final.results[[i]]), 
                                   cols = everything(), 
                                   names_to = "Model",
                                   values_to = "Error_rate")
}


chart_1 <- ggplot(data = charts.data[[1]], 
                  aes(x=Model, y=Error_rate, fill = Model)) +
  geom_boxplot() +
  labs(title = "Scenario 1", y = "Error rate") +
  guides(fill = "none") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

chart_2 <- ggplot(data = charts.data[[2]], 
                  aes(x=Model, y=Error_rate, fill = Model)) +
  geom_boxplot() +
  labs(title = "Scenario 2", y = "Error rate") +
  guides(fill = "none") + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

chart_3 <- ggplot(data = charts.data[[3]], 
                  aes(x=Model, y=Error_rate, fill = Model)) +
  geom_boxplot() +
  labs(title = "Scenario 3", y = "Error rate") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

chart_4 <- ggplot(data = charts.data[[4]], 
                  aes(x=Model, y=Error_rate, fill = Model)) +
  geom_boxplot() +
  labs(title = "Scenario 4", y = "Error rate") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

chart_5 <- ggplot(data = charts.data[[5]], 
                  aes(x=Model, y=Error_rate, fill = Model)) +
  geom_boxplot() +
  labs(title = "Scenario 5", y = "Error rate") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

chart_6 <- ggplot(data = charts.data[[6]], 
                  aes(x=Model, y=Error_rate, fill = Model)) +
  geom_boxplot() +
  labs(title = "Scenario 6", y = "Error rate") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

chart_7 <- ggplot(data = charts.data[[7]], 
                  aes(x=Model, y=Error_rate, fill = Model)) +
  geom_boxplot() +
  labs(title = "Scenario 7", y = "Error rate") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

chart_8 <- ggplot(data = charts.data[[8]], 
                  aes(x=Model, y=Error_rate, fill = Model)) +
  geom_boxplot() +
  labs(title = "Scenario 8", y = "Error rate") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())


linear_scenarios <- ggarrange(chart_1, chart_2, chart_3, nrow = 1)
nlinear_scenarios <- ggarrange(chart_4, chart_5, chart_6, nrow = 1)
fewinst_scenarios <- ggarrange(chart_7, chart_8, nrow = 1)

linear_scenarios <- annotate_figure(linear_scenarios,
                                    bottom = text_grob("Model"),
                                    left = text_grob("Error rate", rot = 90),
                                    top = text_grob("Linear Scenarios", 
                                                    face = "bold", size = 16))
nlinear_scenarios <- annotate_figure(nlinear_scenarios,
                                     bottom = text_grob("Model"),
                                     left = text_grob("Error rate", rot = 90),
                                     top = text_grob("Non-Linear Scenarios", 
                                                     face = "bold", size = 16))
fewinst_scenarios <- annotate_figure(fewinst_scenarios,
                                     bottom = text_grob("Model"),
                                     left = text_grob("Error rate", rot = 90),
                                     top = text_grob("Few Istances Scenarios", 
                                                     face = "bold", size = 16))

linear_scenarios
nlinear_scenarios
fewinst_scenarios
