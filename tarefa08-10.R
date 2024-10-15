#tarefa aula 08/10 - FCM

library(readxl)
library(tidyverse)
library(rcartocolor)

dados <- read_excel("dadosGGPLOT.xlsx")

carto_pal(12, "Pastel") %>% scales::show_col()
cores <- carto_pal(12, "Pastel")

dados %>% 
  mutate(
    escala_pH = case_when(
      PH < 6 ~ "Ácido",
      PH == 7 ~ "Neutro",
      TRUE ~ "Alcalino"
    )
  ) %>% 
  group_by(escala_pH) %>% 
  summarise(
    media = mean(PH)
  ) %>% 
  mutate(
    escala_pH = factor(escala_pH, levels = c("Ácido", "Neutro", "Alcalino"))
  ) %>% 
  ggplot(aes(x = escala_pH, y = media, color = escala_pH,
             fill = escala_pH))+
  geom_col()+
  scale_color_manual(values = cores[c(1, 3, 7)])+
  scale_fill_manual(values = cores[c(1, 3, 7)])+
  scale_x_discrete(breaks = c("Ácido", "Neutro", "Alcalino"))+
  labs(x = "Escala do pH", y = "Média", title = "Média dos valores de pH por escala")+
  theme_bw()+
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "plain"),
    axis.text = element_text(size = 12, face = "plain")
  )

ggsave("~/Downloads/AulaFCM/graficoTAREFA.png", device = "png")



