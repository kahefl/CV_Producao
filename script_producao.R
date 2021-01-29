library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)
library(patchwork)

setwd("C:\\Users\\kahel\\OneDrive\\Documents\\Coisas do R\\scripts\\Campo_Verde\\producao")

producao <- read_excel('culturas.xlsx')

prod <- producao %>% 
  filter(anos > "1989-12-31") %>% 
  select(anos, Total, `Soja (em grão)`,
         `Algodão herbáceo (em caroço)`,
         `Milho (em grão)`) %>% 
  mutate(anos=as.Date(anos)) %>%
  mutate(restante = Total - c(`Milho (em grão)`+`Soja (em grão)`+ `Algodão herbáceo (em caroço)`))

p1 <- prod %>%
  select(anos,Total) %>% 
  mutate(cult = rep("Todos os cultivos",29)) %>% 
  rename(valor = Total)

p2 <- prod %>%
  select(anos, `Milho (em grão)`) %>% 
  rename(valor =`Milho (em grão)`)%>% 
  mutate(cult = rep("Milho (em grão)",29))

p3 <- prod %>%
  select(anos, `Soja (em grão)`) %>% 
  rename(valor =`Soja (em grão)`)%>% 
  mutate(cult = rep("Soja (em grão)",29))

p4 <- prod %>%
  select(anos, `Algodão herbáceo (em caroço)`) %>% 
  rename(valor = `Algodão herbáceo (em caroço)`)%>% 
  mutate(cult = rep("Algodão herbáceo (em caroço)",29))

p5 <- p1 %>% 
  mutate(valor = as.numeric(p1$valor - c(p2$valor+p3$valor+p4$valor))) %>% 
  mutate(cult = rep("Demais cultivos",29))

p6 <- p1 %>% 
  mutate(valor = as.numeric(c(p2$valor+p3$valor+p4$valor))) %>% 
  mutate(cult = rep("Algodão, Milho e Soja", 29))

p7 <- rbind(p1,p6)

p8 <- rbind(p2,p3,p4)


graf_1 <- ggplot(p7, aes(x=anos, y=valor, fill=cult)) + 
  geom_col(position = "dodge")+
  scale_y_continuous(limits = c(0, 500000),labels = label_number(suffix = " ton"))+
  scale_x_date(limits = as.Date(c("1989-12-31","2020-12-31")),
               date_labels = "%Y",date_breaks = "5 years")+
  theme_excel_new()+
  labs(title="Produção agrícola",
       x=" ", y=" ")+
  theme(axis.text = element_text(colour = "black", size = 10),
        legend.text = element_text(size = 9.5, colour = "black"),
        plot.title = element_text(hjust = 0.5, colour = "black"))+
  scale_fill_manual(values = c("#FF4500", "#008000"))

graf_2 <- ggplot(p8, aes(x=anos, y=valor,color=cult))+
  geom_line(size=1.2)+
  scale_y_continuous(limits = c(0, 300000),labels = label_number(suffix = " ton"))+
  scale_x_date(limits = as.Date(c("1990-12-31","2019-12-31")),
               date_labels = "%Y", date_breaks = "5 years")+
  labs(title="Principais cultivos", x=" ", y=" ")+
  theme_excel_new()+
  theme(axis.text = element_text(colour = "black", size = 10),
        legend.text = element_text(size = 9.5, colour = "black"),
        plot.title = element_text(hjust = 0.5, colour = "black"))+
  scale_colour_manual(values = c("#CD853F","#FDE910","#FF8C00"))

graf_1 + graf_2 + plot_layout(ncol = 1,heights = c(1, 3))

ggsave("CV_cultivos.jpg", width = 8, height = 7.5)
