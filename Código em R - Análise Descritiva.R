# TRABALHO FINAL - ANÁLISE SOBRE SAÚDE

# PACOTES ----

library(tidyverse) # Manipulação de dados
library(readxl)   # leitura de dados do excel


# LEITURA DO BANCO DE DADOS ----

dados_1 <- read_excel("dados_trabalho.xlsx")


# RENOMEAR VARIÁVEIS E CODIFICAÇÃO ----

# Nome das Variáveis
dados <- dados_1 %>%
         rename(ID = ID, IDADE = X1, Status = X2, Casa_propria = X3,
                Setor = X4, Poupanca = X5)

# Código do Status Socioeconomico
dados$Status <- dados$Status %>%
                factor(label = c("Superior","Médio","Inferior"),
                level= 1:3)


# Código da Casa Propria
dados$Casa_propria <- dados$Casa_propria %>%
                      factor(label = c("Não/Sim (Pagando)","Sim (Quitada)"),
                      level= 1:2)


# Código Setor da Cidade
dados$Setor <- dados$Setor %>%
               factor(label = c("Setor B","Setor A"),
               level= 0:1)


# Código Poupanca
dados$Poupanca <- dados$Poupanca %>%
                  factor(label = c("Não","Sim"),
                  level= 0:1)


# Intervalos de classes para geração de tabela da Idade
dados$IDADE2 <- cut(dados$IDADE, right=FALSE,
                    breaks=c(0, 10, 20, 30, 40, 50, 60,70, 80, 90), 
                        labels=c("0 |- 10", "10 |- 20", "20 |- 30",
                                 "30 |- 40","40 |- 50", "50 |- 60",
                                 "60 |- 70","70 |- 80", "80 |- 90"))


# TABELAS INDIVIDUAIS
Tab_id = table(dados$IDADE2) # Idade
Tab_id = cbind(Tab_id, prop.table(Tab_id) %>% round(2))
Tab_id  

Tab_poup = table(dados$Poupanca) # Poupanca
Tab_poup = cbind(Tab_poup, prop.table(Tab_poup) %>% round(2))
Tab_poup  

Tab_SS = table(dados$Status) # Status Socioeconomico
Tab_SS = cbind(Tab_SS, prop.table(Tab_SS) %>% round(2))
Tab_SS 

Tab_set = table(dados$Setor) # Setor
Tab_set = cbind(Tab_set, prop.table(Tab_set) %>% round(2))
Tab_set 

Tab_cp = table(dados$Casa_propria) # Casa Propria
Tab_cp = cbind(Tab_cp, prop.table(Tab_cp) %>% round(2))
Tab_cp 



# TABELAS Bidimensionais: poupanca como variavel resposta

Tab_id_pop = table(dados$IDADE2, dados$Poupanca) # Idade e Poupança
Tab_id_pop = cbind(Tab_id_pop, prop.table(Tab_id_pop,1) %>% round(2))
Tab_id_pop 

Tab_SS_pop = table(dados$Status, dados$Poupanca) # Status e Poupança
Tab_SS_pop = cbind(Tab_SS_pop, prop.table(Tab_SS_pop,1) %>% round(2))
Tab_SS_pop 

Tab_set_pop = table(dados$Setor, dados$Poupanca) # Setor e Poupança
Tab_set_pop = cbind(Tab_set_pop, prop.table(Tab_set_pop,1) %>% round(2))
Tab_set_pop 

Tab_cp_pop = table(dados$Casa_propria, dados$Poupanca) # Casa e Poupança
Tab_cp_pop = cbind(Tab_cp_pop, prop.table(Tab_cp_pop,1) %>% round(2))
Tab_cp_pop 


# IDADE - MEDIDAS E GRÁFICOS ---- 

# Idade
dados %>% 
summarise(media = mean(IDADE),
          desvio_padrao = sd(IDADE),
          cv = desvio_padrao/media,
          min = min(IDADE),
          max = max(IDADE),
          mediana = median(IDADE))

# Idade e Poupança
dados %>% 
  group_by(Poupanca) %>% 
  summarise(media = mean(IDADE),
            desvio_padrao = sd(IDADE),
            cv = desvio_padrao/media,
            min = min(IDADE),
            max = max(IDADE),
            mediana = median(IDADE))


# Histograma - Idade
ggplot(dados, aes(x = IDADE)) + # Freq. Absoluta
  geom_histogram(colour="white", alpha = 1,
                 fill = "#d14136",binwidth = 5, boundary = 0) +
  scale_x_continuous(breaks = seq(from = 0,to = 90,
                                  by = 5), limits = c(0,90)) +
  scale_y_continuous(breaks = seq(from = 0,to = 30,
                                  by = 5), limits = c(0,30)) +
  labs(x = "\n Idade (em anos) do paciente", 
       y = "Frequência Absoluta \n",
       title = "") +
  theme_minimal()


# Boxplot - Idade e Poupança
ggplot(dados, aes(x = Poupanca, y = IDADE)) +
  geom_boxplot(fill = c("#d14136"), width = 0.5, colour = "#5e050f") +
  stat_summary(fun = "mean", geom = "point", shape = 20,
               size = 3, fill = "white") +
  scale_y_continuous(breaks = seq(from = 0,to = 100,
                     by = 10), limits = c(0,100)) +
  labs(x = "\n Conta Poupança", y = "Idade (em anos) do paciente \n") +
  theme_minimal()



# STATUS SOCIOECONÔMICO - GRÁFICOS ----


# Gráfico - Status Socioeconomico
ggplot(dados) +
  geom_bar(mapping = aes(x = Status, fill = Status,
                         y = (..count..)/sum(..count..)), 
           position = "dodge2") +
  labs(title = "", x ="\n Status Socioeconômico",
       y = "Frequência Relativa \n") +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  scale_fill_manual(values = c("#329ea8","#0ead58","#d14136")) +
  scale_x_discrete(limits = rev(levels(dados$Status))) +
  guides(fill="none") + 
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(from = 0,to = 0.4,
                     by = 0.05), limits = c(0,0.4),) +
  theme_minimal()


# Gráfico - Status Socioeconomico e Poupança
ggplot(dados) + 
  geom_bar(mapping = aes(x = Status, fill = Poupanca), 
           position = "fill") +
  labs(title = "", x ="\n Status Socioeconômico",
       y = "Frequência Relativa \n") +
  theme(legend.position = "right", 
        legend.title = element_blank()) +
  scale_fill_manual(values = c("#d14136","#329ea8")) +
  scale_y_continuous(breaks = seq(from = 0,to = 1,
                     by = 0.10), limits = c(0,1),
                     labels=scales::percent) +
  scale_x_discrete(limits = rev(levels(dados$Status))) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() 




# CASA PROPRIA - GRÁFICOS ----


# Gráfico - Casa Propria
ggplot(dados) +
  geom_bar(mapping = aes(x = Casa_propria, fill = Casa_propria,
                         y = (..count..)/sum(..count..)), 
           position = "dodge2") +
  labs(title = "", x ="\n Casa Própria",
       y = "Frequência Relativa \n") +
  scale_fill_manual(values = c("#d14136","#329ea8")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(from = 0,to = 0.6,
                                  by = 0.1), limits = c(0,0.6),) +
  guides(fill="none") +
  theme_minimal()


# Gráfico - Casa Propria e Poupança
ggplot(dados) + 
  geom_bar(mapping = aes(x = Casa_propria, fill = Poupanca), 
           position = "fill") +
  labs(title = "", x ="\n Casa Própria",
       y = "Frequência Relativa \n") +
  theme(legend.position = "right", 
        legend.title = element_blank()) +
  scale_fill_manual(values = c("#d14136","#329ea8")) +
  scale_y_continuous(breaks = seq(from = 0,to = 1,
                                  by = 0.10), limits = c(0,1),
                     labels=scales::percent) +
  guides(fill = guide_legend(title = "Poupança")) + 
  theme_minimal() 



# SETOR DA CIDADE - GRÁFICOS ----

# Gráfico - Setor da cidade
ggplot(dados) +
  geom_bar(mapping = aes(x = Setor, fill = Setor,
                         y = (..count..)/sum(..count..)), 
           position = "dodge2") +
  labs(title = "", x ="\n Setor da Cidade",
       y = "Frequência Relativa \n") +
  scale_fill_manual(values = c("#329ea8","#d14136")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(from = 0,to = 0.8,
                                  by = 0.1), limits = c(0,0.8),) +
  scale_x_discrete(limits = rev(levels(dados$Setor))) +
  guides(fill="none") + 
  theme_minimal()



# Gráfico - Setor da cidade
ggplot(dados) + 
  geom_bar(mapping = aes(x = Setor, fill = Poupanca), 
           position = "fill") +
  labs(title = "", x ="\n Setor",
       y = "Frequência Relativa \n") +
  theme(legend.position = "right", 
        legend.title = element_blank()) +
  scale_fill_manual(values = c("#d14136","#329ea8")) +
  scale_y_continuous(breaks = seq(from = 0,to = 1,
                                  by = 0.10), limits = c(0,1),
                     labels=scales::percent) +
  scale_x_discrete(limits = rev(levels(dados$Setor))) +
  guides(fill = guide_legend(reverse = TRUE, title = "Poupança")) + 
  theme_minimal()
