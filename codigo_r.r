# Analise de dados de gastos no Cartão de Pagamentos do Governo Federal

#Bibliotecas
library(tidyverse)
library(rmarkdown)
library(knitr)

#Dados
if (!("cpgf_2003_2022.csv" %in% dir())) {
  download.file(url =
                  "https://www.gov.br/secretariageral/pt-br/acesso-a-informacao/informacoes-classificadas-e-desclassificadas/Planilha12003a2022.csv",
                destfile = "cpgf_2003_2022.csv")
}

gastos <-
  read.csv2("cpgf_2003_2022.csv",
            check.names = T,
            colClasses = "character",
            fileEncoding = "windows-1252")

colnames(gastos)<-tolower(colnames(gastos))

tibble(Linhas = dim(gastos)[1],
       Colunas = dim(gastos)[2]) %>% 
  kable(align = "l",caption = "Dimenções dos dados")

# Temos pouco mais de 113 mil registos de compras com o cartão corporativo entra 2003 e 2022. Sobre estes gastos temos as informações da data de pagamento, CPF restrito do sevidor, CNPJ ou CPF do servidor que efetuou o gasto, nome do fornecedor, valor da compra, tipo e sub-elemento da despesa que diz respeito a regulação que autoriza o gasto.


n_letras <- function(x) {
  str_length(x)
}

apply(gastos, c(1, 2), n_letras) %>%
  apply(., 2, function(x) {
    sum(x == 0)
  }) %>%
  tibble(Coluna = names(.),
         `Valores faltantes` = .) %>% 
  kable(align = "l",capiton = "Colunas com problemas")


# Observa-se 434 linhas onde a variavel CPF ou CNPJ do fornecedor são invalidas e 2 linhas vazias essa são as ultimas duas linhas que foram lidas incorretamente.


gastos <- gastos[-c(113342, 113341), ]


### Adicionando o presidente Ativo

# Adicionando o presidente ativo nas datas dos gastos:

gastos <-
  gastos %>%
  mutate(
    data.pgto = lubridate::as_date(data.pgto, format = "%d/%m/%Y"),
    Ano = lubridate::year(data.pgto),
    mes_num = lubridate::month(data.pgto)
  ) %>%
  mutate(
    presidente = case_when(
      Ano >= 2003 & Ano <= 2010 ~ "Lula",
      Ano >= 2011 & Ano <= 2016 ~ "Dilma",
      Ano >= 2017 & Ano <= 2019 ~ "Temer",
      Ano >= 2020 & Ano <= 2022 ~ "Bolsonaro",
    )
  )

gastos$presidente[gastos$Ano==2016 & gastos$mes_num>=9]<-"Temer"

gastos <-
  gastos %>% 
  mutate(presidente = factor(
    presidente,
    levels = c("Lula", "Dilma", "Temer", "Bolsonaro"),
    ordered = T
  ))


## Gastos sem CNPJ de fornecedor

# Observando por ano quantas transções no total não tem o CPF ou CNPJ do fornecedor


sem_cnpj_fornecedor<-
  gastos %>%
  mutate(tamanho = str_length(cpf.cnpj.fornecedor)) %>%
  filter(tamanho == 0) 

sem_cnpj_fornecedor %>%
  group_by(Ano, presidente) %>%
  summarise(`CPF ou CNPJ faltante` = n()) %>%
  ggplot(aes(x = factor(Ano), y = `CPF ou CNPJ faltante`, fill = presidente)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Total de transações",
       x = "Ano",
       title = "Total de transações sem CNPJ do fornecedor entre 2003 e 2022")

# Corrigindo pela inflação utilizando o IPCA temos o total gasto em transações sem CNPJ do fornecedo por ano asseguir: 

  
ipca <-
  read.csv2("ipca_2002_2022.csv")

sem_cnpj_fornecedor <-
  sem_cnpj_fornecedor %>%
  mutate(
    valor_num = str_remove_all(valor, pattern = "R\\$") %>%
      str_replace_all(pattern = "\\.", replacement = "") %>%
      str_replace_all(pattern = "\\,", replacement = "\\.") %>%
      str_squish() %>%
      as.numeric()
  ) %>%
  mutate(
    data.pgto = lubridate::as_date(data.pgto, format = "%d/%m/%y"),
    Ano = lubridate::year(data.pgto),
    mes_num = lubridate::month(data.pgto)
  ) %>%
  left_join(ipca) %>%
  mutate(valor_corrigido = valor_num * fator_inflacao_2022)

sem_cnpj_fornecedor %>%
  group_by(Ano,presidente) %>%
  summarise(total_gasto = sum(valor_corrigido)) %>% 
  ggplot(aes(x = factor(Ano),
             y = total_gasto / 1000,
             fill = presidente)) +
  geom_bar(stat = "identity") +
  # scale_x_discrete(drop=F)+
  coord_flip()+
  scale_y_continuous(n.breaks = 10)+
  labs(y = "Total de gastos em milhares de Reais",
       x = "Ano",
       title = "Total de gastos corrigidos sem CNPJ do fornecedor entre 2003 e 2022")

## Gastos corrigidos pela inflação

# Observando o total de gastos corrigidos pela inflação entres os presidentes temos: 

  
gastos<-
  gastos %>% 
  left_join(ipca) %>% 
  mutate(
    valor_num = 
      str_remove_all(valor, pattern = "R\\$") %>%
      str_replace_all(pattern = "\\.", replacement = "") %>%
      str_replace_all(pattern = "\\,", replacement = "\\.") %>%
      str_replace_all(pattern = " ", replacement = "") %>%
      str_replace_all(pattern = "-", replacement = "-0") %>%
      str_squish() %>% as.numeric()
  ) %>%
  mutate(valor_corrigido = valor_num * fator_inflacao_2022)  

gastos %>% 
  group_by(Ano,presidente) %>% 
  summarise(total_gasto= sum(valor_corrigido)) %>% 
  ggplot(aes(x = factor(Ano), y = total_gasto/1000000, fill = presidente))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_y_continuous(n.breaks = 10)+
  labs(title = "Total anual de gastos, corrigidos pela inflação",
       x = "Ano",
       y = "Gastos em milhões de reais",
       fill= "Presidentes")+
  theme(legend.position = "bottom")


### Meses com os maiores gastos totais

# Somando os gastos mensamente e identificando os meses com os maiores totais de cada presidente:

gastos %>% 
  group_by(Ano,mes_num,presidente) %>% 
  summarise(total_gasto_mes = sum(valor_corrigido)) %>% 
  ungroup() %>% 
  group_by(presidente) %>% 
  filter(total_gasto_mes == max(total_gasto_mes)) %>% 
  transmute(presidente,Ano,mes_num,
            gasto_maximo_mes = total_gasto_mes) %>% 
  ggplot(aes(x = presidente,
             y = gasto_maximo_mes/1000000,
             fill = presidente))+
  geom_bar(stat="identity")+
  labs(title = "Gasto mensal máximo por presidente",
       x = "Presidente",
       y = "Gasto máximo em milhões de reais")+
  theme(legend.position = "none")



### Gasto diário máximo


(diario_maximo <-
    gastos %>%
    group_by(data.pgto, presidente) %>%
    summarise(total_gasto_dia = sum(valor_corrigido)) %>%
    ungroup() %>%
    group_by(presidente) %>%
    filter(total_gasto_dia == max(total_gasto_dia)) %>%
    transmute(presidente, data.pgto,
              gasto_maximo_dia = total_gasto_dia)) %>% 
  kable(align = "l",caption = "Gasto diario máximo por presidente")
diario_maximo %>%
  ggplot(aes(
    x = presidente,
    y = gasto_maximo_dia / 1000000,
    fill = presidente
  )) +
  geom_bar(stat = "identity") +
  labs(title = "Gasto diário máximo por presidente",
       x = "Presidente",
       y = "Gasto máximo em milhões de reais") +
  theme(legend.position = "none")

#### Dias mais caros de cada presidente

##### Lula

# No dia 12 de dezembro de 2008 o Presidente Lula gastou 1,64 milhões de reais, gasto quatro vezes maior que o segundo colocado o presidente Bolsonaro no dia 23 de dezembro de 2021 gastando pouco menos de 403 mil reais.

gastos %>% 
  filter(data.pgto == lubridate::as_date("2008/12/17")) %>% 
  group_by(subelemento.de.despesa) %>% 
  summarise(presidente,
            subelemento.de.despesa,
            total_gasto_dia= sum(valor_corrigido)) %>% 
  unique() %>% 
  arrange(-total_gasto_dia) %>% 
  ggplot(aes(x = reorder(subelemento.de.despesa,total_gasto_dia),y=total_gasto_dia/1000,
             fill = subelemento.de.despesa))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(n.breaks = 10)+
  labs(x = "Categoria",
       y = "Total gasto corrigido em milhares de reais",
       title = "Categorias de gastos do dia mais caro")+
  theme(legend.position = "none")


# Nesse dia, o presidente Lula gastou incriveis 1,4 milhoes em hospedagem, é rasovel pensar que seja o total de varias estadias em diferentes hoteis, mas sem acesso as notas ficais é impossivel dizer. Dentre os fonecedores de hospedagem desse dia temos:
  

gastos %>% 
  filter(data.pgto == lubridate::as_date("2008/12/17")) %>% 
  filter(subelemento.de.despesa == "HOSPEDAGENS") %>% 
  group_by(nome.fornecedor) %>% 
  summarise(presidente,
            nome.fornecedor,
            total_gasto_dia= sum(valor_corrigido)) %>% 
  unique() %>% 
  arrange(-total_gasto_dia) %>% 
  kable(align = "l", caption = "Empresas e total gasto no dia mais caro")

# A empresa Sauipe Park é um resort localizado na Bahia. Nesse dia existe uma [noticia](https://g1.globo.com/Noticias/Mundo/0,,MUL926846-5602,00-LULA+PEDE+PARA+LIDERES+LATINOAMERICANOS+NAO+SEREM+SERVIS+AOS+EUA.html) que faz referência a 1ª Cúpula da América Latina e do Caribe na Costa do Sauipe.

#### Dilma


gastos %>% 
  filter(data.pgto == lubridate::as_date("2014/07/15")) %>% 
  group_by(subelemento.de.despesa) %>% 
  summarise(presidente,
            subelemento.de.despesa,
            total_gasto_dia= sum(valor_corrigido)) %>% 
  unique() %>% 
  arrange(-total_gasto_dia) %>% 
  ggplot(aes(x = reorder(subelemento.de.despesa,total_gasto_dia),y=total_gasto_dia/1000,
             fill = subelemento.de.despesa))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(n.breaks = 10)+
  labs(x = "Categoria",
       y = "Total gasto corrigido em milhares de reais",
       title = "Categorias de gastos do dia mais caro")+
  theme(legend.position = "none")


# A presidente Dilma gastou 327 mil reais em hospedagens no dia 15 de julho de 2014. As empresas que compões este gasto são:
  

gastos %>% 
  filter(data.pgto == lubridate::as_date("2014/07/15")) %>% 
  filter(subelemento.de.despesa == "HOSPEDAGENS") %>% 
  group_by(nome.fornecedor) %>% 
  summarise(presidente,
            nome.fornecedor,
            total_gasto_dia= sum(valor_corrigido)) %>% 
  unique() %>% 
  arrange(-total_gasto_dia) %>% 
  kable(align = "l", caption = "Empresas e total gasto no dia mais caro")


# Marina de Iracema Park é um resort em Fortaleza no estado de Ceará. No dia 14 de julho encontramos uma [noticia](https://g1.globo.com/ceara/noticia/2014/07/apos-reuniao-com-dilma-putin-chega-fortaleza-para-vi-cupula-do-brics.html) da passagem de Putin por Fortaleza.

#### Temer

# O dia mais caro do presidente Temer foi em 23 de janeiro de 2018.


gastos %>% 
  filter(data.pgto == lubridate::as_date("2018/01/23")) %>% 
  group_by(subelemento.de.despesa) %>% 
  summarise(presidente,
            subelemento.de.despesa,
            total_gasto_dia= sum(valor_corrigido)) %>% 
  unique() %>% 
  arrange(-total_gasto_dia) %>% 
  ggplot(aes(x = reorder(subelemento.de.despesa,total_gasto_dia),y=total_gasto_dia/1000,
             fill = subelemento.de.despesa))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(n.breaks = 10)+
  labs(x = "Categoria",
       y = "Total gasto corrigido em milhares de reais",
       title = "Categorias de gastos do dia mais caro")+
  theme(legend.position = "none")

# O sub-elemento mais caro foi de Serviços de apio administrativo, tecnico e operacional com total de 314 mil reais. As empresas beneficiadas são:

  
gastos %>% 
  filter(data.pgto == lubridate::as_date("2018/01/23")) %>% 
  filter(subelemento.de.despesa == "SERV.DE APOIO ADMIN.,TECNICO E OPERACIONAL") %>% 
  group_by(nome.fornecedor) %>% 
  summarise(presidente,
            nome.fornecedor,
            total_gasto_dia= sum(valor_corrigido)) %>% 
  unique() %>% 
  arrange(-total_gasto_dia) %>% 
  kable(align = "l", caption = "Empresas e total gasto no dia mais caro")


# A PrivaPort SA é uma empresa de aviação em Meyrin, Geneva. Diz respeito a uma viagem a Zurique fotos podem sem encontradas no [flickr do Palacio do planalto](https://flickr.com/photos/palaciodoplanalto/albums/72157690912980441)

#### Bolsonaro

# O dia mais caro do presidente Bolsonaro foi em 23 de dezembro de 2021.


gastos %>% 
  filter(data.pgto == lubridate::as_date("2021/12/23")) %>% 
  group_by(subelemento.de.despesa) %>% 
  summarise(presidente,
            subelemento.de.despesa,
            total_gasto_dia= sum(valor_corrigido)) %>% 
  unique() %>% 
  arrange(-total_gasto_dia) %>% 
  ggplot(aes(x = reorder(subelemento.de.despesa,total_gasto_dia),y=total_gasto_dia/1000,
             fill = subelemento.de.despesa))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(n.breaks = 10)+
  labs(x = "Categoria",
       y = "Total gasto corrigido em milhares de reais",
       title = "Categorias de gastos do dia mais caro")+
  theme(legend.position = "none")

# O sub-elemento mais caro foi de Hospedagens com total de 305 mil reais. As empresas beneficiadas são:

gastos %>% 
  filter(data.pgto == lubridate::as_date("2021/12/23")) %>% 
  filter(subelemento.de.despesa == "HOSPEDAGENS") %>% 
  group_by(nome.fornecedor) %>% 
  summarise(presidente,
            nome.fornecedor,
            total_gasto_dia= sum(valor_corrigido)) %>% 
  unique() %>% 
  arrange(-total_gasto_dia) %>% 
  kable(align = "l", caption = "Empresas e total gasto no dia mais caro")

# A Hotur São Paulo é uma empresa de hotelaria com alguns hoteis o pricipal fica no Guarujá em São Paulo com o nome de Ferraretto Hotel. Nesse dia existe uma [noticia](https://www.cartacapital.com.br/politica/em-semana-decisiva-em-brasilia-bolsonaro-decide-esticar-as-ferias/) fazendo referencia ao presidente dançando funk em uma lancha.


### Gasto médio por mês 

# Como alguns presidentes permaneceram no poder por mais tempo é interessante comparar a media de gastos por mês de cada um, assim temos:

gastos %>%
  group_by(Ano, mes_num, presidente) %>%
  summarise(total_gasto_mes = sum(valor_corrigido)) %>%
  ungroup() %>%
  group_by(presidente) %>%
  summarise(media_gastos_mes = mean(total_gasto_mes)) %>%
  ggplot(aes(
    x = presidente,
    y = media_gastos_mes / 1000000,
    fill = presidente
  )) +
  geom_bar(stat = "identity") +
  labs(title = "Média mensal de gastos por presidente, corrigidos pela inflação",
       x = "Presidente",
       y = "Gasto mensal médio em milhões de reais") +
  theme(legend.position = "none") +
  scale_y_continuous(n.breaks = 10)

### Panorama semanal de gastos



## Conclusão

# Pelos dados disponiveis é possivel inferir que o presidente Temer foi o mais parcimonioso com os gastos do Cartão de Pagamentos do Governo Federal apresentando a menor média de gastos mensais. 
# 
# Algumas informações interessantes sobre dos dias com maiores gastos dos presidentes encontramos para:
#   
#   * A presidencia de Lula um gasto com hotelaria possivelmente ligada a 1ª Cúpula da América Latina e do Caribe; 
# * A presidencia da Dilma temos um gasto com a viagem de Putin a Fortaleza;
# * A presidencia de Temer temos gastos com aviação de uma viagem a Zurich na Suiça;
# * A presidencia de Bolsonaro uma despeza com hotel em Guaruja possivelmente ligada a uma folga do presidente.
# 
# 




