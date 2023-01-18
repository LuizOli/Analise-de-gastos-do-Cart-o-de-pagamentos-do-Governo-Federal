# Dados disponiveis pelo portal da transparencia
#
library(tidyverse)

message(getwd())

if (!("Dados_CPGF.rds" %in% dir())) {
  if (!dir.exists("Arquivos")) {
    dir.create("Arquivos")
    for (ano in 2013:2022) {
      for (mes in c(paste0(0, c(1:9)), 10, 11, 12)) {
        arquivo <- paste0(ano, mes)
        arquivo_zip <- paste0(arquivo, ".zip")
        destino <- paste0(paste0("/Arquivos/", arquivo_zip))
        if (!(arquivo_zip %in% dir("Arquivos"))) {
          try(expr = {
            message(paste("baixando", destino))
            Sys.sleep(runif(1, 0.5, 1))
            download.file(
              method = "auto",
              mode = "wb",
              cacheOK = F,
              url = paste0(
                "https://portaldatransparencia.gov.br/download-de-dados/cpgf/",
                arquivo_zip
              ),
              destfile = paste0(getwd(), destino)
            )
          })
        }
      }
    }
  }
  
  if (!dir.exists("CSVs")) {
    caminhos <-  paste0(getwd(), "/Arquivos/", dir("Arquivos"))
    
    lapply(caminhos, function(x) {
      unzip(zipfile = x, exdir = "CSVs")
    })
  }
  
  csvs <-
    paste0("CSVs/", dir("CSVs"))
  
  dados <-
    read_csv2(
      csvs[1],
      locale = locale(
        decimal_mark = ",",
        grouping_mark = ".",
        encoding = "Latin5"
      ),
      show_col_types = F
    )
  
  for (i in csvs[-1]) {
    dados <-
      rbind(dados,
            read_csv2(
              i,
              locale = locale(
                decimal_mark = ",",
                grouping_mark = ".",
                encoding = "Latin5"
              ),
              show_col_types = F
            ))
    message(paste("Lendo", i))
  }
  
  message(paste(colnames(dados), sep = " /n"))
  
  dados <-
    dados %>%
    mutate(
      `CÓDIGO ÓRGÃO SUPERIOR` = factor(`CÓDIGO ÓRGÃO SUPERIOR`),
      `NOME ÓRGÃO SUPERIOR` = factor(`NOME ÓRGÃO SUPERIOR`),
      `CÓDIGO ÓRGÃO` = factor(`CÓDIGO ÓRGÃO`),
      `NOME ÓRGÃO` = factor(`NOME ÓRGÃO`),
      `CÓDIGO UNIDADE GESTORA` = factor(`CÓDIGO UNIDADE GESTORA`),
      `NOME UNIDADE GESTORA` = factor(`NOME UNIDADE GESTORA`),
      `CPF PORTADOR` = factor(`CPF PORTADOR`),
      `NOME PORTADOR` = factor(`NOME PORTADOR`),
      `MÊS EXTRATO` = factor(`MÊS EXTRATO`),
      `CNPJ OU CPF FAVORECIDO` = factor(`CNPJ OU CPF FAVORECIDO`),
      `NOME FAVORECIDO` = factor(`NOME FAVORECIDO`),
      TRANSAÇÃO = factor(TRANSAÇÃO),
      `DATA TRANSAÇÃO` = lubridate::as_date(`DATA TRANSAÇÃO`, format = "%d/%m/%y")
    )
  saveRDS(dados,
          file = "Dados_CPGF.rds")
  infoRDS("Dados_CPGF.rds")
  
  message(unlink("Arquivos",
                 recursive = TRUE))
  message(unlink("CSVs",
                 recursive = TRUE))
}

dados <- read_rds("Dados_CPGF.rds")

colnames(dados) <-
  colnames(dados) %>%
  tolower() %>%
  make.names(unique = T)  %>%
  stringi::stri_trans_general("ascii")

table(dados$nome.orgao.superior) %>%
  sort(decreasing = T) 

dados %>%
  group_by(nome.orgao.superior) %>%
  summarise(nome.orgao.superior, operacoes = n()) %>%
  distinct() %>%
  ggplot(aes(x = reorder(nome.orgao.superior, operacoes), y = operacoes)) +
  geom_bar(stat = "identity") +
  labs(y = "Operações",
       x = "Orgao") +
  coord_flip()

dados %>% colnames()

dados[is.na(dados$valor.transacao),] 

# Corrigir pela inflacao

ipca<-read_csv2("ipca_2002_2022.csv")

# Total anual gasto em milhoes

dados %>%
  group_by(ano.extrato,nome.orgao.superior) %>%
  summarise(
    ano.extrato,
    nome.orgao.superior,
    Total = sum(valor.transacao) / 1000000,
    Media = mean(valor.transacao) / 1000000,
    Transacoes = n()
  ) %>%
  distinct() %>% 
  ggplot(aes(x = factor(ano.extrato),
             y = Total,
             fill = nome.orgao.superior)) +
  geom_bar(stat = "identity")+
  labs(x = "Ano",
       y = "Total Gasto",
       title="Gasto anual total em milhões de reais",
       fill = "Orgão")

# Gasto anual total PR

dados %>%
  filter(nome.orgao.superior == "Presidência da República") %>%
  group_by(ano.extrato) %>%
  summarise(
    ano.extrato,
    nome.orgao.superior,
    Total = sum(valor.transacao) / 1000000,
    Media = mean(valor.transacao) / 1000000,
    Transacoes = n()
  ) %>%
  distinct() %>% 
  ggplot(aes(x = factor(ano.extrato),
             y = Total,
             fill=nome.orgao.superior)) +
  geom_bar(stat = "identity")+
  labs(x = "Ano",
       y = "Total Gasto",
       title="Gasto anual total em milhoes de reais")+
  theme(legend.position = "none")

