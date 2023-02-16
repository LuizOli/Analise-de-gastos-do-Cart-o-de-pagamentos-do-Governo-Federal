# Análise de gastos do Cartão de pagamentos do Governo Federal
 
Análise dos gastos do cartão corporativo da presidência da republica disponíveis no [Repositório de informações Classificadas e Desclassificas](https://www.gov.br/secretariageral/pt-br/acesso-a-informacao/informacoes-classificadas-e-desclassificadas), produzido por meio do RMarkdown e disponível em HTML.


## Metodologia

Utilizando os dados disponíveis sobre os gastos do cartão corporativo da Presidência da República do período entre 2003 e 2022 foi feita a correção dos valores dos gastos considerando a inflação por meio do IPCA trazendo-os para a data presente de 18 de janeiro de 2023 e comparou-se **como, quando e com o que** foram gastos os recursos entres os presidentes **Lula, Dilma, Temer e Bolsonaro.**

## Bibliotecas utilizadas

* Tidyverse: [https://www.tidyverse.org/]
* RMarkdown: [https://rmarkdown.rstudio.com/]
* Knitr: [https://www.rdocumentation.org/packages/knitr/versions/1.41]

## Observações

O arquivo "codigo_r.r" possui o mesmo conteúdo apenas com os códigos utilizados.

O ajuste de inflação foi elaborado utilizando o indicador [IPCA](https://ibge.gov.br/Precos_Indices_de_Precos_ao_Consumidor/IPCA/)

## Idéias 

* Relações entre empresas, quais empresas são reorrentes?
* Comparação entre presidentes em relação aos sub-elementos das despesas, quem gastou com o quê?
* Incluir dados do portal da transparência, possivel omição de gastos nos dados disponibilizados, dados estão completos?
* Dashboard interativo com os gastos
* Reunir informações da RFB utilizando os CNPJS disponibilizados
* Raspagem de notícias relevantes à relação a gastos altos
* Elaborar vizualização de mapa para os gastos efetuados em território nacional
* Reunir informções sobre os portadores dos cartões, quais foram os CPFs que mais gastaram, quando, em que, onde e porque.
* Análisar o CIDIC quais foram os artigos mais usados, encontrar o porquê da classificação sigilosa

