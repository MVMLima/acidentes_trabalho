# Acidentes de Trabalho — Revista Brasileira de Saúde Ocupacional

Repositório com os dados e scripts de análise do artigo sobre **qualidade do preenchimento das notificações de acidentes de trabalho** no Brasil, submetido à **Revista Brasileira de Saúde Ocupacional (RBSO)**.

---

## Sobre o estudo

O estudo avalia a **completude** (proporção de campos preenchidos) das variáveis das fichas de notificação de acidentes de trabalho do **SINAN** (Sistema de Informação de Agravos de Notificação), abrangendo o período de **2007 a 2022**. A análise é realizada nos níveis nacional, regional, estadual e municipal, utilizando:

- **Regressão de Prais-Winsten** para análise de tendência temporal (cálculo da Variação Percentual Anual — VPA)
- **Análise de cluster** (PAM, K-means, dendrograma hierárquico) para agrupar Unidades Federativas por similaridade de completude
- **Classificação da completude** em categorias (Excelente, Bom, Regular, Ruim, Muito Ruim)
- **Análise geoespacial** por município

## Estrutura do repositório

```
.
├── Scrpit_Artigo_Acidentes.R       # Script R principal com todas as análises (855 linhas)
├── artigo_acidentes_trabalho.RData # Workspace com objetos R resultantes (~8.8 MB)
├── ArtigoAcidentesPT.pdf           # Artigo completo (português)
├── ArtigoAcidentesEN.pdf           # Artigo completo (inglês)
└── README.md
```

## Fontes de dados

- **SINAN** — Sistema de Informação de Agravos de Notificação (notificações de acidentes de trabalho)
- **SIM** — Sistema de Informação sobre Mortalidade (óbitos relacionados)
- **CADMUN** — Base de municípios com coordenadas geográficas (IBGE)

## Metodologia

### Variáveis analisadas

| Grupo | Variáveis |
|---|---|
| **Obrigatórias** | UF de notificação, data de nascimento, idade, sexo, gestante, ocupação, local do acidente, CID do acidente, parte do corpo atingida, CID da lesão, CAT, CNAE |
| **Essenciais** | Raça/cor, escolaridade, situação de trabalho, dados do empregador, horário do acidente, tipo de acidente, atendimento médico, evolução, entre outras |

### Técnicas aplicadas

1. **Cálculo de completude**: proporção de registros com valor válido (não "Ignorado") por variável, ano e UF
2. **Classificação**: Excelente (≥95%), Bom (90–94,9%), Regular (70–89,9%), Ruim (50–69,9%), Muito Ruim (<50%)
3. **Clusterização**: PAM, K-means e dendrograma hierárquico com distância euclidiana
4. **Tendência temporal**: Regressão de Prais-Winsten com transformação logarítmica, cálculo da VPA e IC95%

## Pacotes R utilizados

```r
# Manipulação e visualização
tidyverse, janitor, stringr, forcats, lubridate, slider, zoo,
tsibble, tidyquant, data.table, rio, readxl, writexl, foreign

# Análise estatística e modelagem
tidymodels, poissonreg, DescTools, gtsummary, flextable

# Clusterização
cluster, dendextend

# Regressão de Prais-Winsten
prais, plm

# Utilitários
aweek, clipr, skimr
```

## Como executar

### Pré-requisitos

- **R** ≥ 4.0
- Pacotes listados acima instalados (`install.packages("nome_do_pacote")`)

### Instruções

1. Clone o repositório:
   ```bash
   git clone https://github.com/MVMLima/acidentes_trabalho.git
   ```

2. Abra o script `Scrpit_Artigo_Acidentes.R` no RStudio ou execute via terminal:
   ```bash
   Rscript Scrpit_Artigo_Acidentes.R
   ```

> **Nota**: O script original utiliza caminhos absolutos do Windows (`D:/Episus Avançado/...`). Ajuste os caminhos dos arquivos de entrada conforme necessário.

3. Carregue o workspace salvo para acessar os objetos resultantes:
   ```r
   load("artigo_acidentes_trabalho.RData")
   ```

## Aviso de privacidade

Os arquivos `.RData` contêm dados do SINAN e SIM, que podem incluir informações sensíveis de saúde. Certifique-se de estar em conformidade com a **Lei Geral de Proteção de Dados (LGPD)** antes de compartilhar ou publicar esses dados.

## Autores

**Marcos Venicius Malveira de Lima**  
EpiSUS Avançado — Programa de Treinamento em Epidemiologia Aplicada aos Serviços do SUS  
Secretaria de Vigilância em Saúde — Ministério da Saúde

**Klauss K. S. Garcia**
The London School of Hygiene and Tropical Medicine
Department of Infectious Disease Epidemiology and International Health
Faculty of Epidemiology and Population Health

📧 e  
🐙 [MVMLima](https://github.com/MVMLima)

## Como citar

> Lima, M.V.M. *Acidentes de Trabalho: análise da completude das notificações no SINAN (2007–2022)*. Revista Brasileira de Saúde Ocupacional. No prelo.

---

© 2025 Marcos Venicius Malveira de Lima. Todos os direitos reservados.
