# PACOTES ----

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, janitor, summarytools, kableExtra, patchwork, ggpubr, esquisse, glue, GGally, lmtest)

summarytools::st_options(lang = "pt")

# pacman::p_load(tidyverse,  janitor, stargazer,  sjmisc, summarytools,
#                kableExtra, moments, ggpubr, formattable, gridExtra, 
#                glue, corrplot, sessioninfo, readxl, writexl, ggthemes,
#                patchwork,  plotly, lmtest, olsrr, gglm, ggplot2,
#                tidymodels, GGally, skimr, performance)

# --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--

# VERSIONAMENTO ----
# Git & GitHub 
# https://curso-r.githud.io/zen-do-r/git-githud.html
gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_github()
# Avaliação da situação
usethis::git_sitrep()

# --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--

# DADOS ----
## Importação ----

# Teste
# datasets::trees

dados_brutos <- read.table("trees.ascii", header = T)

dplyr::glimpse(dados_brutos)

## Verify NA's ----
# Função para identificar NAs
quantos.na <-
  plyr::colwise(function(x) sum(is.na(x)))

quantos.na(dados_brutos)

## Arrumação ----
dados1 <- dados_brutos|>
  janitor::clean_names()

# Conversão para medidas no SI
dados1 <- dados1|>
  mutate(
    height = height*0.3048,
    volume = volume*0.02831685,
    girth = girth*0.3048
  )

# --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--

# ANÁLISE ----
## 1. AnaData ----
### Tab. Resumo ----
dados1 |> 
  dplyr::rename(
    "Altura" = height, "Volume" = volume, "Circunferência" = girth)|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv"),
    justify = "c", style = "grid", #' rmarkdown',
    round.digits = 3, transpose = T
  ) 
  # kableExtra::kbl(
  #   caption = "Tabela 1: Medidas Resumo.",
  #   digits = 2,
  #   format.args=list(big.mark=".", decimal.mark=","),
  #   align = "c",
  #   row.names = T,
  #   col.names =
  #     c("Min", "Q1", "Med", "Média", "Q3", "Max", "Desvio Padrão", "CV")
  # )|>
  # kableExtra::kable_material(c("striped", "hover", "condensed"))

# Opções destinadas a modelos em html
# ________________________________________________________________________________________
#   kableExtra::kable_styling(
#   # dootstrap_options = c("striped", "hover", "condensed", "responsive"),
#   dootstrap_options = c("striped", "hover"),
#   full_width = F,
#   fixed_thead = T # Fixa o cadeçalho ao rolar a tabela.) |> 
#   kableExtra::footnote(general = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |>
#   kableExtra::kable_material()
# ________________________________________________________________________________________

# --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--

### Plots ----
dados_brutos |> esquisse::esquisser()

dados1 |> 
  dplyr::rename(
    "Altura" = height, "Volume" = volume, "Circunferência" = girth)|>
  GGally::ggpairs()

#### Densidade & Histograma ----

dados1 |> 
  ggplot() +
  aes(x = height) +
  # geom_histogram(aes(y=..density..), bins = 30L, fill = "#112446", colour="white") +
  geom_density(alpha=.6, fill="darkblue", color = "white") +
  geom_vline(
    # show.legend = T,
    xintercept = mean(dados_brutos$age),
    color = "red",
    linetype = "dashed" # "dotted"
  ) +
  annotate("text", x = 21.3, y = 0.035,
           label = "Média",
           size=3, color="red")+
  geom_vline(
    xintercept = quantile(dados_brutos$age, 0.5),
    color = "blue",
    linetype = "dashed"
  ) +
  annotate("text", x = 19.5, y = 0.035,
           label = "Mediana",
           size=3, color="blue")+
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Idade Gestacional", y = "Densidade") +
  theme_classic()


d2 <- dados_brutos |> 
  ggplot() +
  aes(x = length) +
  # geom_histogram(aes(y=..density..), bins = 30L, fill = "#112446", colour="white") +
  geom_density(alpha=.2, fill="#112446", colour = "white") +
  geom_vline(
    # show.legend = T,
    xintercept = mean(dados_brutos$length),
    color = "red",
    linetype = "dashed" # "dotted"
  ) +
  annotate("text", x = 25.5, y = 0.02,
           label = "Média",
           size=3, color="red")+
  geom_vline(
    xintercept = quantile(dados_brutos$length, 0.5),
    color = "blue",
    linetype = "dashed"
  ) +
  annotate("text", x = 22.7, y = 0.02,
           label = "Mediana",
           size=3, color="blue")+
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Comprimento Mandibular", y = "Densidade") +
  theme_classic()


(d1+p1+p1+d2) +
  # plot_layout(nrow = 2, byrow = F) +
  plot_layout(ncol = 2, byrow = F) +
  plot_annotation(
    title = "Figura 3: Densidade das variáveis.",
    # caption = "Fonte: StatLib - Carnegie Mellon University",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    tag_suffix = ":") &
  # theme_classic(base_size = 7) &
  theme_classic() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = -0.4))


#### BoxPlots ----

b1 <- dados_brutos|>
  ggplot2::ggplot(aes(y = age))+
  geom_boxplot(col="#163C82", fill="skyblue", alpha = 0.5)+
  labs(
    title = "",
    # x = "Taxas de desemprego"
    y = "Índice de suicídios nos EUA"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      dig.mark = ".", decimal.mark = ","))
# stat_summary(
#   fun=mean, geom="point", shape=18, size=2, color = "red")
# annotate("text", x = "Feminino", y = 68.5,
#          label = "C45",
#          size=3, color="#163C82")+
# annotate("text", x = "Feminino", y = 50.5,
#          label = "BB31",
#          size=3, color="#163C82")+
# annotate("text", x = "Masculino", y = 69.4,
#          label = "BTP14",
#          size=3, color="#163C82")


b2 <- dados_brutos|>
  ggplot2::ggplot(aes(y = length))+
  geom_boxplot(col="#163C82", fill="skyblue", alpha = 0.5)+
  labs(
    title = "",
    y = "Taxas de desemprego"
    # y = "Índice de suicídios nos EUA"
  ) + scale_y_continuous(
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))


b1 + b2 +
  plot_layout(ncol = 2) + 
  plot_annotation(
    title = "Figura 1: BoxPlot das variáveis em análise por sexo.",
    # caption = "Fonte: StatLib - Carnegie Mellon University",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    tag_suffix = ":") &
  # theme_minimal(base_size = 7) &
  theme_classic() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 5, hjust = 0, vjust = -0.4))

#### Graf. Linha ----
l1 <- dados_brutos|>
  ggplot(aes(x = age, y = length))+
  geom_line(color="blue", linewidth=1, alpha=0.9)+
  labs(
    title = "Evolução do comprimento mandibular ao longo da idade gestacional",
    x = "Idade Gestacional",
    y = "Comprimento Mandibular"
  )+
  theme_bw()

# l2 <- dados|>
#   ggplot(aes(x = ano, y = suic))+
#   geom_line(color="blue", size=1, alpha=0.9)+
#   labs(
#     title = "Índice de Suicídio por ano",
#     x = "Ano",
#     y = "Índice de Suicídio"
#   )+
#   theme_bw()
# 
# l1/l2 + plot_annotation(
#   title = "Figura 3: Evolução dos Índices de Desemprego e Suicídio nos EUA entre 1950 e 2019",
#   tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
#   tag_suffix = ":") &
#   theme_minimal(base_size = 8) &
#   theme(
#     legend.position = "none",
#     plot.tag.position = c(0, 1),
#     plot.tag = element_text(size = 5.5, hjust = 0, vjust = -0.6)
#   )

#### Dispersão ----

p1 <- dados_brutos |> 
  ggplot(aes(x = age, y = length)) +
  geom_point(shape = "triangle", size = 1.5, colour = "#112446") +
  # geom_point()+#scolor="blue", shape=22, fill="#69b3a2"ize=2.5, pch=21, col="brown4", fill='coral')+
  # ggpubr::stat_regline_equation(color="blue", label.x = 92, label.y = 50, size = 3)+
  ggpubr::stat_cor(aes(label = ..r.label..), color="#112446", method = "pearson", 
                   label.y = 40,label.x = 13, r.accuracy = 0.001, size = 3)+ 
  annotate("text", x = 14.5, y = 42,
           label = "Coeficiente de Correlação:",
           size=3, color="#112446")+
  # geom_smooth(formula = "y ~ x", method="lm", se=F, color="red", fill="#69b3a2")+
  # annotate("text", x = 94.5, y = 51,
  #          label = "Modelo Ajustado:",
  #          size=3, color="blue")+
  # facet_wrap(~sex)+ # Divide o gráfico com base em alguma variável
  labs(
    title="Figura 1: Relação entre comprimento mandibular fetal ao longo da idade gestacional",
    x = "Idade Gestacional", 
    y = "Comprimento Mandibular Fetal"
    # caption = "Fonte: Los Angeles Times Journal"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  # hrbrthemes::theme_ipsum(plot_title_size = 15, grid = T, grid_col = "lightgray")+
  theme_classic()+
  # theme_bw(base_size = 10)+
  theme(legend.position = "none"
        # title = element_text(size = 5)
  )

### Correlação ----

stats::cor(dados1$volume, dados1$height)

dados1 |> 
  dplyr::rename(
    "Altura" = height, "Volume" = volume, "Circunferência" = girth)|>
  stats::cor() |> 
  corrplot::corrplot(method = "number", type = "lower")

stats::cor.test(dados1$volume, dados1$height)

# --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--

## 2. Fit Model ----

### Modelo ----
library(broom)

mFit <- stats::lm(volume~height, data=dados1)
mFit

mFit |> summary()

mFit |> broom::tidy(conf.level=0.95, conf.int = T)

# Tabela com coef. de determinação
mFit |> broom::glance()

# mFit$coefficients[[1]] |> round(3) # Beta_0
# mFit$coefficients[[2]] |> round(3) # Beta_1
# 
# beta_0 <- mFit$coefficients[[1]]
# beta_1 <- mFit$coefficients[[2]]

# ========================================================================-=
# mFit |> 
#   broom::tidy(conf.level=0.95) |> 
#   # tidyr::replace(term)
#   # dplyr::select(-term) |>
#   kbl(
#     caption = "Resultados da ANOVA.",
#     digits = 4,
#     format.args=list(big.mark=".", decimal.mark=","),
#     align = "c", 
#     row.names = F,
#     col.names =
#       c("Termos", "Estimativas", "Erro Padrão", "Estatística T", "p-valor")
#   ) |> 
#   kable_styling(
#     full_width = F, position = 'center', 
#     latex_options = c("striped", "HOLD_position", "repeat_header")
#   )|>
#   column_spec(1, bold = T
#   )|>
#   # footnote(
#   #   number = c("GL: Graus de Liberdade"),
#   #   number_title = "Legenda:",
#   #   footnote_as_chunk = F
#   # )|>
#   kable_material()
# ========================================================================-=


### Plots Mod ----

#### Item b ----
mFit|>
  ggplot2::ggplot(aes(x = height, y = rstudent(mFit)))+
  geom_point(color = "#112446", shape = 1)+
  geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
  geom_hline(yintercept = 2, linetype = 2, size = 0.2) +
  geom_hline(yintercept = -2, linetype = 2, size = 0.2)+
  labs(
    title = "Gráfico de Resíduos Jacknife",
    x = "Altura (m)",
    y = "Resíduos Jacknife")+
  scale_x_continuous(
    breaks = seq(60, 90, 5),
    labels = scales::number_format(
      dig.mark = ".", decimal.mark = ","))+
  scale_y_continuous(
    breaks = seq(-3.5, 3, .5),
    labels = scales::number_format(
      dig.mark = ".", decimal.mark = ","))+
  theme_classic()+
  theme(
    legend.position = "none",
    # plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9, face = "plain"),
    axis.line = element_line(size = 0.5, color = "#222222"))

dados1 |> 
  ggplot(aes(x = height, y = volume)) +
  geom_point(shape = 1, size = 1.5, colour = "#112446") +
  # geom_point(shape = "triangle", size = 1.5, colour = "#112446") +
  # geom_point()+#scolor="blue", shape=22, fill="#69b3a2"ize=2.5, pch=21, col="brown4", fill='coral')+
  ggpubr::stat_regline_equation(color="red", label.x = 65, label.y = 70.0, size = 3)+
  annotate("text", x = 65.0, y = 75,
           label = "Modelo Ajustado:",
           size=3, color="red")+
  ggpubr::stat_cor(aes(label = ..rr.label..), color="#112446", method = "pearson", 
                   label.y = 60,label.x = 65.0, r.accuracy = 0.0001, size = 3)+ 
  annotate("text", x = 70, y = 65,
           label = "Coeficiente de Determinação:",
           size=3, color="#112446")+
  # ggpubr::stat_cor(aes(label = ..rr.label..), color="#112446", method = "pearson",
  #                  label.y = 12.8,label.x = 3.5, r.accuracy = 0.0001, size = 3)+
  geom_smooth(formula = "y ~ x", method="lm", se=F, color="red", fill="#69b3a2")+
  # facet_wrap(~sex)+ # Divide o gráfico com base em alguma variável
  labs(
    title="Figura 1: Modelo de RLS para volume da árvore com base na altura",
    x = "Altura", 
    y = "Volume",
    caption = "Fonte: Floresta Nacional de Allegheny"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 9, face = "plain"),
    axis.line = element_line(size = 0.5, color = "#222222"))

# --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--

# Script roteiro com r base

qqnorm(rstudent(m1),ylab="Residuos estudentizados",main=" ")
qqline(rstudent(m1))
hist(rstudent(m1),probability="TRUE", main=" ")
boxplot(rstudent(m1))
plot((desemp),rstudent(m1),ylab="Residuos jacknife",main="")
abline(h=0,lty=4)
abline(h=-2, lty=3)
abline(h=2, lty=3)
plot(rstudent(m1))

# --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--


# • Para testar normalidade dos res´ıduos usando o procedimento de Kolmogorov-Smirnov:
#   ks.test(rstudent(m2),"pnorm")
# • Para implementa¸c~ao dos testes para homocedasticidade de Breusch-Pagan e GoldfeldQuandt:
#   
#   library(lmtest)
# bptest(length ~ age, data=Mandible)
# gqtest(length ~ age, data=Mandible)
# 
# • O teste F de linearidade pode ser implementado usando:
#   
#   mfac=lm(length ~ factor(age), data=Mandible)
# anova(m2,mfac)
# 
# onde m2 representa o objeto que cont´em o ajuste do modelo de regress~ao linear.
# • Para avaliar independ^encia dos res´ıduos atrav´es do teste de Durbin-Watson:
#   
#   library(lmtest)
# dwtest(length ∼ age, data=Mandible)



### Var-Cov Matrix ----


c(mFit$coefficients[[1]], mFit$coefficients[[2]])

var(beta)
cov(beta)

vcov(mFit)
mFit |> vcov()

fit_sumario$coefficients

## 3. Validation Param ----

### Tabela ANOVA ----
#### Metodo 1 ----
mFit_anova <- anova(mFit)

mFit_anova

  # kbl(
  #   caption = "Resultados da ANOVA.",
  #   digits = 5,
  #   format.args=list(big.mark=".", decimal.mark=","),
  #   align = "c", 
  #   row.names = F,
  #   col.names =
  #     c("GL", "SQ", "QM", "Estatística", "p-valor")) |> 
  # kable_styling(
  #   full_width = F, position = 'center', 
  #   latex_options = c("striped", "HOLD_position", "repeat_header")
  # )|>
  # column_spec(1, bold = T
  # )|>
  # footnote(
  #   number = c("GL: Graus de Liberdade"),
  #   number_title = "Legenda:",
  #   footnote_as_chunk = F
  # )|>
  # kable_material()

#### Metodo 2 ----

mFit_anova |> 
  broom::tidy() 

# kbl(
#   caption = "Resultados da ANOVA.",
#   digits = 5,
#   format.args=list(big.mark=".", decimal.mark=","),
#   align = "c", 
#   row.names = F,
#   col.names =
#     c("", "GL", "SQ", "QM", "Estatística", "p-valor")) |> 
#   kable_styling(
#     full_width = F, position = 'center', 
#     latex_options = c("striped", "HOLD_position", "repeat_header")
#   )|>
#   column_spec(1, bold = T
#   )|>
#   footnote(
#     number = c("GL: Graus de Liberdade"),
#     number_title = "Legenda:",
#     footnote_as_chunk = F
#   )|>
#   kable_material()


# --==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--

broom::tidy(mFit)


mFit_sumario <- summary(mFit)
mFit_sumario

ic_parametros <- broom::confint(mFit)

broom::augment(mFit)

?glance

broom::tidy(mFit)

broom::glance(mFit)


## Item f ----

## SQTotal
SQT <- function(y){
  y_barra = mean(y)
  sqt = sum((y - y_barra)^2)
  
  return(sqt)
}

## SQResíduos
SQE <- function(x, y){
  n = length(x)
  mFit <- lm(y~x)
  b0 = mFit$coefficients[[1]]
  b1 = mFit$coefficients[[2]]
  y_hat = numeric(n)
  y_hat = b0+b1*x
  sqr = sum((y - y_hat)^2)
  
  return(sqr)
}

## SQRegressão
SQR <- function(x, y){
  n = length(x)
  y_barra = mean(y)
  mFit <- lm(y~x)
  b0 = mFit$coefficients[[1]]
  b1 = mFit$coefficients[[2]]
  y_hat = numeric(n)
  y_hat = b0+b1*x
  sqres = sum((y_hat - y_barra)^2)
  
  return(sqres)
}

SQReg = SQR(dados_brutos$desemp, dados_brutos$suic)

SQRes = SQE(dados_brutos$desemp, dados_brutos$suic)

SQTot = SQT(dados_brutos$suic)


SQ <- rbind(SQReg, SQRes, SQTot)

n = dim(dados_brutos)[[1]]

QMRes = SQRes/(n-2)

SQRes/12

QMReg = SQReg

QMTot = NA

QM <- rbind(QMReg, QMRes, QMTot)


F_Statistic = rbind(QMReg/QMRes, NA, NA)

1-pf(QMReg/QMRes, 1, 12) |> round(5)

p_val = rbind(1-pf(QMReg/QMRes, 1, 12), NA, NA)


results <- cbind(SQ, QM, F_Statistic, p_val) |> round(5)
rownames(results) <- c("Regressão", "Resíduos", "Total")
colnames(results) <- c("Soma de Quadrados", "Quadrados Médios", "F", "p-valor")

results[is.na(results)] <- ""
results[is.na(results)] <- "-"

results |>
  kableExtra::kbl(
    caption = "Tabela 1: Medidas Resumo.",
    digits = 4,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c",
    row.names = T,
    col.names =
      c("Soma de Quadrados", "Quadrados Médios", "F")
  )

## 4. IC - IntConf ----
### Método 1 ----
confint(mFit)[2,]
rownames(resultados) <- c("$\\hat \\beta_0$", "$\\hat \\beta_1$")

IC_beta1 <- rbind(confint(mFit)[2,])
rownames(IC_beta1) <- c("$\\hat \\beta_1$")

IC_beta1

### Método 2 ----

ic = mFit |> broom::tidy(conf.level=0.95, conf.int = T)

t$statistic[[2]]
t$conf.low[2]
t$conf.high[2]


# Cria um vetor de zeros de tamanho 5.
numeric(5)

# -----

fit_anova <- anova(mFit)
fit_sumario <- summary(mFit)
ic_parametros <- confint(mFit)


fit_sumario[["coefficients"]] |>  tibble::as_tibble() %>% 
  kbl(
    caption = "Sumarização do modelo ajustado.",
    digits = 4,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = F,
    col.names =
      c("Estimativa", "Erro Padrão", "Estatística t", "p-valor")
  ) %>% 
  footnote(
    number = c("Linha 1: Dados referentes a β0", "Linha 2: Dados referentes a β1"),
    number_title = "Legenda:",
    footnote_as_chunk = F
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "repeat_header"))|>
  column_spec(1, bold = F)|>
  kable_material()

fit_anova |> 
  kbl(
    caption = "Resultados da ANOVA.",
    digits = 4,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = F,
    col.names =
      c("GL", "SQ", "QM", "Estatística", "p-valor")) |> 
  footnote(
    number = c(
      "Linha 1: Dados referentes a β0", 
      "Linha 2: Dados referentes a β1",
      "GL: Graus de Liberdade", 
      "SQ: Soma de Quadrados", 
      "QM: Quadrado Médio", 
      "Estatística: F-Snedecor"
    ),
    number_title = "Legenda:",
    footnote_as_chunk = F
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "repeat_header")
  )|>
  column_spec(1, bold = F
  )|>
  kable_material()

ic_parametros %>% 
  kbl(
    caption = "Intervalo de Confiança.",
    digits = 4,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = F,
    col.names =
      c("α/2 = 2,5%", "1-α/2 = 97,5%")
  ) %>%
  footnote(
    number = c("Linha 1: Dados referentes a β0", "Linha 2: Dados referentes a β1"),
    number_title = "Legenda:",
    footnote_as_chunk = F
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "repeat_header")
  )|>
  column_spec(1, bold = F
  )|>
  kable_material()

## Teste T ----

Tcritc_B1 <- function(x, y){
  # SQRes
  n = length(x)
  mFit <- lm(y~x)
  b0 = mFit$coefficients[[1]]
  b1 = mFit$coefficients[[2]]
  y_hat = numeric(n) # criação do vetor nulo
  y_hat = b0+b1*x
  
  SQRes = sum((y - y_hat)^2)
  # QMReg
  QMRes = SQRes/(n-2)
  
  x_barra = mean(x)
  xi = sum((x - x_barra)^2)
  
  t = (b1*sqrt(xi))/sqrt(QMRes)
  
  return(t)
}


Tcritc_B1(dados_brutos$age, dados_brutos$length) |> round(4)

qt(p=0.975, df=12) |> abs()





## Item i ----

f1 <- predict(mFit,data.frame(desemp=5.5),se=T)

conf.plim <- predict(mFit, data.frame(desemp=5.5), interval="confidence")

pred.plim <- predict(mFit, data.frame(desemp=8), interval="prediction")




## Item l ----
pacman::p_load(ellipse)

plot(ellipse(mFit,c(1,2)),type="l",xlim=c(0,11))
points(0,0)
points(coef(mFit)[1],coef(mFit)[2],pch=18)
abline(v=confint(mFit)[1,],lty=2)
abline(h=confint(mFit)[2,],lty=2)


new <- data.frame(desemp = seq(3, 9, 0.5))

pred.plim <- predict(lm(dados_brutos$suic ~ dados_brutos$desemp), new, interval="prediction")
pred.clim <- predict(lm(dados_brutos$suic ~ dados_brutos$desemp), new, interval="confidence")

pred.plim <- predict(mFit, new, interval="prediction")
pred.clim <- predict(mFit, new, interval="confidence")


matplot(new$desemp,cbind(pred.clim, pred.plim[,-1]), 
        lty=c(1,2,2,3,3), type="l", ylab="predicted y")


# 5. AnaRes ----



gglm(mFit)

# Como coletar os resíduos do modelo ajustado:
rstudent(mFit) #para os resíduos estudentizados
rstandard(mFit) #para os resíduos padronizados
residuals(mFit) #para os resíduos ordinários

#### Gráficos RBase ----
par(mfrow = c(2, 2))

plot(mFit)

par(mfrow = c(1, 1))

# _____________________________________________



###### Pacote Performance ----
pacman::p_load(performance)

performance::check_model(mFit, 
                         check = c("linearity", "qq", "homogeneity", "outliers"))
# __________________________________________________________________


### 3. ----

mFit_resid <- broom::augment(mFit)
dplyr::glimpse(mFit_resid)

mFit_resid |> filter(.std.resid<=-3)
mFit_resid |> filter(.std.resid>=2)

mFit_resid$.std.resid

library(ggthemes)

#### Histograma ----
##### Método 1 ----
# Sem criação de variável
p1 <- mFit|>
  ggplot2::ggplot(aes(x = rstudent(mFit)))+
  geom_histogram(aes(y = ..density..), fill = "#112446", 
                 color = "#112446", binwidth = 0.3, alpha = 0.5)+
geom_density(
  # fill = "red",
  alpha = 0.2)+
stat_function(
  fun = dnorm,
  args = (list(mean = mean(rstudent(mFit)), sd = sd(rstudent(mFit)))),
              # geom = "polygon",
              # fill = "blue",
              # alpha = 0.2,
              # color = "black",
              size = 0.5)+
geom_vline(
  # show.legend = T,
  xintercept = mean(rstudent(mFit)),
  color = "red",
  linetype = "dashed" # "dotted"
) +
  annotate("text", x = 0.2, y = 0.55,
           label = "Média",
           size=3, color="red")+
  geom_vline(
    xintercept = quantile(rstudent(mFit), 0.5),
    color = "blue",
    linetype = "dashed"
  ) +
  annotate("text", x = -0.45, y = 0.55,
           label = "Mediana", size=3, color="blue")+
  labs(
    x = "Resíduos Estudentizados", y = "Densidade",
    title = "Histograma dos Resíduos Estudentizados (Jacknife)"
  )+
  scale_x_continuous()+
    # breaks = seq(-6, 4, 1))+
  # scale_y_continuous(
  #   labels = scales::number_format(
  #     big.mark = ".", decimal.mark = ","
  #   ))+
  scale_y_continuous(
    labels = scales::percent
    # labels = scales::number_format(
    #   big.mark = ".", decimal.mark = ","
  )+
  theme_minimal()+
  theme(
    legend.position = "none",
    # plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9, face = "plain"),
    axis.line = element_line(size = 0.5, color = "#222222"))
# ggthemes::theme_economist()


##### Método 2 ----
# Com criação de variável (melhor escolha!)

mFit_resid|>
  ggplot2::ggplot(aes(x = .std.resid))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", 
                 color = "blue", binwidth = 0.3, alpha = 0.5)+
  geom_density(fill = "red", alpha = 0.2)+
  stat_function(fun = dnorm, 
                args = (
                  list(
                    mean = mean(mFit_resid$.std.resid), 
                    sd = sd(mFit_resid$.std.resid))), 
                geom = "polygon", fill = "blue", alpha = 0.5,
                color = "black", size = 0.5)+
  labs(
    x = "Resíduos Padronizados",
    y = "Densidade",
    title = "Histograma dos Resíduos Padronizados"
  )+
  scale_x_continuous(breaks = seq(-3, 3, 1))+
  scale_y_continuous(
    # labels = scales::percent
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )
  )+
  # theme_minimal()+
  theme(legend.position = "none",
        axis.line = element_line(size = 0.8, color = "#222222"))
# ggthemes::theme_economist()


#### Box-plot ----

##### Método 1 ----
# Sem criação de variável
p2 <- mFit|>
  ggplot2::ggplot(aes(x="", y = rstudent(mFit)))+
  geom_boxplot(col="#112446", fill="#112446", alpha = 0.5, 
               orientation = "x", outlier.shape=1, 
               outlier.color="#112446", outlier.fill="#112446")+
  stat_summary(fun="mean", geom="point", 
               shape=18, size=2, color = "tomato")+
  labs(
    title = "",
    x="",
    y = "Resíduos Estudentizados") +
  # scale_x_continuous(
  #   breaks = seq(-0.6, 0.6, 0.2),
  #   labels = scales::number_format(
  #     dig.mark = ".",
  #     decimal.mark = ","))+
  scale_y_continuous(
    breaks = seq(-6, 4, 1),
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_minimal()+
  theme(
    legend.position = "none",
    # plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9, face = "plain"),
    axis.line = element_line(size = 0.5, color = "#222222"))


##### Método 2 ----
# Com criação de variável (melhor escolha!)
mFit_resid|>
  ggplot2::ggplot(aes(x="", y = .std.resid))+
  geom_boxplot(col="#112446", fill="#112446", alpha = 0.5)+
  stat_summary(fun="mean", geom="point", 
               shape=18, size=2, color = "tomato")+
  # annotate("text", x = 0.95, y = 2.4, label = "2,40",
  #          size=3, color="#112446")+
  # annotate("text", x = 0.95, y = -3.12, label = "-3,12",
  #          size=3, color="#112446")+
  # annotate("text", x = 0.95, y = -3.76, label = "-3,76",
  #          size=3, color="#112446")+
  # annotate("text", x = 0.95, y = -4.36, label = "-4,36",
  #          size=3, color="#163C82")+
  labs(title = "Box-Plot", x="", y = "Resíduos Estudentizados")+
  scale_y_continuous(
    breaks = seq(-6, 4, 1),
    labels = scales::number_format(
      dig.mark = ".", decimal.mark = ","))+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size = 9, face = "plain"),
        axis.line = element_line(size = 0.5, color = "#222222"))


##### Gráfico de Resíduos contra Valor Médio ----
d1 <- mFit_resid|>
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point(color = "#234B6E", shape = 15) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
  geom_smooth(
    # aes(linewidth=0.01),
    linewidth=1,
    se = T, color = "tomato", method = 'loess', formula = 'y ~ x')+
  labs(
    x = "Valores Médios Ajustados",
    y = "Resíduos Ordinários",
    title = "Linearidade",
    subtitle = "Resíduos vs. Valores Ajustados"
  )+
  # scale_x_continuous(breaks = seq(0,30,5))+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  # theme_minimal()+
  theme(
    legend.position = "none",
    # plot.title = element_text(size = 11, face = "bold"),
    # axis.title = element_text(size = 8, face = "plain"),
    axis.line = element_line(size = 0.8, color = "#222222"))


##### Gráfico de normalidade dos resíduos ----
d2 <- mFit_resid |> 
  ggplot(aes(sample = .std.resid)) + 
  qqplotr::stat_qq_band(alpha = 0.3) + # Plota a banda de confiança
  qqplotr::stat_qq_point(color = "#234B6E", shape=15) + # Plota os pontos
  qqplotr::stat_qq_line(linetype = 2, size = 0.2) + # Plota a reta
  labs(
    x = "Quantil Teórico",
    y = "Quantil Amostral",
    # title = glue::glue('{expression(4))} teste')
    # title = paste(expression(sqrt('Volume (m³)')),"Figura 3: Avaliação dos pressupostos do modelo T1", expression(sqrt('Volume (m³)')))
    # title = "Q-Q Normal Plot"
    title = "Gráfico quantil-quantil normal"
  )+
  # scale_x_continuous(breaks = seq(-3,3,1))+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(breaks = seq(-4, 2, 1),
                     labels = scales::number_format(
                       big.mark = ".", decimal.mark = ","))+
  theme_minimal()+
  theme(
    legend.position = "none",
    # plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 8, face = "plain"),
    axis.line = element_line(size = 0.8, color = "#222222"))



mFit_resid %>% 
  ggplot(aes(sample = .std.resid)) + 
  qqplotr::geom_qq_band(bandType = "ks", mapping = aes(fill = "KS"), alpha = 0.3) +
  # qqplotr::geom_qq_band(bandType = "ts", mapping = aes(fill = "TS"), alpha = 0.5) +
  # qqplotr::geom_qq_band(bandType = "pointwise", mapping = aes(fill = "Normal"), alpha = 0.5) +
  # qqplotr::geom_qq_band(bandType = "boot", mapping = aes(fill = "Bootstrap"), alpha = 0.5) +
  qqplotr::stat_qq_band(mapping = aes(fill = "IC"), alpha = 0.3) + # Plota a banda de confiança
  qqplotr::stat_qq_point(color = "#234B6E", shape=15) + # Plota os pontos
  qqplotr::stat_qq_line(linetype = 2, size = 0.2) + # Plota a reta
  labs(
    x = "Quantil Teórico",
    y = "Quantil Amostral",
    title = "Q-Q normal"
    # title = "Gráfico quantil-quantil normal"
  )+
  # scale_x_continuous(breaks = seq(-3,3,1))+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(breaks = seq(-4, 2, 1),
                     labels = scales::number_format(
                       big.mark = ".", decimal.mark = ","))+
  theme_minimal()+
  theme(
    # legend.position = "none",
    # plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 8, face = "plain"),
    axis.line = element_line(size = 0.8, color = "#222222"))


##### Gráfico Homogeneidade de Variâncias (Locação-Escala) ----
d3 <- mFit_resid %>% 
  ggplot(aes(x = .fitted, y = sqrt(abs(.std.resid)))) + 
  geom_point(color = "#234B6E", shape=15) +
  # geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
  geom_smooth(
    se = T, color = "tomato", method = 'loess', formula = 'y ~ x')+
  # ylab("$\\sqrt(Resíduos Padronizados)$")+
  # ggtitle("Teste")+
  labs(
    x = "Valores Ajustados",
    y = expression(sqrt("|Resíduos Padronizados|")),
    title = "Homogeneidade de Variâncias",
    subtitle = "(Locação-Escala)"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  theme_minimal()+
  theme(
    legend.position = "none",
    # plot.title = element_text(size = 11, face = "bold"),
    # axis.title = element_text(size = 8, face = "plain"),
    axis.line = element_line(size = 0.8, color = "#222222"))

##### Gráfico Resíduos Estudentizados vs. Volume Médio Ajustado ----
# mFit_resid %>% 
#   ggplot(aes(x = .fitted, y = rstudent(mFit))) + 
#   geom_point(color = "#234B6E") +
#   geom_hline(aes(yintercept = 0), col="red")+
#   # geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
#   # geom_smooth(
#   #   se = T, color = "tomato", method = 'loess', formula = 'y ~ x')+
#   # ylab("$\\sqrt(Resíduos Padronizados)$")+
#   # ggtitle("Teste")+
#   labs(
#     x = "Volume Médio Ajustado",
#     y = "Resíduos Estudentizados",
#     title = "Resíduos Estudentizados vs. Volume Médio Ajustado")+
#   scale_x_continuous(
#     labels = scales::number_format(
#       big.mark = ".", decimal.mark = ","))+
#   scale_y_continuous(
#     breaks = seq(from = -3, to = 4, by = 1),
#     labels = scales::number_format(
#       big.mark = ".", decimal.mark = ","))+
#   theme_minimal()+
#   theme(
#     legend.position = "none",
#     plot.title = element_text(size = 11, face = "bold"),
#     axis.title = element_text(size = 8, face = "plain"),
#     axis.line = element_line(size = 0.5, color = "#222222"))

d4 <- mFit|>
  ggplot2::ggplot(aes(x = height, y = rstudent(mFit)))+
  geom_point(color = "#234B6E", shape = 1)+
  geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
  geom_hline(yintercept = 2, linetype = 2, size = 0.2) +
  geom_hline(yintercept = -2, linetype = 2, size = 0.2)+
  labs(
    title = "Presença de valores atípicos",
    x = "Idade Gestacional",
    y = "Resíduos Jacknife")+
  scale_x_continuous(
    # breaks = seq(10, 45, 2.5),
    labels = scales::number_format(
      dig.mark = ".", decimal.mark = ","))+
  scale_y_continuous(
    # breaks = seq(-4, 3, 1),
    labels = scales::number_format(
      dig.mark = ".", decimal.mark = ","))+
  theme_minimal()+
  theme(
    legend.position = "none",
    # plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9, face = "plain"),
    axis.line = element_line(size = 0.5, color = "#222222"))

p1+p2+d1 + d2 + d3 + d4 +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Figura 5: Análise de resíduos do modelo ajustado",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ",
    tag_sep = ".", tag_suffix = ":") &
  theme(
    legend.position = "none",
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = -0.4)
  )


(d1+p1+p1+d2) +
  # plot_layout(nrow = 2, byrow = F) +
  plot_layout(ncol = 2, byrow = F) +
  plot_annotation(
    title = "Figura 3: Densidade das variáveis.",
    # caption = "Fonte: StatLib - Carnegie Mellon University",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    tag_suffix = ":") &
  # theme_classic(base_size = 7) &
  theme_classic() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = -0.4))

#### Pts Influentes Verify ----

mFit_resid |> 
  # dplyr::select(.resid) |> 
  dplyr::filter(.resid <= -8)


mFit_resid |> 
  # dplyr::select(.resid) |> 
  dplyr::filter(.std.resid <= -3)



### Comandos dados ----

qqnorm(rstudent(mFit),ylab="Residuos estudentizados",main=" ")
qqline(rstudent(mFit))
hist(rstudent(mFit),probability="TRUE", main=" ")
boxplot(rstudent(mFit))
plot((dados_brutos$age),rstudent(mFit),ylab="Residuos jacknife",main="")
abline(h=0,lty=4)
abline(h=-2, lty=3)
abline(h=2, lty=3)
plot(rstudent(mFit))

#### Testes ----  

# Script lab

# Para testar normalidade dos resíduos usando o procedimento de Kolmogorov-Smirnov:
ks.test(rstudent(mFit),"pnorm")

# • Para implementação dos testes para homoscedasticidade de Breusch-Pagan e GoldfeldQuandt:
library(lmtest)

lmtest::bptest(length ~ age, data=dados_brutos) 
lmtest::gqtest(length ~ age, data=dados_brutos)

# • O teste F de linearidade pode ser implementado usando:
mfac=lm(length ~ factor(age), data=dados_brutos) 
anova(mFit,mfac)
# onde mFit representa o objeto que contém o ajuste do modelo de regressão linear.

# • Para avaliar independência dos resíduos através do teste de Durbin-Watson:
library(lmtest)
dwtest(length ~ age, data=dados_brutos)


res1 <- residuals(mFit) # Resíduo Ordinário
res_ord <- mFit_resid$.resid

res_ord <- mFit_resid$.resid

d1 + gridExtra::tableGrob(c(t_ks$statistic, t_ks$p.value))
d1 + d4 + gridExtra::tableGrob(round(resultados,4))+plot_layout(ncol = 2) 
d2 + gridExtra::tableGrob(round(resultados,4))+plot_layout(ncol = 2) 

d2 + gridExtra::tableGrob(round(resultados,4))+plot_layout(ncol = 2) 





##### KS ----

##### Teste de normalidade dos resíduos
#H0: normalidade
#H1: não normalidade

t_ks <- stats::ks.test(mFit_resid$.resid, "pnorm", mean(mFit_resid$.resid), sd(mFit_resid$.resid))

t_ks
t_ks$statistic

# pnorm = acumulada da normal, o "p" antes da distribuição indica ser a acumulada.

### SW* ----

##### Teste de homoscedasticidade dos resíduos ----
shapiro.test(mFit_resid$.resid)
(t_sw <- shapiro.test(res1))

#H0: resíduos homoscedásticos - Variância constante
#H1: resíduos heteroscedásticos - Variância NÃO constante

## GQ ----
(t_gq <- lmtest::gqtest(mFit))

## BP* ----
(t_bp <- lmtest::bptest(mFit, studentize = F))

lmtest::bptest(mFit, studentize = T) # Teste

# PARK

summary(lm(res1^2 ~ dados1$height))

### Teste F para linearidade ----

m_kmedias <- stats::lm(length ~ factor(age), data = dados_brutos)
anova(mFit, m_kmedias)

t_f <- anova(mFit, m_kmedias) |> 
  broom::tidy()




# Teste de correlação serial lag 1 (Independência dos erros)
#H0: correlacionados - existe correlação serial
#H1: não correlacionados - não existe correlação serial ///ficou confuso no vídeo as hipoteses///

# DW
(t_dw <- lmtest::dwtest(mFit))

resultados <- rbind(
  t_sw$statistic,
  t_bp$statistic,
  t_dw$statistic)

aux <- rbind(
  t_sw$p.value,
  t_bp$p.value,
  t_dw$p.value)

resultados <- cbind(resultados, aux)

rownames(resultados) <- c("Shapiro-Wilks", "Breush-Pagan", "Durbin-Watson")

colnames(resultados) <- c("Estatística de teste", "p-valor")

t1 <- resultados|>
  kbl(
    caption = "Testes de Diagnósticos dos Resíduos",
    digits = 5,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", row.names = T, booktabs = T
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "repeat_header")
  )|>
  column_spec(1, bold = T
  )|>
  # footnote(
  #   general = "Teste realizado com 5% de significância",
  #   general_title = "Nota:",
  #   footnote_as_chunk = T
  # )|>
  kable_material()

# Plots + Tests ----

resultados <- rbind(
  t_sw$statistic,
  t_bp$statistic,
  t_dw$statistic)

aux <- rbind(
  t_sw$p.value,
  t_bp$p.value,
  t_dw$p.value)

### Normalidade =================================================

resultados <- cbind(t_ks$statistic, t_ks$p.value) |> round(6)

rownames(resultados) <- "Kolmogorov-Smirnov"

colnames(resultados) <- c("Estatística de teste", "p-valor")

resultados

patchwork <- p1+p2+d2 + gridExtra::tableGrob(round(resultados,4))

patchwork[[4]] <- patchwork[[4]] + plot_layout(tag_level = 'new')

patchwork + plot_annotation(
  title = "Figura 3: Normalidade.",
  tag_levels = c("A"), tag_prefix = "Sub Fig. ", 
  tag_sep = ".", tag_suffix = ":") &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = -0.4))

t_ks$p.value |> round()

### Homocedasticidade =================================================

resultados <- rbind(
  t_bp$statistic,
  t_gq$statistic)

aux <- rbind(
  t_bp$p.value,
  t_gq$p.value)

resultados <- cbind(resultados, aux)

rownames(resultados) <- c("Breush-Pagan", "GoldfeldQuandt")

colnames(resultados) <- c("Estatística \nde teste", "p-valor")

resultados



patchwork <- d3 + gridExtra::tableGrob(round(resultados,4))

patchwork[[2]] <- patchwork[[2]] + plot_layout(tag_level = 'new')

patchwork + plot_annotation(
  title = "Figura 3: Heterocedasticidade.",
  tag_levels = c("A"), tag_prefix = "Sub Fig. ", 
  tag_sep = ".", tag_suffix = ":") &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = -0.4))

### Linearidade =================================================

tt <- t_f |> as.matrix()

colnames(tt) <- c("Termos", "GL.Res.", "RSS", "GL", "Soma SQ", "Estatística F", "p-valor")

patchwork <- (d1 + d4) / gridExtra::tableGrob(tt)

patchwork[[2]] <- patchwork[[2]] + plot_layout(tag_level = 'new')

patchwork + plot_annotation(
  title = "Figura 3: Análise de Linearidade.",
  tag_levels = c("A"), tag_prefix = "Sub Fig. ", 
  tag_sep = ".", tag_suffix = ":") &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = -0.4))


### Independência =================================================


(t_dw <- lmtest::dwtest(mFit))


resultados <- cbind(t_dw$statistic, t_dw$p.value)

rownames(resultados) <- c("Durbin-Watson")

colnames(resultados) <- c("Estatística de teste", "p-valor")




resultados |> 
  kableExtra::kbl(
    caption = "Tabela 1: resultados do teste de Durbin-Watson.",
    digits = 4,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c",
    row.names = T,
    col.names =
      c("Estatística de teste", "p-valor")
    # c("Min", "Q1", "Med", "Média", "Q3", "Max", "Desvio Padrão", "CV")
  )|>
  kableExtra::kable_styling(
    # dootstrap_options = c("striped", "hover", "condensed", "responsive"),
    # dootstrap_options = c("striped", "hover"),
    full_width = F,
    fixed_thead = T # Fixa o cadeçalho ao rolar a tadela.
  ) %>%
  kable_material(c("striped", "hover", "condensed"))
# footnote(general = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |>
# kable_material()


resultados <- t_dw |> as.matrix()


gridExtra::tableGrob(resultados) + d1+plot_annotation(
  title = "Figura 3: Análise de Linearidade.",
  tag_levels = c("A"), tag_prefix = "Sub Fig. ", 
  tag_sep = ".", tag_suffix = ":") &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = -0.4))



















# FIM ----




