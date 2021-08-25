path <- "C:/Users/karen/Downloads/Cursos Alura/Linguagem R/Data Visualization parte 1 gráficos com uma variável/googleplaystore.csv" #Data
dados <- read.csv(file = path) ##Values

View(dados) ##Visualização de dados, tendo o objeto como parâmetro

head(dados) ##Visualização dos primeiros dados

tail(dados) ##Visualização dos últimos dados

View(head(dados)) 

str(dados) ##Verificação de tipos de dados armazenados

##Alterar tipo de importação, função não reconhece Factor como dado
dados <- read.csv(file = "C:/Users/karen/Downloads/Cursos Alura/Linguagem R/Data Visualization parte 1 gráficos com uma variável/googleplaystore.csv")

str(dados)

##install.packages("dplyr") ##Função para instalação de bibliotecas
library(dplyr) ##Função para utilizar a biblioteca instalada

##ggplot2 = Biblioteca de histogramas
##install.packages("ggplot2")
library(ggplot2)

hist(dados$Rating) ##Função para desenhar o histograma

table(dados$Rating) ##Cria uma tabela de frequência -> Contagem de cada valor que aparece na base de dados

hist(dados$Rating, xlim = c(1,5)) ##Define limite para a reta X do gráfico

##+ é o concatenador de funções no ggplot2
##aes = Título do gráfico abaixo do eixo X
## na.rm = TRUE - Recebe valor booleano, remove valores infinitos
##breaks = seq(1,5) - Cria sequencia no gráfico de colunas de 1 a 5
rating.Histogram <- ggplot(data = dados) + geom_histogram(mapping = aes(x = Rating), na.rm = TRUE, breaks = seq(1,5)) + xlim(c(1,5))

seq(1,5) ##Cria um array de valor minimo até o máximo

##rating.Histogram <- Onde o gráfico foi salvo
rating.Histogram

##stat - contagem de quantos registros tem a categoria no nosso banco de dados
##Valores do eixo X irão para o eixo Y - coord_flip()
ggplot(data = dados) + geom_bar(mapping = aes(x = Category), stat = "count") + coord_flip()

category.Freq <- data.frame(table(dados$Category))

##stat = "identity" - Indica para a função geom_bar que a própria função não vai mais gerara  tabela de frequência, não fará a contagem
##x = reorder(Var1, Freq) - Ordena valores de forma decrescente -> -Freq para ordem crescente) 
ggplot(data = category.Freq) + geom_bar(mapping = aes(x = reorder(Var1, Freq), y = Freq), stat = "identity") + coord_flip()

##Plotar categorias com mais aplicativos, ou com menos.
##Deixar colunas em branco indica que usaremos todas
##order(-category.Freq$Freq) altera a ordem para decrescente
category.Top10 <- category.Freq[(order(-category.Freq$Freq)), ] 
category.Top10 <- category.Top10[1:10, ]

freq.Category.Plot <- ggplot(data = category.Top10) + geom_bar(mapping = aes(x = reorder(Var1, Freq), y = Freq), stat = "identity") + coord_flip()

##Importante manter a base original e criar sub bases, chamadas de subsets.
## %>% - Concatenação de funções
dados_2 <- dados %>% filter(Category != "1.9")

##Verificar valor minimo e maximo
min(dados_2$Rating)
max(dados_2$Rating)
##[1] NaN
##Ocorre pois há dados sem definição

##is.na identifica todos os NaN
dados_2 %>% filter(is.na(Rating)) %>% count()

##Retorna informações mais precisas e detalhadas sobre o campo Rating
summary(dados_2$Rating)

dados_2 %>% filter(is.na(Rating)) %>% group_by(Category) %>% 
  count() %>% 
  summarise(media = mean(Rating))

mean.Category <- dados_2 %>% filter(!is.na(Rating)) %>% group_by(Category) %>% 
  summarise(media = mean(Rating))

mean.Category

##Substituir os valores NaN pela media
for(i in 1:nrow(dados_2)){
  ##nrow percorre todo o vetor
  if(is.na(dados_2[i, "Rating"])){
    #slice
    dados_2[i, "newRating"] <- mean.Category[mean.Category$Category == dados_2[i, "Category"], "media"]
  }else{
    dados_2[i, "newRating"] <- dados_2[i, "Rating"]
  }
}

View(dados_2)
summary(dados_2$newRating)
min(dados_2$newRating)
max(dados_2$newRating)

##newRating < 2 = "ruim"
##newRating > 4 = "bom"
##newRating 2.1 e 3.9 = "regular"

##não cria um novo conjunto de dados, mas sim uma nova coluna (Summarise cria um novo conjunto e vc perde o anterior)
##if_else(Condição, Caso seja verdadeira, Senão)
dados_2 <- dados_2 %>%
  mutate(rating_class = if_else(newRating < 2, "Ruim", 
                                if_else(newRating > 4, "Bom", "Regular")))

rating_class_plot <- ggplot(dados_2) + geom_bar(aes(rating_class), stat = "count")

rating.Histogram
freq.Category.Plot
rating_class_plot

##Gráfico de pizza (não tem uma função própria/Até 5 elementos)
type.Freq <- data.frame(table(dados_2$Type))
##Caso não houvesse um y, teríamos que fazer o stat count para fazer uma frequencia dos valores de x
##coord_polar altera o formato do gráfico através da alteração do raio
type.Plot <- ggplot(type.Freq) + geom_bar(aes(x = "", y = Freq, fill = Var1), stat = "identity", width = 1) +
  coord_polar(theta = "y", start = 0)

type.Plot

str(dados_2)

freq.Size <- data.frame(table(dados_2$Size))

teste <- dados_2$Size[1]
teste

##Ocorrência de K ou M nos registros
grepl(pattern = "m", x = teste, ignore.case = T)
grepl(pattern = "K", x = teste, ignore.case = T)

##Eliminar a letra M ou K
gsub(pattern = "M", replacement = "--", x = teste)
gsub(pattern = "K", replacement = "--", x = teste)

##Convrsão de M para Kb
1 * 1024

##Loops dentro do R em grande bases de dados
dados_2$kb <- sapply(X = dados_2$Size, FUN = function(y){
  if(grepl("m", y, ignore.case =T)){
    y <- as.numeric(gsub(pattern = "M", replacement = "", x = y)) * 1024
  }else if(grepl("k|\\+", y, ignore.case = T)){
    y <- gsub("k|\\+", replacement =  "", x = y)
  }else{
    y <- "nd"
  }
})

hist(as.numeric(dados_2$kb))

options(scipen = 999)

size.app <- dados_2 %>% filter(kb != "nd") %>% mutate(kb = as.numeric(kb))

size.App.Plot <- ggplot(size.app) + geom_histogram(aes(kb))

size.App.Plot

##lubridate - Facilita manipulação de datas e horas
##install.packages("lubridate")
library(lubridate)

dmy("14-07-2021")
dmy("14072021")
dmy("1472021")
##All formats failed to parse. No formats found.
ymd("20210714")
ymd("690113")
##68 - 2068/69 - 1969
ymd("20213110")
##All formats failed to parse. No formats found. 
ydm("20213110")
mdy("10232000")
ymd_hms("2021-07-14 10:31:08")
ymd_hm("2021-07-14 10:31")
ymd_h("2021-07-14 10")
hms("10:31:08")
typeof(hms("10:31:08"))
hm("10:31")
lubridate::hours("10")
lubridate::hours(10)
data_hora <- "2021-07-14 10:21:08"
##"2021-07-14 10:21:08"
data_hora <- ymd_hms(data_hora)
##"2021-07-14 10:21:08 UTC"
##Extrair apenas o mês
month(data_hora)
##7
##Extrair apenas o dia
mday(data_hora)
##14
##Extrair apenas o ano
year(data_hora)
##2021
hour(data_hora)
minute(data_hora)
second(data_hora)
wday(data_hora, label = TRUE)
wday(data_hora)
month(data_hora, label = TRUE)
month(data_hora)

path <- "C:/Users/karen/Downloads/Linguagem_R/user_reviews.csv"
notas <- read.csv(file = path)
str(notas)
notas$data_2 <- ymd_hms(notas$data)
str(notas)
ggplot(notas) + geom_line(aes(x = data_2, y = Sentiment_Polarity))
##Datas utilizamos % antecedendo
##Y maiúsculo é ano com quatro dígitos
notas$data_2 <- parse_date_time(format(notas$data_2, "%Y-%m"), "ym")
notas$data_2
ggplot(notas) + geom_line(aes(x = data_2, y = Sentiment_Polarity))
##Pacote dplyr para médias
media_nota <- notas %>% group_by(data_2) %>% summarise(media = mean(Sentiment_Polarity))
nota_plot <- ggplot(media_nota) + geom_line(aes(x = data_2, y = media))

##Gráficos até aqui
rating.Histogram
freq.Category.Plot
type.Plot
size.App.Plot
rating_class_plot
nota_plot

rating.Histogram
##inserir título no gráfico
rating.Histogram <- rating.Histogram + ggtitle("Histograma Rating")
##centralizar título
##0.5 pois o valor vai de 0 a 1
rating.Histogram <- rating.Histogram + theme(plot.title = element_text(hjust = 0.5))
##alterar cor do layout do gráfico
rating.Histogram <- rating.Histogram + theme_bw()

freq.Category.Plot
freq.Category.Plot <- freq.Category.Plot + ggtitle("Quantidade de Apps por Categoria")
##Titulos/Rotulos dos eixos
freq.Category.Plot <- freq.Category.Plot + xlab("Categoria") + ylab("Quantidade")
freq.Category.Plot <- freq.Category.Plot + geom_bar(aes(Var1, Freq), fill = "darkcyan", stat = "identity")
##freq.Category.Plot + geom_bar(aes(Var1, Freq, fill = Var1), stat = "identity")
##freq.Category.Plot + geom_bar(aes(Var1, Freq, fill = Freq), stat = "identity")
freq.Category.Plot + theme_dark()
freq.Category.Plot <- freq.Category.Plot + theme_bw()
freq.Category.Plot

type.Plot
blank_theme <- theme_minimal() +
               theme(
                  ##ggplot não suporta o gráfico de pizza, a gente adapta. 
                  ##Função que vai limpar os erros do gráfico
                  axis.title.x = element_blank(), 
                  axis.title.y = element_blank(), 
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.ticks = element_blank(), 
                  axis.text.x = element_blank()
               )
type.Plot <- type.Plot + blank_theme
library(scales)
type.Plot <- type.Plot + geom_text(aes(x = "", y = Freq/2,
                          label = percent(Freq/sum(Freq))), size = 5)
type.Plot <- type.Plot + scale_fill_discrete(name = "Tipo App")
type.Plot <- type.Plot + ggtitle("Tipo de Aplicações") + theme(plot.title = element_text(hjust = 0.5))
type.Plot

size.App.Plot <- size.App.Plot + ggtitle("Histograma Tamanho dos Aplicativos")
size.App.Plot + geom_histogram(aes(kb), fill = "blue")
rainbow(5) ##Função disponibiiza hexadecimal de cores
size.App.Plot + geom_histogram(aes(kb), fill = rainbow(30))
size.App.Plot <- size.App.Plot + geom_histogram(aes(kb, fill = ..x..) ) +
                    scale_fill_gradient(low = "blue", high = "yellow") + guides(fill = FALSE)
size.App.Plot <- size.App.Plot + xlab("Tamanho App (em kb)") + ylab("Quantidade de Apps")
size.App.Plot <- size.App.Plot + theme_bw()

rating_class_plot
rating_class_plot <- rating_class_plot + ggtitle("Categoria de notas do App") + xlab("Categoria") +
                                                                                ylab("Quantidade")
#Prestar atenção nas cores intuitivas
##rating_class_plot + geom_bar(aes(rating_class, fill = rating_class)) + scale_fill_manual("legend", values = c("bom" = "green4", "regular" = "yellow2", "ruim" = "red"))

rating_class_plot <- rating_class_plot + geom_bar(aes(rating_class), fill = c("green4", "yellow2", "red"))
rating_class_plot <- rating_class_plot + theme_bw()

##nota_plot <- nota_plot + geom_line(aes(nota_plot), color = "blue") + theme_bw() + ggtitle("Média das Avaliações dos Apps") +
      ##xlab("Data") + ylab("Média Nota")

rating.Histogram
freq.Category.Plot
type.Plot
size.App.Plot
rating_class_plot
nota_plot

#Permite plotar varios graficos em uma unica janela
##install.packages("gridExtra")
library(gridExtra)

grid.arrange(rating.Histogram, freq.Category.Plot,
             type.Plot,
             size.App.Plot,
             rating_class_plot,
             nota_plot,
             nrow = 2, ncol = 3, heights = c(2.5, 5))