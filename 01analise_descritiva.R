library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(corrplot)
library(leaflet)

pacientes <- read.table('./data/pacientes.csv', sep=',', h=T)
head(pacientes)

######################################################

summary(pacientes$Longitude)
summary(pacientes$Latitude)

# medidas para a idade
pacientes %>% group_by(Medico) %>% 
  summarise(min = min(Idade), 
            media = mean(Idade),
            mediana = median(Idade),
            max = max(Idade),
            vari�ncia = var(Idade)
  )

# medidas para a dist�ncia
pacientes %>% group_by(Medico) %>% 
  summarise(min = min(dist), 
            media = mean(dist),
            mediana = median(dist),
            max = max(dist),
            vari�ncia = var(dist)
  )

######################################################

fill_color <- 'royalblue'

######################################################

# frequ�ncia de atendimento por m�dicos
pacientes %>%
  ggplot(aes(x=Medico)) + geom_bar(fill=fill_color) + theme_minimal()


######################################################

# gr�fico de histograma com boxplot
hist_boxplot <- function(var) {
  g1 <- ggplot(pacientes, aes(var)) + geom_histogram(fill=fill_color, color='white') +
    theme_minimal() + xlim(range(var))
  
  g2 <- ggplot(pacientes, aes(var)) + geom_boxplot(fill=fill_color, color='black') +
    theme_minimal() + xlim(range(var))
  
  grid.arrange(g1,g2, ncol=1)
}

# distribui��o da idade
hist_boxplot(pacientes$Idade)

# distribui��o da dist�ncia
hist_boxplot(pacientes$dist)

######################################################

# gr�fico de boxplot de acordo com o m�dico
boxplot_idade <- function(var) {
  ggplot(pacientes, aes(x=Medico, y=var)) + geom_boxplot(fill= fill_color) +
    theme_minimal() 
}

# boxplot para a idade de acordo com o m�dico
boxplot_idade(pacientes$Idade)

# boxplot para a dist�ncia de acordo com o m�dico
boxplot_idade(pacientes$dist)


######################################################

# correla��o
plot(pacientes[,4:5])
cor(pacientes[,4:5])

# N�o possui correla��o idade dist

######################################################

id<-c(1:2042)
pacientes<-data.frame(pacientes,id)
# mapas

cof <- colorFactor(c("red", "blue", "orange",'black',"green","pink","purple"), domain=c("A", "B", "C","D","E","F","G"))
medicos$Local <- factor(medicos$Local)

new <- c("red3","blue","orange","black","green","pink","purple")[medicos$Local]

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'white',
  library = 'ion',
  markerColor = new
)
m <- leaflet(pacientes)  %>% 
  addProviderTiles("Esri") %>% 
  setView(-51.931180, -23.415453, zoom = 7) %>% 
  addCircleMarkers(~Longitude, ~Latitude, weight = 3, radius=4, 
                   color=~cof(Medico), stroke = F, fillOpacity = 0.9)  %>%
  addLegend("bottomright", colors= c("red", "blue", "orange","black","green","pink","purple"), labels=c("A'", "B", "C","D","E","F","G"), title="Medico")%>%
  addAwesomeMarkers(lng = ~medicos$Longitude, lat=~medicos$Latitude, icon=icons,
                    popup = ~medicos$Local)
m
# categorizando idade

pacientes$Categoria[pacientes$Idade < 40] = "menor que 40"
pacientes$Categoria[pacientes$Idade >= 40 & pacientes$Idade < 60] = "40 a 60"
pacientes$Categoria[pacientes$Idade >= 60] = "maior que 60"

#grafico idade
barplot(table(pacientes$Categoria))
# mapa por idade
cof <- colorFactor(c("red", "blue", "orange"), domain=c("menor que 40","40 a 60","maior que 60"))
m <- leaflet(pacientes)  %>% 
  addProviderTiles("Esri") %>% 
  setView(-51.931180, -23.415453, zoom = 7) %>% 
  addCircleMarkers(~Longitude, ~Latitude, weight = 3, radius=4, 
                   color=~cof(pacientes$Categoria), stroke = F, fillOpacity = 0.9)  %>%
  addLegend("bottomright", colors= c("red", "blue", "orange"), labels=c("Entre 40 anos e 60 anos","Maior que 60 anos","Menor que 40 anos"), title="Idade")
m
# Fazendo estudo dos medicos um a um pela idade
dadosa<-filter(pacientes, Medico == "A")
dadosb<-filter(pacientes, Medico == "B")
dadosc<-filter(pacientes, Medico =="C")
dadosd<-filter(pacientes, Medico =="D")
dadose<-filter(pacientes, Medico == "E")
dadosf<-filter(pacientes, Medico == "F")
dadosg<-filter(pacientes, Medico == "G")
#grafico idade do medico a
barplot(table(dadosa$Categoria))
#grafico idade do medico b
barplot(table(dadosb$Categoria))
#grafico idade do medico c
barplot(table(dadosc$Categoria))
#grafico idade do medico d
barplot(table(dadosd$Categoria))
#grafico idade do medico e
barplot(table(dadose$Categoria))
#grafico idade do medico f
barplot(table(dadosf$Categoria))
#grafico idade do medico g
barplot(table(dadosg$Categoria))

