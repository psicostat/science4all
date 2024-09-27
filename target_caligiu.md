# Science4all_Psicostat
# hit-the-target

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```


# Colpire il Pensiero con le Domande Giuste


**Hai mai giocato a colpire un bersaglio?**


Sai com’è quando tiri una pallina e speri di fare centro, ma le palline vanno in giro dappertutto? A volte qualcuna si avvicina al centro, altre volte vanno lontane e sembrano colpire punti a caso. 

**Ecco, in psicologia capita qualcosa di simile.** Noi cerchiamo di capire le persone, ma spesso le loro risposte sono come le palline sparse sul bersaglio: ci danno un’idea, ma non dicono esattamente le coordinate del bersaglio.

# Misurare il Pensiero: Un Bersaglio Sfuggente

Immagina che il centro del bersaglio sia la cosa che vogliamo davvero **misurare**, come **la personalità, l’intelligenza o un’abilità specifica di una persona.** Il punto che colpiamo con il lancio di una pallina, invece, è la risposta che otteniamo quando facciamo domande o test. **Il nostro compito è fare in modo che le risposte colpiscano il più possibile il centro**, ma non è così semplice come sembra. 

A differenza di cose che si possono misurare direttamente, come l’altezza di un albero o la temperatura dell’acqua, **le abilità cognitive, i pensieri e le emozioni non si vedono e non si toccano**. Per questo dobbiamo usare le risposte come un riflesso di ciò che sta dentro la persona. E immaginare che il bersaglio possa muoversi nel tempo e nello spazio rendendo la misurazione in psicologia una sfida complessa e affascinante.

```{r, echo=FALSE, warning=FALSE}
library(ggplot2)

# Imposta il seme per la riproducibilità
set.seed(123)

# Crea un data frame con diversi centri e dispersioni
n <- 100

# Genera punti centrati in (0, 0) con bassa dispersione (risposte attendibili)
x_center1 <- rnorm(n, mean = 0, sd = 0.1)
y_center1 <- rnorm(n, mean = 0, sd = 0.1)

# Genera punti centrati in (1, 1) con alta dispersione (risposte meno attendibili)
x_center2 <- rnorm(n, mean = 1, sd = 0.5)
y_center2 <- rnorm(n, mean = 1, sd = 0.5)

# Genera punti centrati in (-1, -1) con media dispersione (risposte variabili)
x_center3 <- rnorm(n, mean = -1, sd = 0.3)
y_center3 <- rnorm(n, mean = -1, sd = 0.3)

# Crea un data frame con tutti i punti generati
data <- data.frame(
  x = c(x_center1, x_center2, x_center3),
  y = c(y_center1, y_center2, y_center3),
  gruppo = factor(rep(c("Percezione", "Intelligenza", "Emozioni"), each = n))
)

# Aggiungi triangolini con posizioni casuali
triangles <- data.frame(
  x = rnorm(3 * n, mean = c(0, 1, -1), sd = c(0.1, 0.5, 0.3)),
  y = rnorm(3 * n, mean = c(0, 1, -1), sd = c(0.1, 0.5, 0.3)),
  shape = sample(c(17, 18, 20), 3 * n, replace = TRUE)
)

# Palette colori esotici
color_palette <- c("Percezione" = "#D8345F", "Intelligenza" = "#34D8A5", "Emozioni" = "#3475D8")

# Disegna il bersaglio con diversi centri e dispersioni più i triangolini sparsi
bersaglio <- ggplot(data, aes(x = x, y = y, color = gruppo)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = triangles, aes(x = x, y = y, shape = factor(shape)), color = "black", size = 3, alpha = 0.4) +
  scale_color_manual(values = color_palette) +
  xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
  labs(title = "Bersagli Psicologici con Diversi Centri e Dispersioni",
       x = "Coordinate psicologiche", y = "Coordinate psicologiche") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "right",
        legend.box = "vertical") +
  guides(shape = "none")

# Mostra il plot
print(bersaglio)
```

## Il Problema della Misurazione: Quando le Palline Non Fanno Centro

Cosa succede se le palline non colpiscono il centro? In psicologia, si parla di **validità e attendibilità**. Vediamo cosa significano con il nostro gioco del bersaglio:

- **Attendibile ma Non-Valido**: Immagina di tirare dieci palline e di colpire sempre lo stesso punto, ma quel punto è lontano dal centro. Vuol dire che hai una buona mira (le palline vanno tutte insieme), ma non stai centrando l’obiettivo. In psicologia, questo succede quando le risposte sono tutte simili, ma non misurano davvero ciò che vogliamo capire. Ad esempio, se vogliamo sapere quanto una persona è creativa ma facciamo sempre domande solo sulla sua memoria, non sapremo mai quanto è creativa, anche se le risposte sono tutte uguali e coerenti.

- **Valido ma Non-Attendibile**: Ora immagina che le palline colpiscano punti diversi intorno al centro, ma in modo casuale. Alcune sono vicine al centro, altre molto lontane. Vuol dire che a volte centri il bersaglio, ma non in modo costante. In psicologia, questo capita quando le risposte ci dicono qualcosa di vero, ma sono troppo variabili. È come se avessimo colto qualcosa, ma non riusciamo a ripeterlo.

## Poche Domande, ma Buone: Come Riuscire a Fare Centro

**C’è un’altra difficoltà**: non possiamo fare centinaia di domande a una persona senza farla annoiare o stancare. **Se in un gioco hai solo poche palline**, devi cercare di tirarle nel modo migliore possibile. **Lo stesso vale per i test psicologici**: dobbiamo trovare poche domande giuste che ci aiutino a colpire il centro del bersaglio. 

**Più le domande sono mirate, più le risposte si concentrano verso il centro, e noi possiamo capire meglio la persona.**

## Colpire il Centro con le Domande Giuste

**Trovare queste domande è il nostro lavoro in psicologia.** È come avere poche palline a disposizione e voler fare sempre centro. 

Per questo **è importante studiare e scegliere con cura le domande giuste. Solo così possiamo “vedere” qualcosa che non possiamo misurare direttamente**, come i pensieri, le emozioni e la personalità.

In sintesi, il nostro compito è fare in modo che poche palline, tirate con precisione e accuratezza, ovvero con **attendibilità e validità**, ci diano l’immagine più chiara possibile di cosa c’è **dentro la persona**. Quando ci riusciamo, è come fare centro con ogni tiro!
  


```{r, echo=FALSE, warning=FALSE}
library(ggplot2)

# Funzione per creare cerchi concentrici con un centro specifico
draw_target <- function(center_x = 0, center_y = 0, inner_radii = c(1, 2, 3, 4), colors = c("#FF69B4", "#8A2BE2", "#7FFF00", "#FF4500"), alpha = 0.4) {
  target <- data.frame(
    r = rep(inner_radii, each = 100),
    theta = rep(seq(0, 2 * pi, length.out = 100), times = length(inner_radii)),
    color = rep(colors, each = 100)
  )

  target$x <- center_x + target$r * cos(target$theta)
  target$y <- center_y + target$r * sin(target$theta)

  geom_polygon(data = target, aes(x = x, y = y, group = interaction(r, color), fill = color), color = "black", alpha = alpha)
}

# Funzione per aggiungere palline con dispersione specifica
add_balls <- function(x_center, y_center, color) {
  balls <- data.frame(
    x = rnorm(4, mean = x_center, sd = 0.1),
    y = rnorm(4, mean = y_center, sd = 0.1)
  )

  geom_point(data = balls, aes(x = x, y = y), color = color, size = 4)
}

# Definizione dei centri dei bersagli
centers <- data.frame(
  x = c(0, 2, -2, 1, -1),
  y = c(0, 2, -2, -1, 1)
)

# Creazione del plot del bersaglio con centri diversi
ggplot() +
  draw_target(center_x = centers$x[1], center_y = centers$y[1], inner_radii = c(1, 2, 3, 4), colors = c("#FF69B4", "#8A2BE2", "#7FFF00", "#FF4500")) +
  draw_target(center_x = centers$x[2], center_y = centers$y[2], inner_radii = c(1, 2, 3, 4), colors = c("#FF6347", "#4682B4", "#FFD700", "#32CD32")) +
  draw_target(center_x = centers$x[3], center_y = centers$y[3], inner_radii = c(1, 2, 3, 4), colors = c("#B0E0E6", "#DC143C", "#FF8C00", "#00FA9A")) +
  draw_target(center_x = centers$x[4], center_y = centers$y[4], inner_radii = c(1, 2, 3, 4), colors = c("#6495ED", "#FF1493", "#7CFC00", "#FF4500")) +
  draw_target(center_x = centers$x[5], center_y = centers$y[5], inner_radii = c(1, 2, 3, 4), colors = c("#8B008B", "#00CED1", "#FF00FF", "#9400D3")) +
  add_balls(x_center = centers$x[1], y_center = centers$y[1], color = "#00FFFF") +
  add_balls(x_center = centers$x[2], y_center = centers$y[2], color = "#FFDEAD") +
  add_balls(x_center = centers$x[3], y_center = centers$y[3], color = "#7B68EE") +
  add_balls(x_center = centers$x[4], y_center = centers$y[4], color = "#FF69B4") +
  add_balls(x_center = centers$x[5], y_center = centers$y[5], color = "#FFD700") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none") 
```
  
