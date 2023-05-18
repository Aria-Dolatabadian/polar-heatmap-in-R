library(grid)
library(gtable)
library(reshape)
library(ggplot2)
library(plyr)

nba <- read.csv("data.csv")

nba$Name <- with(nba, reorder(Name, PTS))
nba.m <- melt(nba)

nba.m <- ddply(nba.m, .(variable), transform, value = scale(value))

# Convert the factor levels (variables) to numeric + quanity to determine    size   of hole.
nba.m$var2 = as.numeric(nba.m$variable) + 15

# Labels and breaks need to be added with scale_y_discrete.
y_labels = levels(nba.m$variable)
y_breaks = seq_along(y_labels) + 15


nba.labs <- subset(nba.m, variable==levels(nba.m$variable)    [nlevels(nba.m$variable)])

nba.labs <- nba.labs[order(nba.labs$Name),]
nba.labs$ang <- seq(from=(360/nrow(nba.labs))/1.5, to=(1.5* (360/nrow(nba.labs)))-360, length.out=nrow(nba.labs))+80
nba.labs$hjust <- 0
nba.labs$hjust[which(nba.labs$ang < -90)] <- 1
nba.labs$ang[which(nba.labs$ang < -90)] <- (180+nba.labs$ang)[which(nba.labs$ang < -90)]

p<-ggplot(nba.m, aes(x=Name, y=var2, fill=value)) +
  geom_tile(colour="white") +
  geom_text(data=nba.labs, aes(x=Name, y=var2+1.5,
                           label=Name, angle=ang, hjust=hjust), size=2.5) +
  scale_fill_gradient(low = "red", high = "blue") +
  ylim(c(0, 50)) +
  coord_polar(theta="x") +
  theme(panel.background=element_blank(),
    axis.title=element_blank(),
    panel.grid=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    axis.text.y=element_text(size=5))+ theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
lab = textGrob((paste("G  MIN  PTS  FGM  FGA  FGP  FTM  FTA  FTP  X3PM X3PA X3PP ORB DRB  TRB  AST  STL  BLK  TO  PF")),
   x = unit(.1, "npc"), just = c("left"), 
   gp = gpar(fontsize = 7))

gp = ggplotGrob(p)
gp = gtable_add_rows(gp, unit(10, "grobheight", lab), -1)
gp = gtable_add_grob(gp, lab, t = -2, l = gp$layout[gp$layout$name == "panel",]$l)

grid.newpage()
grid.draw(gp)
