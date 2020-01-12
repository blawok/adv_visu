library(reshape2)
library(ggplot2)
library(plotly)
library(reshape2)

getLowerTri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

df_co <- df_corr[, -which (names(df_corr) %in% c("date_time", "ID"))] %>% 
  na.omit()

cormat <- round(cor(df_co),2)

reorderCormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
cormat <- reorderCormat(cormat)
lower_tri <- getLowerTri(cormat)

melted_cormat <- melt(lower_tri, na.rm = TRUE)

gg_corr <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1.0, 0.5),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
# Print the heatmap

correlation_plot <- ggplotly(gg_corr)
correlation_plot

save(correlation_plot, file = "./objects/corr_plot.RData")

save(melted_cormat, file = "./objects/corr_plot_data.RData")
