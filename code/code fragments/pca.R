#reading the file and scaling and centering
crime = read_csv(here("data/Crime raw data.csv"))
L = crime[, -(1:2)]
L = scale(L, center=TRUE, scale=TRUE)

pc1 = prcomp(L, scale=TRUE, rank=18)

#summary(pc1)
#var_explained = pc1$sd^2/sum(pc1$sd^2)*100

#plot the scree plot to decide the right number of components
# qplot(c(1:37), var_explained) + 
#   geom_line() + 
#   xlab("Principal Component") + 
#   ylab("Variance Explained") +
#   ggtitle("Scree Plot") +
#   ylim(0, 20)

loadings_summary = pc1$rotation %>%
  as.data.frame()%>%
  rownames_to_column('crime_category')

#for simplicity we plot only first 2 components
ggplot(loadings_summary) +geom_col(aes(x=reorder(crime_category, PC1),y=PC1))+xlab("PC1")+ylab("Crime Category")+coord_flip()
ggsave(here("figures/PC1.png"), width=8, height=4.5)

ggplot(loadings_summary) +geom_col(aes(x=reorder(crime_category, PC2),y=PC2))+xlab("PC2")+ylab("Crime Category")+coord_flip()
ggsave(here("figures/PC2.png"), width=8, height=4.5)

df3 = round(pc1$rotation,2) %>%
  kbl(caption = "Principal Components of crime categories") %>%
  kable_classic(full_width = F, html_font = "Cambria")
df3
