#reading the file and scaling and centering
fempos = read_csv(here("data/Position of females.csv"))
X = fempos[,-1]
X = scale(X, center=TRUE, scale=TRUE)
X_dist = dist(X, method='euclidean')

#hierarchical clustering based on women empowerment factors
h2 = hclust(X_dist, method='complete')
#plot(h2)
cluster2 = cutree(h2, k=5)

D2 = data.frame(X, z1 = cluster2, z2 = fempos$State)
ggplot(D2) + geom_point(aes(x=literacy, y=I4, col=factor(z1)))+
  labs(title = "Clustering of literacy rates among women who think they should be involved in major decision making",
       x = "Female Literacy Rate",
       y = "Decision Making")
ggsave(here("figures/Clustering of literacy rates among women who think they should be involved in major decision making.png"), width=8, height=4.5)

#summarise state and cluster mapping
# df1 = D2 %>%
#   select(z1,z2) %>%
#   arrange(z1)
# colnames(df1) = c("Cluster","State/UT")
# df1 = df1 %>%
#   kbl(caption = "Cluster grouping of states according to factors affecting women empowerment") %>%
#   kable_classic(full_width = F, html_font = "Cambria")
# df1

#law enforcement clustering
lawenf = read_csv(here("data/Law Enforcement.csv"))
Y = lawenf[,-1]
Y = scale(Y, center=TRUE, scale=TRUE)
Y_dist = dist(Y, method='euclidean')

#hierarchical clustering based on law enforcement factors 
h1 = hclust(Y_dist, method='complete')
#plot(h1)
cluster1 = cutree(h1, k=5)
D1 = data.frame(Y, a1 = cluster1, a2 = lawenf$State)
ggplot(D1) + geom_point(aes(x=court_pending, y=court_convic, col=factor(a1)))+
  labs(title = "Clustering of states based on law enforcement factors",
       x = "Number of cases pending for trail",
       y = "Number of cases convicted in court")
ggsave(here("figures/Clustering of states based on law enforcement factors.png"), width=8, height=4.5)


# df2 = D1 %>%
#   select(a1,a2) %>%
#   arrange(a1)
# colnames(df2) = c("Cluster","State/UT")
# df2 = df2 %>%
#   kbl(caption = "Cluster grouping of states according to law enforcement factors") %>%
#   kable_classic(full_width = F, html_font = "Cambria")
# df2