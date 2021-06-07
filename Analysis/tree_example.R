# read in example migration data
data.for.tree <- read_csv("../Data/migexample.csv") %>% 
                 mutate(location = as.factor(location))

# train the conditional inference tree
model.ctree <- ctree(location ~ age + education + experience, data = data.for.tree,control = ctree_control(minbucket=10,mincriterion=0.9))

# export the tree to EPS file
setEPS()
postscript("../Paper/Graphics/migexample.eps")
plot(model.ctree)
dev.off()


