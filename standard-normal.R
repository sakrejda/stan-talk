library(rstan); m1 <- stan('standard-normal.stan', chains=1); 
s1 <- extract(m1, inc_warmup=TRUE, permuted=FALSE)
s1df <- s1[,1,] %>% data.frame %>% mutate(iteration=1:nrow(.)) 

pl_traces <- ggplot(data=s1df %>% gather(parameter, value, x, lp__), aes(x=iteration, y=value)) +
  geom_line() + facet_grid(parameter ~ ., scales='free_y') + theme_minimal() +
  theme(strip.text.y=element_text(angle=0))


output_store <- '~/output-store/stan-talk'
pdf(file=file.path(output_store,'normal-sample.pdf'), width=8, height=6, useDingbats=FALSE)
print(pl_traces); dev.off()



