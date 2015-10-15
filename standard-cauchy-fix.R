library(rstan)

m1 <- stan('standard-cauchy.stan', chains=1); 
s1 <- extract(m1, inc_warmup=TRUE, permuted=FALSE)
s1df <- s1[,1,] %>% data.frame %>% mutate(iteration=1:nrow(.)) %>% gather(parameter, value, x, lp__) %>%
  mutate(parameter=as.character(parameter), parameterization='bad')

m2 <- stan('standard-cauchy-fix.stan', chains=1); 
s2 <- extract(m2, inc_warmup=TRUE, permuted=FALSE)
s2df <- s2[,1,] %>% data.frame %>% mutate(iteration=1:nrow(.)) %>% gather(parameter, value, x, x_raw, lp__) %>%
  mutate(parameter=as.character(parameter), parameterization='good')

pl_compare_couches <- ggplot(data=rbind(s1df,s2df) %>% filter(parameter=='x'), aes(x=iteration, y=value)) +
  geom_line() + facet_grid(parameterization ~ .) + theme_minimal() +
  theme(strip.text.y=element_text(angle=0))


output_store <- '~/output-store/stan-talk'
pdf(file=file.path(output_store,'cauchy-sample-compare.pdf'), width=8, height=6, useDingbats=FALSE)
print(pl_compare_couches); dev.off()





