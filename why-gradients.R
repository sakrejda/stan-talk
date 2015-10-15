#' Why use gradients:
#' 
library(grid)
x <- seq(0,10,length.out=200)
y <- dgamma(x=x, shape=2, scale=1)
curve_data <- data.frame(x=x, y=y)

pl_curve <- ggplot() + 
  geom_line(data=curve_data, aes(x=x, y=y)) + theme_minimal() +
  geom_point(data=data.frame(xx=5,yy=dgamma(5,2,1)), aes(x=xx, y=yy), size=5) + 
  geom_segment(data=data.frame(xx=5, yy=dgamma(5,2,1)+.03), aes(x=xx, xend=xx-.5,y=yy, yend=yy+.05), arrow = arrow(length = unit(0.2,'cm'))) +
  theme(legend.position="none") 

print(pl_curve)


output_store <- '~/output-store/stan-talk'
pdf(file=file.path(output_store,'why-gradients.pdf'), width=8, height=6, useDingbats=FALSE)
print(pl_curve); dev.off()





