library(readr)
library(dplyr)
library(ggplot2)
dat <- read_tsv("bioc-f1000r.tsv")
dois <- dat %>%
  filter(vers==1) %>%
  arrange(date) %>%
  pull(doi)
dat <- dat %>%
  mutate(number=factor(as.integer(factor(doi,levels=dois))))
dat <- dat %>%
  mutate(topic=factor(topic, levels=c("Bioc","RNA","scRNA","ChIP","methyl","protein","microbiome","variant")))
rgb2 <- function(x,y,z) rgb(x,y,z,maxColorValue=255)
cols <- c("black",
          rgb2(230,159,0),
          rgb2(86,180,233),
          rgb2(0,158,115),
          rgb2(0,114,178),
          rgb2(213,94,0),
          rgb2(204,121,167))
ggplot(dat, aes(date, number, col=topic, shape=index, group=number)) +
  geom_line(show.legend=FALSE) + geom_point(fill="white", size=4, stroke=1) + 
  scale_shape_manual(values=c(21,19)) +
  scale_color_manual(values=cols) + 
  geom_label(data=dat[dat$vers==1,], aes(date, number, label=author),
             hjust="right", nudge_x=-30, show.legend=FALSE) +
  theme_bw() +
  xlim(as.Date(c("2015-06-01","2018-01-01")))
