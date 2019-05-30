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

topic.lvls <- c("Bioc","RNA","sc","ChIP","methyl","protein","microb","variant")
dat <- dat %>%
  mutate(topic=factor(topic, levels=topic.lvls))

rgb2 <- function(x,y,z) {
  rgb(x,y,z,maxColorValue=255)
}
cols <- c("black",
          "grey50",
          rgb2(230,159,0),
          rgb2(86,180,233),
          rgb2(0,158,115),
          rgb2(0,114,178),
          rgb2(213,94,0),
          rgb2(204,121,167))

png(file="bioc-f1000r-1.png",width=1000,height=800,res=120)
ggplot(dat, aes(date, number, col=topic, shape=index, group=number)) +
  geom_line(show.legend=FALSE) +
  geom_point(fill="white", size=4, stroke=1) + 
  scale_shape_manual(values=c(21,19)) +
  scale_color_manual(values=cols) + 
  geom_text(data=dat[dat$vers==1,],
            aes(date, number, label=author),
            hjust="right", nudge_x=-30, show.legend=FALSE) +
  theme_bw() +
  xlim(as.Date(c("2015-09-01","2019-06-01"))) +
  ggtitle("Workflows on F1000Research's Bioconductor Channel")
dev.off()

png(file="bioc-f1000r-2.png",width=800,height=800,res=120)
dat %>%
  filter(vers==1) %>%
  ggplot(aes(topic, fill=topic)) +
  geom_bar() +
  scale_fill_manual(values=cols) + 
  theme_bw()
dev.off()

png(file="bioc-f1000r-3.png",width=800,height=800,res=120)
dat %>%
  filter(vers==1) %>%
  ggplot(aes(nauth)) +
  geom_bar(binwidth=1) +
  theme_bw() + xlab("number of authors")
dev.off()

png(file="bioc-f1000r-4.png",width=800,height=800,res=120)
dat %>%
  filter(vers==1) %>%
  ggplot(aes(nrev, fill=index)) + 
  geom_bar(position="dodge") +
  theme_bw() +
  ggtitle("More reviewers helps indexing on first submission")
dev.off()

png(file="bioc-f1000r-5.png",width=800,height=800,res=120)
dat %>%
  group_by(doi) %>%
  filter(vers==max(vers)) %>%
  ggplot(aes(nrev, fill=index)) + 
  geom_bar(position="dodge") +
  theme_bw() +
  ggtitle("More reviewers helps indexing on last submission")
dev.off()

png(file="bioc-f1000r-6.png",width=800,height=800,res=120)
dat %>%
  filter(vers==1) %>%
  ggplot(aes(pages, group=index, fill=index, col=index)) + 
  geom_histogram(binwidth=15, position="dodge") + 
  theme_bw() +
  ggtitle("More pages doesn't mean indexing on first submission")
dev.off()

png(file="bioc-f1000r-7.png",width=800,height=800,res=120)
dat %>%
  group_by(doi) %>%
  filter(vers==max(vers)) %>%
  ggplot(aes(pages, group=index, fill=index, col=index)) + 
  geom_histogram(binwidth=15, position="dodge") + 
  theme_bw() +
  ggtitle("More pages helps indexing on last submission")
dev.off()
