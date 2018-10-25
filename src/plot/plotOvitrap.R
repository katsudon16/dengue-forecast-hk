rm(list=ls(all=TRUE))
if (!require(ggplot2)) install.packages(ggplot2)

ovitrap <- read.csv("../../dat/vector/changzhou_ovitrap.csv", header=T)
names(ovitrap) <- c("year", "month", "percentage")
ovitrap$month_txt <- month.abb[ovitrap$month]

ggplot(data=ovitrap, aes(x=month_txt, y=percentage, colour=factor(year), group=factor(year))) +
  geom_line() +
  scale_x_discrete(limits=month.abb[1:12]) +
  scale_color_brewer(palette="Paired", name="Years") +
  labs(x="Month") +
  labs(y="Ovitrap Index (%)")


