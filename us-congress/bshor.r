## Congressional ideological scores by Boris Shor, 2012.
## 2014-10-16

## -- reference ----------------------------------------------------------------

## Boris Shor, "Individual 2012 Congressional Candidate Scores", 31 October 2012
## http://research.bshor.com/?s=congressional

## -- packages -----------------------------------------------------------------

library(ggplot2)
# library(xlsx) # uncomment if you need to download and convert the data

## -- dataset ------------------------------------------------------------------

file = "bshor.csv"
if(!file.exists(file)) {

  download.file("http://bshor.files.wordpress.com/2012/10/cands_house2012_boris_shor.xlsx",
                file, mode = "wb")

  # read XLSX
  shor = read.xlsx(file, 1)
  
  # write CSV
  write.csv(shor, file, row.names = FALSE)

}

# read CSV
shor = read.csv(file)

## -- variables ----------------------------------------------------------------

# top rows
head(shor[, 1:9])

# party id
table(shor$party, exclude = NULL)

# ideological scores
summary(shor$score)

# scores by party
aggregate(score ~ party, data = shor, summary)

aggregate(score ~ party, data = shor, mean)
aggregate(score ~ party, data = shor, function(x) { sum(x) / length(x) })

aggregate(score ~ party, data = shor, sd)
aggregate(score ~ party, data = shor, function(x) { sqrt(var(x)) })

## -- plots --------------------------------------------------------------------

# relabel and reorder party id
shor$party = factor(shor$party, levels = c("R", "D"))
levels(shor$party) = c("Republicans", "Democrats")

# base plot structure
p = ggplot(data = subset(shor, party != "X"), aes(x = score)) +
  scale_fill_brewer("Party", palette = "Set1") +
  scale_colour_brewer("Party", palette = "Set1") +
  theme_linedraw(12) +
  theme(legend.position = "none")

# bar plot
p + geom_bar(aes(x = factor(round(score, 0))), position = "stack")

# histogram
p + geom_histogram(binwidth = .5, color = "white")

# histogram by party
p + geom_histogram(binwidth = .5, color = "white", aes(fill = party)) + 
  facet_grid(party ~ .)

# dot plot by party
p + geom_dotplot(aes(colour = party, fill = party), 
                 size = 1, binwidth = .1, alpha = .75)

# density by party
p + geom_density(aes(colour = party, fill = party), 
                 size = 1, binwidth = .1, alpha = .75)

# detailed density curves
p + geom_rug() + 
  aes(fill = party, colour = party) +
  geom_density(alpha = .75) +
  facet_grid(party ~ .) +
  labs(title = "Congressional candidate ideological scores, 2012\n",
       y = "density\n", x = "\nscore")

# uncomment to save last plot
# ggsave("us-congress.png", width = 11, height = 8)

# empirical cumulative distribution functions (ecdf)
p + geom_step(stat = "ecdf", aes(color = party))

# annotated boxplots
p + aes(x = party, y = score, colour = party) + 
  geom_boxplot(outlier.size = 0) + 
  geom_text(aes(label = st), size = 6) +
  geom_hline(y = 0, linetype = "dotted") + 
  coord_flip()

## have a nice day
