# Parameters
nGens <- 100
locus <- 10
popSize <- c(100, 500, 1000, 5000, 10000, 500000, 1000000)
startingAlleleFreq <- 0.5

# Function for single locus
fun1 <- function(nGens = 100, popSize = 500, startingAlleleFreq = 0.5, locus = 1) {
  tmp <- data.table(gen = 1:nGens, af = -1.0, popSize = popSize, locus = locus)
  tmp[gen == 1]$af <- startAlleleFreq
  
  for (i in 2:nGens) {
    afprev <- tmp[gen == (i - 1)]$af
    tmp[gen == i]$af <- rbinom(1, 2 * popSize, afprev) / (2 * popSize)
  }
  return(tmp)
}

# Simulate all loci all populations
data <- foreach(N = pop_sizes, .combine = "rbind") %:%
  foreach(locus.i = 1:n_loci, .combine = "rbind") %dopar% {
    fun1(nGens = nGens, popSize = N, startingAlleleFreq = 0.5, locus = locus.i)
  }

# Display Population Sizes
data[, popSize_label := factor(
  popSize,
  levels = pop_sizes,
  labels = c("100", "500", "1000", "5000", "10000", "5e+05", "1e+06")
)]

# Graph
p <- ggplot(data = data, aes(x = gen, y = af, group = locus)) +
  geom_line() +
  facet_grid(. ~ popSize_label) +
  labs(x = "Generation", y = "Allele Frequency")
p

