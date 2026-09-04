
# GMM normalisation for DNA staining

# Original script from Julia

# libraries =============================================

library(tidyverse)   # dplyr, ggplot2, purrr, tidyr
library(dplyr)      # should come with tidyverse but sometimes doesn't
library(mclust)      # Mclust 
library(purrr)   # should come with tidyverse but sometimes doesn't
library(mixtools)    # normalmixEM

# read in data -----------------------------------------------

df <- read.csv('individual_objects_filtered.csv')

# take random subsample to reduce porcessing time
df <- df_whole %>%
  sample_n(10000)

# assign grouping factor - image
Group <- "ImageNumber_Image"

## Mutate Metadata_Well to 'factor' (if not already) for grouping cells by well (or factor of choice) - note that GMM won't work if there are too few cells per group
df <- df %>%
  mutate(!!Group := as.factor(.data[[Group]]))

## Assign to generic name (feature) to the measurement to be normalised
feature <- "Intensity_MeanIntensity_CorrectedDNA_Nuclei"
normalised_feature <- paste0(feature, "_normalised")

## Do a visual inspection of your whole dataset to set reasonable ranges for plots
# Plot all cells together to inspect the range for the designated feature
ggplot(df, aes(x = .data[[feature]])) +
  geom_histogram(bins = 200) +
  ggtitle("All images combined")

# Then set the max x value (xlim) to display in subsequent plots based on the histogram
x_max <- 200


## Optional - filter outliers (mis-segmented nuclei, debris, artefacts)
df <- df %>%
  filter(.data[[feature]] <= 200)  # exclude values above 6n, e.g. very bright contaminants or multiple merged nuclei


## You can also make a density plot of raw values per well to see whether peaks seem to line up across replicates
ggplot(df, aes(x = .data[[feature]], colour = .data[[Group]])) +
  geom_density(bins = 200) +
  guides(colour = 'none') +
  #ylim(0, 0.5) +
  theme_classic()


## Fit GMM with multi-seed approach
# Group by wells (or factor of choice) and extract the raw data values
gmm_constrained <- df %>%
  group_by(.data[[Group]]) %>%
  mutate(sample_number = n()) %>%
  filter(sample_number > 2) %>%
  summarise({
    x <- .data[[feature]]
    # Estimate the first peak (mu 1) as a starting point, looking only at the bottom 75% of values 
    mu1 <- density(x)$x[which.max(density(x)$y[density(x)$x < quantile(x, 0.75)])]
    # Fit GMM using random seeds. The 'normalmixEM' is sensitive to the starting conditions, so different seeds may converge at different optima (solutions)
    #  Running it with various seeds gives it a range of starting values. These numbers can be anything and you can use any number of them (though more will eventually produce diminishing returns for longer compute times)
    seeds <- c(1, 7, 42, 99, 100, 123, 456, 999)
    fits <- lapply(seeds, function(s) {
      set.seed(s)
      # Fit a two-component Gaussian Mixture Model. 
      # The rough estimate of the first peak, mu 1, and 2*mu 1, are used to tell the algorithm where to start searching, which improves efficiency and convergence on a good fit.
      # The 'mean.constr' constrains the model so that the second peak is 2 times the first, which would be expected for DNA content. Note that 0 cells in the second peak is not an issue.
      # If there is not a clear second peak or you don't know what to expect, just leave out the 'mean.const' instruction. If the second peak is expected but not 2*mu 1, then the "2a" term can be changed to 1.5a, 3a, etc.
      # TryCatch and the null filter remove any seeds that caused errors 
      tryCatch(
        normalmixEM(x, mu = c(mu1, mu1*2), mean.constr = c("a", "2a"), k = 2),
        error = function(e) NULL
      )
    })
    fits <- Filter(Negate(is.null), fits)
    # Pick the best fit from log likelihood 
    # The algorithm works iteratively, using the log-likelihood to measure how well the Gaussian mixture fits the observed data.
    # 'f$loglik' is a vector of likelihoods, one per iteration. At some point it plateaus, converging on an optimum.
    # 'which.max' asks which seed converged on the best log-likelihood for each well (or factor grouping)
    fit <- fits[[which.max(sapply(fits, function(f) tail(f$loglik, 1)))]]
    # Extact and report the GMM parameters
    # 'Tibbles' are dataframes with some tweaks that make them easier to use in tidyverse.
    # The values returned are the x coordinates of the first and second peaks (fit$mu), the proportion of cells in each Gaussian (lambda), and the SD for each Gaussian (sigma) 
    tibble(peak1_gmm = fit$mu[1], peak2 = fit$mu[2],
           lambda1 = fit$lambda[1], lambda2 = fit$lambda[2],
           peak1_sigma1 = fit$sigma[1], sigma2 = fit$sigma[2])
  })

# Join the GMM peak 1 value and sigma into the single cell dataframe by well (or other factor grouping)
df <- df %>%
  left_join(gmm_constrained %>% select(.data[[Group]], peak1_gmm, peak1_sigma1), 
            by = Group)
# Calculate the normalized value (divided by the first peak) as a new column
df <- df %>%
  mutate(normalised_feature := .data[[feature]] / peak1_gmm)

# plot normalised features
ggplot(df, aes(x = normalised_feature)) +
  geom_histogram(bins = 100) +
  ggtitle("All images combined")

# Density plot of all normalized values per image - takes ages to run
ggplot(df, aes(x = normalised_feature, colour = .data[[Group]])) +
  geom_density() +
  guides(colour = 'none') +
  theme_classic()


## Rename the general 'normalised_feature' with the name of the variable + '_normalised'
final_name <- paste0(feature, "_normalised")
df <- df %>%
  rename(!!final_name := normalised_feature)


# Optional: You can replace 'Peak1' with another string, e.g. 'G1' in the summary statistics and df dataframes
df <- df %>%
  rename_with(~ gsub("peak1", "G1", .x))


# done, export csv.

write.csv(df, 'object_data_DNA_Normalised.csv')






