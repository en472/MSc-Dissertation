
## SPM Pre-processing 

# libaries

# read in data

# ==========================================================================================

# the point of this script is to prepare the data for entry into spm. as of writing, there is
# only one experimental replicate (cells were seeded on the same chip) per condition.
# therefore, spm would not work normally as it conducts t tests across replicates. for our
# purposes, we are going to assign all the cells in each surface condition into five random
# groups so that these groups can act as our 'replicates' and spm can work. I chose five 
# arbitrarily but any number can be chosen (above 2). If you have replicates from multiple 
# experiments you can skip this step.

# otherwise this script simply calculates the kernel density estimate (relative proportion
# of number of samples at each point, like a histogram) for each real or fabricated replicate,
# then formats this in a way compatible with the spm function, which is python based.

# simply save the .csv, one per condition, and then feed it into spm.py

# =========================================================================================

# set seed for reproducibility 
set.seed(123)

# split out condition to run
surface_group <- df %>%
  filter(surface == 'nN_5_700')

# ------------------------- can skip if you have experimental replicates -------------------

# number of images in each group - useful to choose a sensible number of replicates to create
df %>%
  group_by(surface) %>%
  mutate(number_of_images = length(unique(ImageNumber_Image))) %>%
  select(number_of_images, surface) %>%
  distinct()

# assign images randomly into one of 5 groups - generates a plot to check these are reasonable
surface_group %>%
  mutate(density_group = sample(rep(1:5, length.out = n()))) %>%
  mutate(density_group = as.factor(density_group)) %>%
  # change orientation values to absolute values and adjust
  mutate(offset_orientation_cells = abs(offset_orientation_cells)) %>%
  group_by(density_group) %>%
  # plot to check
  ggplot(aes(x = offset_orientation_cells, colour = density_group)) +
  geom_density(linewidth = 1) +
  #scale_colour_manual(values = c('flat_medium_density' = 'darkgrey')) +
  theme_minimal() +
  scale_x_continuous(breaks = c(90, 45, 0, -45, -90)) +
  labs(
    title = 'density plots of five random groupings'
  )

# assign into groups and label them
surface_group <- surface_group %>%
  mutate(replicate_group = sample(rep(1:5, length.out = n()))) %>%
  mutate(replicate_group = as.factor(replicate_group)) %>%
  # change orientation values to absolute values
  mutate(offset_orientation_cells = abs(offset_orientation_cells))


# -----------------------------------------------------------------------------------------

# NOTE: if you have experimental replicates then you will need to add a factor column to
# the data frame with '1', '2', '3' etc. to label out your replicates before the next step

# Calculate Density: Run once per condition ----------------

# set up empty data frame for export
density_export = data.frame()

for(group in 1:5){
  
  # extract orientation values
  orientation <- as.numeric(surface_group$offset_orientation_cells[surface_group$replicate_group == group])
  
  # calculate density
  density <- density(orientation)
  
  # check - can run this for certainty
  #plot(density$x, density$y)
  
  # join into dataframe
  density_export <- rbind(density_export, density$y)
  
}

# rename col headers

colnames(density_export) <- 1:512

# export

write.csv(density_export, 'nN_5_700_spm_cells.csv')

# done! below is code to generate two spm-compatible .csv files to test spm with, ---------------
# one is a regular sine wave and the other is a straight line.

# model sine wave

x_axis <- seq(0, 5 * pi, length.out = 512)

y_axis <- 20 * sin(1 * x_axis + 0)

line = rep(0, times = 512)

# plot to show example

plot(x = x_axis, y = y_axis, type = 'l', col = 'blue')
lines(x = x_axis, y = line, col = 'red')

## format the sine and line data for spm

sine_export <- data.frame()

line_export <- data.frame()

for(n in 1:8){
  
  # add in some variance (to enable statistical test)
  
  sine_wave <- y_axis + rnorm(length(y_axis), mean = 0, sd = 2)
  
  straight_line <- line + rnorm(length(line), mean = 0, sd = 2)
  
  # add to df
  
  sine_export <- rbind(sine_export, sine_wave)
  
  line_export <- rbind(line_export, straight_line)
  
}

# change colnames

colnames(sine_export) <- 1:512

colnames(line_export) <- 1:512

# export

write.csv(sine_export, 'sine_wave.csv')

write.csv(line_export, 'straight_line.csv')


