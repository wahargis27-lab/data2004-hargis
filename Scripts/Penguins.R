library(tidyverse)
library(palmerpenguins)

penguins <- penguins
glimpse(penguins)

penguins_small <- penguins %>%
  select(species, island, sex, body_mass_g, flipper_length_mm)

glimpse(penguins_small)

adelies <- filter(penguins, species == "Adelie")

bigs <- filter(penguins, body_mass_g > 5000)

glimpse(adelies)
glimpse(bigs)

kg_penguins <- penguins 
kg_penguins$body_mass_kg <- kg_penguins$body_mass_g/1000
glimpse(kg_penguins)

unique(kg_penguins$species)
# 3: Adelie    Gentoo    Chinstrap

mean(kg_penguins$body_mass_kg, na.rm = TRUE)
# 4.201754

median(kg_penguins$body_mass_kg, na.rm = TRUE)
# 4.05

mean(kg_penguins$flipper_length_mm, na.rm = TRUE)
# 200.9152

# some could have missing values. how should you deal with that?
# you gotta ignore them

#A)

#Among female penguins, which species has the greatest average body mass?

mean(filter(penguins, sex == "female", species == "Adelie")$body_mass_g, na.rm = TRUE)
# 3368.836
mean(filter(penguins, sex == "female", species == "Gentoo")$body_mass_g, na.rm = TRUE)
# 4679.741
mean(filter(penguins, sex == "female", species == "Chinstrap")$body_mass_g, na.rm = TRUE)
# 3527.206

#Gentoo has the highesrt average weight

# B

unique(penguins$island)

penguins %>%
  group_by(species, island) %>%
  summarise(
    observations = n(),
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_flipper_length))

# C

Top_Dogs <- data.frame(island = penguins$island, species = penguins$species, sex = penguins$sex, mass = penguins$body_mass_g)

Top_Dogs[order(Top_Dogs$mass, decreasing = TRUE), ]

#170    Biscoe    Gentoo   male 6300
#186    Biscoe    Gentoo   male 6050
#230    Biscoe    Gentoo   male 6000
#270    Biscoe    Gentoo   male 6000
#232    Biscoe    Gentoo   male 5950

#Viz review

penguins %>%
  group_by(species) %>%
    summarize(mean_mass = mean(body_mass_g, na.rm = TRUE)) %>%
      ggplot(aes(x = species, y = mean_mass)) +
      ggtitle("Speices and Weight") +
      geom_col()


ggplot(penguins, aes(x = body_mass_g, flipper_length_mm, shape = species)) +
  geom_point() +
  ggtitle("Speices and Size")

#Put it together
penguins %>%
  group_by(species, sex) %>%
  summarise(
    body_mass_g = mean(body_mass_g, na.rm = TRUE),
  ) %>%
  arrange(desc(body_mass_g))

#it appears sex and it holds true across species