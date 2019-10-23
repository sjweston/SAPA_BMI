keys = read.csv("data/superKey.csv", header = TRUE, row.names = 1)

keys = keys %>%
  select(contains("SPI_135")) 

#identify rows with all 0's
row_remove = apply(keys, 1, function(row) all(row == 0))
keys = keys[!(row_remove), ]

#save as key matrix
key.matrix = keys

# create vectors with names of scales
keys = names(keys)
names(keys) = keys

SPI_27_names = gsub("SPI_135_27_5_", "", keys)
SPI_5_names = c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")
names(SPI_5_names) = names(SPI_27_names)[1:5]
SPI_27_names = SPI_27_names[6:32]
#names(SPI_27_names) = SPI_27_names
SPI_27_names = gsub("([a-z])([A-Z])", "\\1 \\2", SPI_27_names)

all_names = c(SPI_5_names, SPI_27_names)
all_names[length(all_names)+1] = "Cognitive Ability"
names(all_names)[length(all_names)] = "cog"
