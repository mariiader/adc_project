# As timeseries.

featsTS <- read.csv.zoo(
  "C:/Users/hendr/Documents/Uni_Konstanz/Master/Semester_4/Challenge/data/dengue_features_train.csv",
  sep=",", stringsAsFactors = F)

labels <- read.csv.zoo(
  "C:/Users/hendr/Documents/Uni_Konstanz/Master/Semester_4/Challenge/data/dengue_labels_train.csv",
  sep=",", stringsAsFactors = F) 