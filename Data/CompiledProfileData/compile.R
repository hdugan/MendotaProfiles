library(tidyverse)

filelist = list.files('Data/CompiledProfileData/', full.names = T, pattern = '.csv')

outputlist = list()
for (i in 1:length(filelist)) {
  outputlist[[i]] = read_csv(filelist[i])
}

# bind into dataframe
outputdf = data.table::rbindlist(outputlist, fill = TRUE)

# write csv
write_csv(outputdf, 'Data/Stanley_compiledProfileData.csv')

