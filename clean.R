library(tidyverse)
library(MetBrewer)
library(lubridate)
library(LakeMetabolizer)


# Stanley
df1 = read_csv('Data/Stanley_compiledProfileData.csv') |> 
  mutate(date = as.Date(DateTime_Start)) |> 
  rename(depth = Depth_Target, temp = TempC, do = ODOmgL, dosat = ODOsat, spc = SpConduScm) |> 
  mutate(name = 'Stanley') |> 
  select(date, depth, temp, do, dosat, spc, name)


# Hart
df2 = read_csv('Data/JuliaHart_lake_weekly_ysi_v1.csv') |> 
  mutate(date = as.Date(sampledate)) |> 
  filter(temp < 40) |> 
  mutate(depth = round(water_depth, 0)) |> 
  group_by(date, depth) |> 
  summarise_if(is.numeric, mean, na.rm = TRUE) |> 
  rename(dosat = do_sat, spc = cond) |> 
  mutate(name = 'Hart') |> 
  select(date, depth, temp, do, dosat, spc, name)

# Gahler
df3 = read_csv('Data/ntl400_v4.csv') |> 
  filter(lakeid == 'ME') |> 
  rename(date = sampledate) |> 
  rename(temp = wtemp, do = do_raw, dosat = do_sat, spc = spec_cond) |> 
  mutate(name = 'Gahler') |> 
  select(date, depth, temp, do, dosat, spc, name)

# NTL physical
df4 = read_csv('Data/ntl29_v11.csv') |> 
  filter(lakeid == 'ME') |> 
  rename(date = sampledate) |> 
  rename(temp = wtemp, do = o2, dosat = o2sat) |> 
  mutate(spc = NA, name = 'NTL') |> 
  select(date, depth, temp, do, dosat, spc, name)

# Rexroade
df5 = read_csv('Data/Rexroade_EXO_profiles.csv') |> 
  filter(Site == 'B') |> 
  rename(date = Date, depth = Depth) |> 
  rename(temp = Temp.C, do = `ODO.mg/L`, dosat = `ODO%sat`, spc = SpCond.uScm) |> 
  mutate(name = 'Rexroade') |> 
  select(date, depth, temp, do, dosat, spc, name)

# Robertson
df6 = read_csv('Data/Robertson_Lake_Mendota_temps_long.csv') |> 
  filter(depth != 'MUD') |> 
  mutate(depth = as.numeric(depth)) |> 
  rename(date = sampledate, temp = watertemp) |> 
  mutate(do = NA, dosat = NA, spc = NA, name = 'Robertson') |> 
  select(date, depth, temp, do, dosat, spc, name)

# Microbial observatory (temp and do need joining)
# Need to calculated DoSat
df7.a = read_csv('Data/robin_MEMO_non-Poseidon_water_temps.csv') |> 
  mutate(date = ymd(paste(Year, Month, Day))) |> 
  rename(depth = Depth.m, temp = Water.Temp.C) |> 
  select(date, depth, temp) |> 
  filter(!is.na(temp))

df7.b = read_csv('Data/robin_MEMO_non-Poseidon_dissolved_oxygen.csv') |> 
  mutate(date = ymd(paste(Year, Month, Day))) |> 
  rename(depth = Depth.m, do = Dissolved.Oxygen.mg.L) |> 
  select(date, depth, do) |> 
  mutate(do = if_else(do < 0, 0, do)) |> 
  mutate(do = if_else(do > 25, NA_real_, do)) |> 
  filter(!is.na(do))

df7 = df7.a |> left_join(df7.b) |> 
  mutate(dosat = NA, spc = NA, name = 'MEMO') |>
  mutate(dosat = 100*do/o2.at.sat.base(temp)) |> 
  select(date, depth, temp, do, dosat, spc, name) 


df8 = read_csv('Data/robin_MEMO_YSI-Poseidon_2014-2019.csv') |> 
  mutate(date = ymd(paste(Year, Month, Day))) |> 
  rename(depth = Depth.m, temp = Temperature.C, do = Dissolved.Oxygen.mg.L,
         spc = Specific.Conductance.uS.cm) |> 
  mutate(do = if_else(do < -100, 0, do)) |> 
  mutate(do = if_else(do > 25, NA_real_, do)) |> 
  mutate(dosat = NA, name = 'MEMO2') |> 
  mutate(dosat = 100*do/o2.at.sat.base(temp)) |> 
  select(date, depth, temp, do, dosat, spc, name)

range(df7$dosat, na.rm = T)
range(df9$dosat, na.rm = T)

# Tran
# Need to calculated DoSat
df9 = read_csv('Data/Tran_combined.profiles.2.csv') |> 
  mutate(date = as.Date(DateTime_Start)) |> 
  rename(depth = Depth_Target, temp = TempC, do = ODOmgL,
         dosat = ODOsat, spc = SpConduScm) |> 
  mutate(dosat = NA, name = 'Tran') |> 
  mutate(dosat = 100*do/o2.at.sat.base(temp)) |> 
  select(date, depth, temp, do, dosat, spc, name)


ggplot(df1) +
  geom_point(aes(x = date, y = depth, fill = temp), shape = 21, stroke = 0.2) +
  geom_point(data = df2, aes(x = date, y = depth, fill = temp), shape = 21, stroke = 0.2) +
  geom_point(data = df3, aes(x = date, y = depth, fill = temp), shape = 21, stroke = 0.2) +
  geom_point(data = df4, aes(x = date, y = depth, fill = temp), shape = 21, stroke = 0.2) +
  geom_point(data = df5, aes(x = date, y = depth, fill = temp), shape = 21, stroke = 0.2) +
  geom_point(data = df6, aes(x = date, y = depth, fill = temp), shape = 21, stroke = 0.2) +
  geom_point(data = df7, aes(x = date, y = depth, fill = temp), shape = 21, stroke = 0.2) +
  geom_point(data = df8, aes(x = date, y = depth, fill = temp), shape = 21, stroke = 0.2) +
  geom_point(data = df9, aes(x = date, y = depth, fill = temp), shape = 21, stroke = 0.2) +
  scale_y_reverse() +
  xlim(as.Date('2010-01-01'), NA) +
  scale_fill_gradientn(colors=rev(met.brewer("OKeeffe1"))) +
  theme_bw(base_size = 9)


ggplot(df1) +
  geom_point(aes(x = date, y = depth, color = dosat), stroke = 0.2) +
  geom_point(data = df2, aes(x = date, y = depth, color = dosat), stroke = 0.2) +
  geom_point(data = df3, aes(x = date, y = depth, color = dosat), stroke = 0.2) +
  geom_point(data = df4, aes(x = date, y = depth, color = dosat), stroke = 0.2) +
  geom_point(data = df5, aes(x = date, y = depth, color = dosat), stroke = 0.2) +
  geom_point(data = df7, aes(x = date, y = depth, color = dosat), stroke = 0.2) +
  geom_point(data = df8, aes(x = date, y = depth, color = dosat), stroke = 0.2) +
  geom_point(data = df9, aes(x = date, y = depth, color = dosat), stroke = 0.2) +
  scale_y_reverse() +
  xlim(as.Date('2010-01-01'), NA) +
  scale_color_gradientn(colors=(met.brewer("OKeeffe1"))) +
  theme_bw(base_size = 9)
  
