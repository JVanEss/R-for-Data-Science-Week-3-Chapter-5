flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)
popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)
popular_dests
popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)

#5.7.1 Exercises
#Exercise 2
flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(prop_time = sum(arr_delay <= 30)/n(),
            mean_arr = mean(arr_delay, na.rm = T),
            fl = n()) %>%
  arrange(desc(prop_time))
#Exercise 3
flights %>%
  group_by(hour) %>%
  filter(!is.na(dep_delay)) %>%
  summarise( delay = mean( dep_delay > 0 , na.rm = T)) %>%
  ggplot(aes(hour, delay, fill = delay)) + geom_col() 
#Exercise 4
flights %>%
  group_by(dest) %>%
  filter(!is.na(dep_delay)) %>%
  summarise(tot_mins = sum(dep_delay[dep_delay > 0]))
flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(tailnum, dest) %>%
  summarise(m = mean(dep_delay > 0), n = n()) %>%
  arrange(desc(m))
#Exercise 5
flights %>%
  mutate(new_sched_dep_time = lubridate::make_datetime(year, month, day, hour, minute)) %>%
  arrange(new_sched_dep_time) %>%
  mutate(prev_time = lag(dep_delay)) %>%
  filter(between(dep_delay, 0, 300), between(prev_time, 0, 300)) %>%
  select(origin, new_sched_dep_time, dep_delay, prev_time) %>%
  ggplot(aes(dep_delay, prev_time)) + geom_point(alpha = 1/10) +
  geom_smooth()
#Exercise 6
flights %>%
  group_by(dest) %>%
  arrange(air_time) %>%
  slice(1:5) %>%
  select(tailnum, sched_dep_time, sched_arr_time, air_time) %>%
  arrange(air_time)
#Exercise 7
flights %>%
  group_by(dest) %>%
  filter(n_distinct(carrier) > 2) %>%
  group_by(carrier) %>%
  summarise(n = n_distinct(dest)) %>%
  arrange(-n)
#Exercise 8
flights %>%
  count(dep_delay > 60)
