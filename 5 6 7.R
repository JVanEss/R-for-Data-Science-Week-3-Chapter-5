summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest, count = n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) + geom_point(aes(size = count), alpha = 1/3) + geom_smooth(se = FALSE)
delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n()
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")
flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))
flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay, na.rm = TRUE))
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) + geom_freqpoly(binwidth = 10)
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) + geom_point(alpha = 1/10)
delays %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x = n, y = delay)) + geom_point(alpha = 1/10)
library(Lahman)
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )
batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x = ab, y = ba)) + geom_point() + geom_smooth(se = FALSE)
batters %>% 
  arrange(desc(ba))
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay1 = mean(arr_delay), avg_delay2 = mean(arr_delay[arr_delay > 0])
  )
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )
not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))
not_cancelled %>%
  group_by(dest) %>%
  summarise(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))
not_cancelled %>%
  count(dest)
not_cancelled %>%
  count(tailnum, wt = distance)
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(n_early = sum(dep_time < 500))
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(hour_perc = mean(arr_delay > 60))
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))
daily %>%
  ungroup() %>%
  summarise(flights = n())

#5.6.7 Exercises
#Exercise 1
flights %>%
  group_by(flight) %>%
  summarise(n = n(),
            fifteenearly = mean(arr_delay == -15)
            fifteenlate = mean(arr_delay == 15)
flights %>%
  group_by(flight) %>%
  summarise(n = n(),
            tenlate = mean(arr_delay == 10)
flights %>%
  group_by(flight) %>%
  summarise(n = n(),
            thirtyearly = mean(arr_delay == -30)
            thirtylate = mean(arr_delay == 30)
flights %>%
  group_by(flight) %>%
  summarise(n = n(),
            ontimeperc = mean(arr_delay == 0)
flights %>%
  group_by(flight) %>%
  summarise(n = n(),
            twohrlate = mean(arr_delay == 120)
#Exercise 2
not_cancelled %>% count(dest)
not_cancelled %>% count(tailnum, wt = distance)
not_cancelled %>% group_by(dest) %>% summarise(n = n())
not_cancelled %>% group_by(tailnum) %>% summarise(n = sum(distance))
#Exercise 3
#if NA is met that means the flight was cancelled
#Exercise 4
flights %>%
  group_by(day) %>%
  summarise(cancelled = mean(is.na(dep_delay)),
            mean_dep = mean(dep_delay, na.rm = TRUE),
            mean_arr = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(y = cancelled)) +
  geom_point(aes(x = mean_dep), color = "purple") +
  geom_point(aes(x = mean_arr), color = "orange")
#Exercise 5
flights %>%
  group_by(carrier) %>%
  summarise(dep_high = max(dep_delay, na.rm = TRUE),
            arr_high = max(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(dep_high, arr_high)) %>%
  filter(1:n() == 1)