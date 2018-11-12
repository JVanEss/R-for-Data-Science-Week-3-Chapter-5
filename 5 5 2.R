flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml, gain = dep_delay - arr_delay, speed = distance / air_time * 60)
mutate(flights_sml, gain = dep_delay - arr_delay, hours = air_time / 60, gain_per_hour = gain / hours)
transmute(flights, gain = dep_delay - arr_delay, hours = air_time / 60, gain_per_hour = gain / hours)
transmute(flights, dep_time, hour = dep_time %/% 100, minute = dep_time %% 100)
x <- 1:10
lag(x)
lead(x)
cumsum(x)
cummean(x)
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

#5.5.2 Exercises
#Exercise 1
flights_min <- select(flights, dep_time, sched_dep_time)
mutate(flights_min, dep_min = dep_time - 0000, sched_dep_min = sched_dep_time - 0000)
#Exercise 2
mutate(flights, air_time, air_min = arr_time - dep_time)
#Exercise 3
#the dep_time should be the sched_dep_time plus the dep_delay
#Exercise 4
?min_rank
flight_del <- select(flights, dep_delay)
min_rank(desc(flight_del))
#Exercise 5
1:3 + 1:10
#longer object length is not a multiple of shorter object length