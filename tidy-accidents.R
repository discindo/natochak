# tidy accidents

library(tidyverse)
library(stringr)
library(lubridate)

acc <- read_csv("cycling-accidents-mk/acc.csv") %>% as_tibble()

(
  acc2 <- tibble(
    date_of_record = parse_datetime(acc$DateOfRecord, format = "%d%.%m%.%Y"),
    date_of_event  = parse_datetime(acc$DateOfEvent, format = "%d%.%m%.%Y"),
    day_event      = day(date_of_event),
    month_event    = month(date_of_event, label = TRUE),
    year_event     = year(date_of_event),
    week_day_event = weekdays(date_of_event),
    time_event     = parse_time(acc$Hour),
    hour_event     = hour(time_event),
    place          = str_replace(acc$Place, "\\.", "\\. "),
    type_of_road   = acc$TypeOfRoad,
    # acc$Event is only `traffic accident', omitted here
    # acc$Vehicle1 is only `bicycle', omitted here
    vehicle        = acc$Vehicle2,
    outcome        = acc$Outcome,
    age_cyclist    = acc$AgeV1,
    age_motorist   = acc$AgeV2,
    car_type       = acc$TypeV2,
    crossroad      = acc$Crossroad,
    # both gender variables omitted b/c they are mostly missing data
    # alcohol omitted because it has only 3 data points
    region         = acc$Region,
    municipality   = acc$Mun2,
    municipality_l = acc$MunLatin
  )
)

acc2$type_of_road2 <- case_when(
  acc2$type_of_road == "Open" ~ "Отворен пат",
  acc2$type_of_road == "Rural" ~ "Селски пат",
  acc2$type_of_road == "Urban" ~ "Градски пат")

acc2$week_day_event2 <- case_when(
  acc2$week_day_event == "Monday" ~ "Понеделник",
  acc2$week_day_event == "Tuesday" ~ "Вторник",
  acc2$week_day_event == "Wednesday" ~ "Среда",
  acc2$week_day_event == "Thursday" ~ "Четврток",
  acc2$week_day_event == "Friday" ~ "Петок",
  acc2$week_day_event == "Saturday" ~ "Сабота",
  acc2$week_day_event == "Sunday" ~ "Недела"
)

acc2$month_event2 <- case_when(
  acc2$month_event == "Jan" ~ "Јануари",
  acc2$month_event == "Feb" ~ "Февруари",
  acc2$month_event == "Mar" ~ "Март",
  acc2$month_event == "Apr" ~ "Април",
  acc2$month_event == "May" ~ "Мај",
  acc2$month_event == "Jun" ~ "Јуни",
  acc2$month_event == "Jul" ~ "Јули",
  acc2$month_event == "Aug" ~ "Август",
  acc2$month_event == "Sep" ~ "Септември",
  acc2$month_event == "Oct" ~ "Октомври",
  acc2$month_event == "Nov" ~ "Ноември",
  acc2$month_event == "Dec" ~ "Декември"
)

(
  mkacc <- tibble(
    `Дата на записник`              = acc2$date_of_record,
    `Дата на незгода`               = acc2$date_of_event,
    `Ден на незгода`                = acc2$day_event,
    `Месец на незгода`              = acc2$month_event2,
    `Година на незгода`             = acc2$year_event,
    `Ден од неделата`               = acc2$week_day_event2,
    `Време на незгода`              = acc2$time_event,
    `Час на незгода`                = acc2$hour_event,
    `Место на незгода`              = acc2$place,
    `Тип на пат`                    = acc2$type_of_road2,
    `Соучесник во незгодата`        = acc2$vehicle,
    `Последица`                     = acc2$outcome,
    `Возраст на велосипедист`       = acc2$age_cyclist,
    `Возраст на автомобилист`       = acc2$age_motorist,
    `Тип на возило`                 = acc2$car_type,
    `Раскрсница`                    = acc2$crossroad,
    `Регион`                        = acc2$region,
    `Општина`                       = acc2$municipality,
    `Општина латиница`              = acc2$municipality_l
  )
)

write_csv(mkacc, path = "cycling-accidents-mk/mkacc.csv")