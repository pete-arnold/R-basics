# 
# Covid-19_functions.R
#

# Load the data

load_data <- function(filename) {
    read_csv(paste('data/', filename, sep=''))
}

load_data_xlsx <- function(filename, sheet) {
    read_xlsx(paste('data/', filename, sep=''), sheet=sheet)
}

# Archive the current data (before updating)

archive_data <- function(data, filename) {
    extension <- '.csv'
    tag <- paste('=', date(now()), '=',
                 str_pad(hour(now()), 2, pad='0'), 
                 str_pad(minute(now()), 2, pad='0'), 
                 str_pad(round(second(now()), 0), 2, pad='0'),
                 sep='')
    write_csv(data, paste('archive/', filename, tag, extension, sep=''))
}

# Update the data

fetch_data <- function(url) {
    data <- read.xlsx(url)
}

update_data <- function(url, filename) {
    download.file(url = url, destfile = 'data/covid-19.xlsx', mode="wb")
    data <- read_xlsx('data/covid-19.xlsx')
    write_csv(data, paste('data/', filename, sep=''))
}

update_data_wales <- function(url, filename) {
    download.file(url = url, destfile = 'data/covid-19-wales.xlsx', mode="wb")
    data <- read_xlsx('data/covid-19-wales.xlsx')
    write_csv(data, paste('data/', filename, sep=''))
}

# Clean the data

clean_data <- function(name) {
    name %>%
    mutate(dateRep=as.Date(dateRep, '%d/%m/%Y')) %>%
    arrange(countriesAndTerritories, dateRep)
}

# Remove known problem data

remove_known_problems <- function(name) {
    name %>%
    mutate(cases=ifelse(countriesAndTerritories=='China' & dateRep=='2020-02-13', NA, cases)) %>%
    mutate(cases=ifelse(countriesAndTerritories=='China' & dateRep=='2020-04-17', NA, cases),
           deaths=ifelse(countriesAndTerritories=='China' & dateRep=='2020-04-17', NA, deaths))
}

# Get country max cases and deaths

get_max_cases_deaths <- function(name) {
    name %>%
    filter(cases>0) %>%
    select(countriesAndTerritories, cases, deaths) %>%
    group_by(countriesAndTerritories) %>%
    mutate(max_cases=max(cases), max_deaths=max(deaths)) %>%
    select(-cases, -deaths) %>%
    ungroup() %>%
    distinct()
}

get_max_smoothed_cases_deaths <- function(name) {
    name %>%
    smooth_cases()
    
    name %>%
    smooth_deaths() %>%
    filter(smoothed_cases>0) %>%
    select(countriesAndTerritories, smoothed_cases, smoothed_deaths) %>%
    group_by(countriesAndTerritories) %>%
    mutate(max_smoothed_cases=max(smoothed_cases), max_smoothed_deaths=max(smoothed_deaths)) %>%
    select(-smoothed_cases, -smoothed_deaths) %>%
    ungroup() %>%
    distinct()
}

get_first_case <- function(name) {
    name %>%
    filter(cases>0) %>%
    arrange(countriesAndTerritories, dateRep) %>%
    select(countriesAndTerritories, cases, deaths, dateRep) %>%
    group_by(countriesAndTerritories) %>%
    filter(row_number()==1) %>%
    rename(firstCase=dateRep) %>%
    select(countriesAndTerritories, firstCase) %>%
    ungroup() %>%
    distinct()
}

get_first_case_at_pc <- function(name, pc) {
    name %>%
    arrange(countriesAndTerritories, dateRep) %>%
    select(countriesAndTerritories, cases, max_cases, dateRep) %>%
    group_by(countriesAndTerritories) %>%
    filter(cases>=(max_cases * pc) / 100) %>%
    filter(row_number()==1) %>%
    rename(firstCase=dateRep) %>%
    select(countriesAndTerritories, firstCase) %>%
    ungroup() %>%
    distinct()
}

get_first_smoothed_case_at_pc <- function(name, pc) {
    name %>%
    arrange(countriesAndTerritories, dateRep) %>%
    select(countriesAndTerritories, smoothed_cases, max_smoothed_cases, dateRep) %>%
    group_by(countriesAndTerritories) %>%
    filter(smoothed_cases>=(max_smoothed_cases * pc) / 100) %>%
    filter(row_number()==1) %>%
    rename(firstCase=dateRep) %>%
    select(countriesAndTerritories, firstCase) %>%
    ungroup() %>%
    distinct()
}

# Smooth the cases and deaths data

smooth_cases <- function(name) {
    name %>%
    mutate(smoothed_cases=ifelse(lag(lag(countriesAndTerritories))==countriesAndTerritories &
                                 lag(countriesAndTerritories)==countriesAndTerritories &
                                 lead(countriesAndTerritories)==countriesAndTerritories &
                                 lead(lead(countriesAndTerritories))==countriesAndTerritories,
                                 (lag(lag(cases))+lag(cases)+cases+lead(cases)+lead(lead(cases)))/5, NA))
}

smooth_deaths <- function(name) {
    name %>%
    mutate(smoothed_deaths=ifelse(lag(lag(countriesAndTerritories))==countriesAndTerritories &
                                 lag(countriesAndTerritories)==countriesAndTerritories &
                                 lead(countriesAndTerritories)==countriesAndTerritories &
                                 lead(lead(countriesAndTerritories))==countriesAndTerritories,
                                 (lag(lag(deaths))+lag(deaths)+deaths+lead(deaths)+lead(lead(deaths)))/5, NA))
}

#smooth <- function(name, field) {
#    name %>%
#    mutate(field:=ifelse(lag(lag(countriesAndTerritories))==countriesAndTerritories &
#                                 lag(countriesAndTerritories)==countriesAndTerritories &
#                                 lead(countriesAndTerritories)==countriesAndTerritories &
#                                 lead(lead(countriesAndTerritories))==countriesAndTerritories,
#                                 (lag(lag(sym(field)))+lag(sym(field))+sym(field)+lead(sym(field))+lead(lead(sym(field))))/5, NA))
#}

cumulative_cases <- function(name) {
    name$cum_cases = 0
    for(i in 2:count(name)[[1]]) {
        if(name$countriesAndTerritories[i]==name$countriesAndTerritories[i-1]) {
            name$cum_cases[i] = name$cum_cases[i-1] + name$cases[i]
        } else {
            name$cum_cases[i] = 0
        }
    }
    return(name)
}

get_max_cumulative_cases <- function(name) {
    name %>%
    select(countriesAndTerritories, cum_cases) %>%
    group_by(countriesAndTerritories) %>%
    mutate(max_cum_cases=max(cum_cases)) %>%
    select(-cum_cases) %>%
    ungroup() %>%
    distinct()
}

cumulative_deaths <- function(name) {
    name$cum_deaths = 0
    for(i in 2:count(name)[[1]]) {
        if(name$countriesAndTerritories[i]==name$countriesAndTerritories[i-1]) {
            name$cum_deaths[i] = name$cum_deaths[i-1] + name$deaths[i]
        } else {
            name$cum_deaths[i] = 0
        }
    }
    return(name)
}

