#' Process Last Name
#'
#' @param df OCR dataframe
#'
#' @return dataframe with processed last name
#' @export
prc_last_name <- function(df) {
  df %>% 
    mutate(SPLIT = str_split(LASTNAME, ',|\\.'),
           LASTNAME = map(SPLIT, 1),
           SPLIT = sapply(SPLIT, get_all_but_first)) %>% 
    unnest(LASTNAME)
}

#' Process Last Name
#'
#' @inheritParams prc_last_name
#'
#' @return dataframe with processed first name
#' @export
prc_first_name <- function(df,
                           regiment_regex = prep_regiment_regex()) {
  df %>% 
    mutate(REGIMENT_SUFFIX = str_extract(SPLIT, regiment_regex),
           SPLIT2 = str_split(SPLIT, regiment_regex),
           FIRST_NAME_AND_REG_PREFIX = map(SPLIT2, 1),
           SPLIT3 = str_split(FIRST_NAME_AND_REG_PREFIX, '[:digit:]'),
           ADD_FIRST_NAME = map(SPLIT3, 1),
           FIRSTNAME = ifelse(is.na(FIRSTNAME), ADD_FIRST_NAME, FIRSTNAME)) %>% 
    select(-ADD_FIRST_NAME) %>% 
    unnest(FIRSTNAME)
}

#' Process Regiment
#'
#' @inheritParams prc_last_name
#'
#' @return dataframe with processed regiment
#' @export
prc_regiment <- function(df) {
  df %>% 
    unnest(FIRST_NAME_AND_REG_PREFIX) %>% 
    mutate(REGIMENT_PREFIX_NUM = parse_number(FIRST_NAME_AND_REG_PREFIX),
           REGIMENT_PREFIX_TH = if_else(!is.na(REGIMENT_PREFIX_NUM), toOrdinal(REGIMENT_PREFIX_NUM), '')) %>% 
    select(-REGIMENT_PREFIX_NUM) %>% 
    mutate(ADD_REGIMENT = ifelse(!is.na(REGIMENT_PREFIX_TH) & !is.na(REGIMENT_SUFFIX),
                                 paste0(REGIMENT_PREFIX_TH, ' ', REGIMENT_SUFFIX),
                                 NA)) %>% 
    mutate(ADD_REGIMENT = str_squish(ADD_REGIMENT)) %>% 
    select(-REGIMENT_PREFIX_TH, -REGIMENT_SUFFIX, -FIRST_NAME_AND_REG_PREFIX, -SPLIT3) %>% 
    mutate(REGIMENT = if_else(is.na(REGIMENT), ADD_REGIMENT, REGIMENT)) %>% 
    select(-ADD_REGIMENT) %>% 
    mutate(SPLIT = sapply(SPLIT2, get_all_but_first)) %>% 
    select(-SPLIT2) %>% 
    mutate(SPLIT = ifelse(SPLIT %in% c(Inf, ''), NA, SPLIT))
}

#' Process Birthplace
#'
#' @inheritParams prc_last_name
#'
#' @return dataframe with processed birthplace
#' @export
prc_birthplace <- function(df) {
  df %>% 
    mutate(ADD_BIRTHPLACE = str_extract(SPLIT, prep_birthplace())) %>% 
    mutate(BIRTHPLACE = if_else(is.na(BIRTHPLACE), ADD_BIRTHPLACE, BIRTHPLACE)) %>% 
    mutate(BIRTHPLACE = str_squish(BIRTHPLACE)) %>% 
    mutate(BIRTHPLACE = case_when(
      BIRTHPLACE == '©' ~ 'C',
      BIRTHPLACE == 'e' ~ 'E',
      BIRTHPLACE == 'c' ~ 'C',
      BIRTHPLACE == 'NB-St John' ~ 'NB-St Johns',
      BIRTHPLACE == 'CW -Prescott' ~ 'CW-Prescott',
      BIRTHPLACE == "CW'-Buxton" ~ 'CW-Buxton',
      BIRTHPLACE == 'CW -Hamilton' ~ 'CW-Hamilton',
      BIRTHPLACE == 'CW -Toronto' ~ 'CW-Toronto',
      BIRTHPLACE == '® €' ~ 'RC',
      BIRTHPLACE == 'Inf C' ~ 'C',
      BIRTHPLACE == '°C' ~ 'C',
      BIRTHPLACE == '€' ~ 'C',
      BIRTHPLACE == '¢' ~ 'C',
      BIRTHPLACE == '° CW' ~ 'CW',
      BIRTHPLACE == 'G' ~ 'G',
      BIRTHPLACE == 'ec' ~ 'EC',
      BIRTHPLACE == 'Inf NBGrafton' ~ 'NB-Grafton',
      BIRTHPLACE == 'S' ~ 'S',
      BIRTHPLACE == '—CESt Johns' ~ 'CE-St Johns',
      BIRTHPLACE == 'CE-Monteal' ~ 'CE-Montreal',
      BIRTHPLACE == 'r €' ~ 'RC',
      BIRTHPLACE == 'Gc' ~ 'GC',
      BIRTHPLACE == 'ie' ~ 'IE',
      TRUE ~ BIRTHPLACE
    )) %>% 
    select(-ADD_BIRTHPLACE)
}

#' Process Occupation
#'
#' @inheritParams prc_last_name
#'
#' @return dataframe with processed occupation
#' @export
prc_occupation <- function(df) {
  df %>% 
    mutate(ADD_OCCUPATION = str_extract(SPLIT, prep_occupation())) %>% 
    mutate(OCCUPATION = if_else(is.na(OCCUPATION), ADD_OCCUPATION, OCCUPATION)) %>% 
    mutate(OCCUPATION = str_squish(OCCUPATION)) %>% 
    mutate(OCCUPATION = case_when(
      OCCUPATION == "Jaborer" ~ "laborer", 
      OCCUPATION == "stewd" ~ "steward", 
      OCCUPATION == "1tobacconist" ~ "tobacconist", 
      OCCUPATION == "jJaborer" ~ "laborer", 
      TRUE ~ OCCUPATION
    )) %>% 
    select(-ADD_OCCUPATION)
}

#' Process Date of Death
#'
#' @inheritParams prc_last_name
#'
#' @return dataframe with processed death date
#' @export
prc_date_of_death <- function(df) {
  df %>% 
    dplyr::rename(DATE_OF_DEATH = `DATE OF DEATH`) %>% 
    mutate(SPLIT_DEATH = str_split(SPLIT, 'DIED|Died'),
           ADD_DATE_OF_DEATH = sapply(SPLIT_DEATH, get_all_but_first)) %>% 
    mutate(ADD_DATE_OF_DEATH = str_squish(ADD_DATE_OF_DEATH)) %>% 
    mutate(ADD_DATE_OF_DEATH = str_replace(ADD_DATE_OF_DEATH, ' e', '')) %>% 
    mutate(ADD_DATE_OF_DEATH_2 = ifelse(str_detect(NOTES, 'Died'), NOTES, NA)) %>% 
    mutate(ADD_DATE_OF_DEATH_2 = str_replace_all(ADD_DATE_OF_DEATH_2, 'Died', '')) %>% 
    mutate(ADD_DATE_OF_DEATH_2 = str_replace_all(ADD_DATE_OF_DEATH_2, 'Ist Set', '')) %>% 
    mutate(ADD_DATE_OF_DEATH_2 = str_squish(ADD_DATE_OF_DEATH_2)) %>% 
    mutate(DATE_OF_DEATH = if_else(is.na(DATE_OF_DEATH), ADD_DATE_OF_DEATH, DATE_OF_DEATH)) %>% 
    mutate(DATE_OF_DEATH = if_else(is.na(DATE_OF_DEATH), ADD_DATE_OF_DEATH_2, DATE_OF_DEATH)) %>% 
    select(-ADD_DATE_OF_DEATH, -ADD_DATE_OF_DEATH_2, -SPLIT_DEATH)
}


