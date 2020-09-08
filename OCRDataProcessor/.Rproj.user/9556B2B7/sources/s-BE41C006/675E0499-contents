#' Clean Notes
#'
#' @inheritParams prc_last_name
#'
#' @return dataframe with clean notes
#' @export
clean_notes <- function(df) {
  df %>% 
    mutate(NOTES = removePunctuation(NOTES), 
           NOTES = str_squish(NOTES),
           NOTES = str_to_title(NOTES),
           NOTES = str_replace_all(NOTES, '“', ''))
}