#' Get OCR Data
#'
#' @return preprocessed OCR data
#' @export
get_ocr_data <- function() {

  cols_to_select <- c('LASTNAME',
                      'FIRSTNAME',
                      'REGIMENT',
                      'BIRTHPLACE',
                      'DATE OF ENLISTMENT',
                      'PLACE OF ENLISTMENT',
                      'AGE',
                      'OCCUPATION',
                      'NOTES',
                      'CAUSE OF DEATH',
                      'DATE OF DEATH',
                      'PLACE OF DEATH',
                      'PLACE OF BURIAL',
                      'SOURCE',
                      'ORDER NUMBER')
  
  prep_ocr_data() %>% 
    select(all_of(cols_to_select)) %>% 
    filter(!is.na(LASTNAME) & LASTNAME != '' & LASTNAME != '\f')
}

#' Prepare OCR Data
#' 
#' This function prepares data from OCR.
#'
#' @param package package name
#'
#' @return OCR dataframe
#' @export
prep_ocr_data <- function(package = 'OCRDataProcessor') {
  file <- system.file('ocr_data/Rough version of all Tom Brooks.csv', package = package)
  read_csv(file)
}

#' Prepare Regiment Regex
#'
#' @return regiment regex
#' @export
prep_regiment_regex <- function() {
  c('Mass', 'Mass Inf', 'Mass Cav',
    'Col Inf',
    'USCT', 'USCC', 'USCHA',
    'U.S. Navy', 'U S  Navy', 'U S Navy', 'US Navy', 'U S Nayy', 'US Navy', 'U S Navy',
    'Contract Surg',
    'U S C T', 'Uscr',
    'AMHUSCY', 'TAA USE', 'ASHISCT',
    'SS USC', 'USCY', 'USET',
    'AXGUSCE', 'UScT', 'Use',
    'AistUser', 'Im vsc', 'ast eer', 
    'Dotnet CoC', 'SistWacr', 'Srsrvoc', 
    'Zou Se', 'Sth UsCT', 'shih WSGL', 
    'USEF', 'Asa UCT', 'USC', 
    'USCE', 'WHLUSGCE', '2sr¢ USEr', 
    ' SCT', 'GSCT', 'tise «', 
    '70 ec', 'SCT', 'BIstUSer', 
    'USCIT', 'DSirusel', 'User') %>% 
    paste(collapse = '|')
}

#' Prepare Birthplace Regex
#'
#' @return birthplace regex
#' @export
prep_birthplace <- function() {
  c("—CESt Johns", "° CW", "°C ", "® €",
    "Boston", "C€", "Cav", "Cav CW Chatham", "Cav CWChatham",
    "Cav CWHamilton", "Cav CWHampton", "Cav NBSt John", "Cav NS", "Cav NSHalifax",
    "CEMontreal", "CEMontreal", "CEMontreal", "CEMontreal",
    "CWBuxton", "CWChatham", "CWEssex Co", "CWFinigal", "CWGosfield", "CWHarwich",
    "CWKingston", "CWLondon", "CWMalden", "CWToronto", "CWWindsor", "E CC ",           
    "Inf C", "Inf CWChatham", "Inf CWWoodstock", "Inf NBGrafton", "IT CC",           
    "NBHillsborough", "NBNassau", "NBSt Andrews", "NBSt John", "NBSt Johns", "Nfld",          
    "NSCape", "NSHalifax", "r €", "Cc", "CC", "ce", "CL", "CW", "ec ", "Gc", "ie", "iS", "NB", "NS",
    "S ", "v", "© ", "¢", "€", "c", "C", "C ", "e ", "G ", "G ") %>% 
    paste(collapse = '|')
}

#' Prepare Occupation Regex
#'
#' @return occupation regex
#' @export
prep_occupation <- function() {
  c('farmer', 'cook', 'laborer', 'Jaborer', 'sailor', 'farmer', 'barber', 
    'mariner', 'steward', 'painter', 'waiter', 'cooper', '1tobacconist', 
    'jJaborer', 'seaman', 'mechanic', 'shoemaker', 'groom', 'porter', 
    'stewd', 'cook’steward', 'carpenter', 'boatman', 'lumberman', 'blacksmith') %>% 
    paste(collapse = '|')
}
