#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja przechowująca nazwę szkoły.
#' @param x ramka danych pośrednich P4
#' @return tekst
dane_szkoly_ad1 = function(x) {
  stopifnot(is.data.frame(x))

  list(
    nazwa = unique(x$NAZWA_SZK),
    adres = unique(x$ADRES_SZK)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja oblicza wskaźnik opisujący liczbę absolwentów na danym
#' poziomie agregacji, którzy zostali objęci monitoringiem.
#' @param x ramka danych pośrednich P4
#' @return liczba
#' @importFrom dplyr n_distinct
l_abs_ad1 = function(x) {
  return(n_distinct(x))
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja oblicza wskaźnik opisujący liczbę kobiet wśród
#' absolwentów na danym poziomie agregacji, którzy zostali objęci monitoringiem.
#' @param x ramka danych pośrednich P4
#' @return liczba
#' @importFrom dplyr %>% filter .data n_distinct
l_kobiet_ad1 = function(x) {
  x %>%
    filter(.data$PLEC %in% "K") %>%
    n_distinct() %>%
    return()
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja oblicza wskaźnik opisujący liczbę absolwentów na danym
#' poziomie agregacji, o których pozyskano informacje z poszczególnych
#' rejestrów.
#' @param x ramka danych pośrednich P4
#' @return lista
#' @importFrom dplyr .data
l_abs_zrodla_ad1 = function(x) {
  x %>%
    summarise(
      n_cie = sum(.data$ABS_W_SIO, na.rm = TRUE),
      n_opi = sum(.data$ABS_W_POLON, na.rm = TRUE),
      n_oke = sum(.data$ABS_W_CKE, na.rm = TRUE),
      n_zus = sum(.data$ABS_W_ZUS, na.rm = TRUE)
    ) %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja tworząca i przechowująca listę wyrażeń tekstowych
#' używanych do podstawiania w szablonach raportów:
#' \itemize{
#'  \item{Nazwa województwa w dopełniaczu przechowywana jako tekst}
#' }
#' @param x ramka danych pośrednich P4
#' @return lista
#' @importFrom dplyr %>% select mutate .data distinct
formy_ad1 = function(x) {
  x %>%
    mutate(
      woj_nazwa = ifelse(length(unique(.data$WOJ_NAZWA)) %in% 1, unique(.data$WOJ_NAZWA), "NA"),
      woj_dop = ifelse(length(unique(.data$WOJ_NAZWA)) %in% 1, paste0(.data$WOJ_NAZWA, "go"), "NA")
    ) %>%
    select(woj_nazwa, woj_dop) %>%
    distinct() %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów o danym statusie
#' edukacyjno-zawodowym z uwzględnieniem bezrobocia (wskaźnik S2) w danym
#' miesiącu \code{mies} dla raportu dla danego roku \code{raport}. Wyróżniamy
#' następujące statusy:
#' \itemize{
#'  \item{stat1 - Uczy się oraz nie pracuje na podstawie umowy o pracę i nie
#'  jest bezrobotny (dla uczących się niestacjonarnie)}
#'  \item{stat2 - Uczy się oraz poza rejestrem ZUS}
#'  \item{stat3 - Uczy się (niestacjonarnie) oraz bezrobotny nie na stażu}
#'  \item{stat4 - Uczy się (niestacjonarnie) oraz bezrobotny na stażu}
#'  \item{stat5 - Uczy się oraz pracuje na podstawie umowy o pracę}
#'  \item{stat6 - Nie uczy się oraz pracuje}
#'  \item{stat7 - Nie uczy się oraz bezrobotny na stażu}
#'  \item{stat8 - Nie uczy się oraz bezrobotny nie na stażu}
#'  \item{stat9 - Nie uczy się oraz nie pracuje i nie jest bezrobotny}
#'  \item{stat10 - Nie uczy się oraz poza rejestrem ZUS}
#' }
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @return lista
#' @importFrom dplyr %>% filter .data summarise n_distinct
S2_mies_ad1 = function(x, raport, mies) {
  stopifnot(is.data.frame(x),
            raport %in% c(0, 1),
            mies %in% c(1:12))

  mies = 2020 * 12 + mies

  x %>%
    filter(.data$OKRES %in% mies) %>%
    summarise(
      n = n_distinct(.data$ID_ABS),
      stat7 = sum(.data$NAUKA %in% 0 & .data$BEZROBOCIE_STAZ %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      stat8 = sum(.data$NAUKA %in% 0 & .data$BEZROBOCIE_STAZ %in% 0, na.rm = TRUE) / n_distinct(.data$ID_ABS)
    ) %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów o danym statusie
#' edukacyjno-zawodowym (wskaźnik S3) w danym miesiącu \code{mies} dla raportu
#' dla danego roku \code{raport}. Wyróżniamy następujące statusy (bez KUZ i
#' KKZ):
#' \itemize{
#'  \item{stat1 - Uczy się i pracuje na podstawie umowy o pracę lub jest
#'  samozatrudniony}
#'  \item{stat2 - Uczy się i nie pracuje na podstawie umowy o pracę ani nie jest
#'  samozatrudniony}
#'  \item{stat3 - Nie uczy się i pracuje}
#'  \item{stat4 - Nie uczy się i nie pracuje}
#'  \item{stat5 - Uczy się i poza rejestrem ZUS}
#'  \item{stat6 - Nie uczy się i poza rejestrem ZUS}
#' }
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @return lista
#' @importFrom dplyr %>% filter .data summarise n_distinct
S3_mies_ad1 = function(x, raport, mies) {
  stopifnot(is.data.frame(x),
            raport %in% c(0, 1),
            mies %in% c(1:12))

  mies = 2020 * 12 + mies

  x %>%
    filter(.data$OKRES %in% mies) %>%
    summarise(
      n = n_distinct(.data$ID_ABS),
      stat1 = sum((.data$NAUKA2 %in% 1 | .data$NAUKA_SZK_ABS %in% 1) & .data$PRACA > 0, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      stat2 = sum((.data$NAUKA2 %in% 1 | .data$NAUKA_SZK_ABS %in% 1) & .data$PRACA %in% 0, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      stat3 = sum(.data$NAUKA2 %in% 0 & .data$PRACA > 0 & .data$NAUKA_SZK_ABS %in% 0, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      stat4 = sum(.data$NAUKA2 %in% 0 & .data$PRACA %in% 0 & .data$NAUKA_SZK_ABS %in% 0, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      stat5 = sum((.data$NAUKA2 %in% 1 | .data$NAUKA_SZK_ABS %in% 1) & .data$STATUS_NIEUSTALONY %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      stat6 = sum(.data$NAUKA2 %in% 0 & .data$STATUS_NIEUSTALONY %in% 1 & .data$NAUKA_SZK_ABS %in% 0, na.rm = TRUE) / n_distinct(.data$ID_ABS)
    ) %>%
    as.list() %>%
    return()
}
#' #' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' #' @description Funkcja licząca odsetek absolwentów o danym statusie
#' #' edukacyjno-zawodowym (wskaźnik S3) w danym miesiącu \code{mies} dla raportu
#' #' dla danego roku \code{raport}. Wyróżniamy następujące statusy (bez KUZ i
#' #' KKZ):
#' #' \itemize{
#' #'  \item{stat1 - Uczy się i pracuje na podstawie umowy o pracę lub jest
#' #'  samozatrudniony}
#' #'  \item{stat2 - Uczy się i nie pracuje na podstawie umowy o pracę ani nie jest
#' #'  samozatrudniony}
#' #'  \item{stat3 - Nie uczy się i pracuje}
#' #'  \item{stat4 - Nie uczy się i nie pracuje}
#' #'  \item{stat5 - Uczy się i poza rejestrem ZUS}
#' #'  \item{stat6 - Nie uczy się i poza rejestrem ZUS}
#' #' }
#' #' Funkcja powinna być wykorzystana dla miesięcy od stycznia do października.
#' #' @param x ramka danych pośrednich P3
#' #' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' #' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' #' @return lista
#' #' @importFrom dplyr %>% filter .data summarise n_distinct
#' S3_mies_sty_paz_ad1 = function(x, raport, mies) {
#'   stopifnot(is.data.frame(x),
#'             raport %in% c(0, 1),
#'             mies %in% c(1:12))
#'
#'   mies = 2020 * 12 + mies
#'
#'   x %>%
#'     filter(.data$OKRES %in% mies) %>%
#'     summarise(
#'       n = n_distinct(.data$ID_ABS),
#'       stat1 = sum(.data$NAUKA_SZK_ABS %in% 1 & .data$PRACA > 0, na.rm = TRUE) / n_distinct(.data$ID_ABS),
#'       stat2 = sum(.data$NAUKA_SZK_ABS %in% 1 & .data$PRACA %in% 0, na.rm = TRUE) / n_distinct(.data$ID_ABS),
#'       stat3 = sum(.data$NAUKA_SZK_ABS %in% 0 & .data$PRACA > 0, na.rm = TRUE) / n_distinct(.data$ID_ABS),
#'       stat4 = sum(.data$NAUKA_SZK_ABS %in% 0 & .data$PRACA %in% 0, na.rm = TRUE) / n_distinct(.data$ID_ABS),
#'       stat5 = sum(.data$NAUKA_SZK_ABS %in% 1 & .data$STATUS_NIEUSTALONY %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
#'       stat6 = sum(.data$NAUKA_SZK_ABS %in% 0 & .data$STATUS_NIEUSTALONY %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS)
#'     ) %>%
#'     as.list() %>%
#'     return()
#' }
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów o danym statusie
#' edukacyjno-zawodowym wśród grup absolwentów wydzielonych ze względu na zawód,
#' w którym się kształcili w szkole. Zwracana lista służy jako wsad do tabeli w
#' raporcie automatycznym. Domyślnie funkcja ma zwracać wsad do tabeli dla
#' grudnia 2020, ale można to zmienić poprzez zmianę argumentów funkcji. Jeśli
#' lista będąca wsadem tabeli generowałaby pustą tabelę, to zwracana jest
#' wartość \code{NULL}. W przeciwnym wypadku zwracana jest lista.
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @return lista
#' @importFrom dplyr %>% group_by .data as_tibble filter mutate select
zawody_S3_ad1 = function(x, raport = 0, mies = 12) {
  stopifnot(is.data.frame(x),
            "NAZWA_ZAW" %in% names(x),
            raport %in% c(0, 1),
            mies %in% c(1:12))

  tab = x %>%
    group_by(.data$NAZWA_ZAW) %>%
    S3_mies_ad1(raport, mies) %>%
    as_tibble() %>%
    arrange(desc(n)) %>%
    filter(n >= 10)

  tot = x %>%
    filter(.data$NAZWA_ZAW %in% unique(tab$NAZWA_ZAW)) %>%
    S3_mies_ad1(raport, mies) %>%
    as_tibble() %>%
    mutate(NAZWA_ZAW = "Ogółem") %>%
    select(NAZWA_ZAW, n:stat6)

  if (nrow(tab) %in% 0) {
    return(list())
  } else {
    rbind(tab, tot) %>%
      as.list() %>%
      return()
  }
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca wskaźnik E2 - Sposoby kontynuowania edukacji.
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @return lista
#' @importFrom dplyr %>% filter .data summarise n_distinct
E2_nauka_kontyn_ad1 = function(x, raport = 0, mies = 12) {
  stopifnot(is.data.frame(x),
            raport %in% c(0, 1),
            mies %in% c(1:12))

  mies = 2020 * 12 + mies

  x %>%
    filter(.data$OKRES %in% mies) %>%
    summarise(
      n = n_distinct(.data$ID_ABS),
      bs2 = sum(.data$NAUKA_BS2ST %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      lodd = sum(.data$NAUKA_LODD %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      spolic = sum(.data$NAUKA_SPOLIC %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      studia = sum(.data$NAUKA_STUDIA %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      kkz = sum(.data$NAUKA_KKZ %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      kuz = sum(.data$NAUKA_KUZ %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
      brak = sum(.data$NAUKA_BS2ST %in% 0 &
                   .data$NAUKA_LODD %in% 0 &
                   .data$NAUKA_SPOLIC %in% 0 &
                   .data$NAUKA_STUDIA %in% 0 &
                   .data$NAUKA_KKZ %in% 0 &
                   .data$NAUKA_KUZ %in% 0, na.rm = TRUE) / n_distinct(.data$ID_ABS)) %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności wyuczonych zawodów wśród
#' absolwentów w podziale na branże. Dodatkowo, funkcja liczy liczebności
#' absolwentów w branżach, a informacja ta służy jako podstawa do definiowania
#' warunków w szablonie raportu dla szkół branżowych 1. stopnia.
#' @param x ramka danych pośrednich P4
#' @return lista
#' @importFrom dplyr %>% filter .data count mutate slice_max
liczebnosc_branze_ucz_bs1_ad1 = function(x) {
  stopifnot(is.data.frame(x))

  x = x %>%
    filter(!(is.na(.data$BRANZA)))

  if (nrow(x) %in% 0) {
    return(list())
  } else {
    n_dist = n_distinct(x$ID_ABS)

    tab = x %>%
      count(.data$BRANZA) %>%
      mutate(odsetek = .data$n / n_dist) %>%
      slice_max(n = 10, order_by = .data$n) %>%
      filter(n >= 10)
    if (nrow(tab) %in% 0) {
      return(list())
    } else {
      tab %>%
        as.list() %>%
        return()
    }
  }
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności absolwentów kontynuujących
#' naukę w szkołach branżowych 2. stopnia w podziale na branże. Dodatkowo,
#' funkcja liczy liczebności absolwentów w branżach, a informacja ta służy jako
#' podstawa do definiowania warunków w szablonie raportu dla szkół branżowych 1.
#' stopnia. Funkcja ma sens tylko dla absolwentów szkół branżowych 1. stopnia,
#' ponieważ tylko oni mogą kontynuować naukę w szkołach branżowych 2. stopnia.
#' @param x ramka danych pośrednich P2
#' @param kont_bs2_df ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w szkołach branżowych 2. stopnia (tabela danych pośrednich P3 lub
#' zawierająca analogiczne informacje oraz te same nazwy kolumn co tabela P3)
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @return lista
#' @importFrom dplyr %>% filter .data group_by count ungroup mutate select
#' left_join
# liczebnosc_branze_bs1_ad1 = function(x, kont_bs2_df, raport = 0, mies = 12) {
#   stopifnot(is.data.frame(x),
#             is.data.frame(kont_bs2_df),
#             raport %in% c(0, 1),
#             mies %in% c(1:12))
#
#   mies = 2020 * 12 + mies
#
#   kont_bs2_df = kont_bs2_df %>%
#     filter(.data$OKRES %in% mies) %>%
#     select(ID_ABS, ROK_ABS, NAUKA_BS2ST)
#
#   x = x %>%
#     filter(.data$OKRES_KONT %in% mies) %>%
#     left_join(kont_bs2_df,
#               by = c("ID_ABS", "ROK_ABS")) %>%
#     filter(!(is.na(.data$BRANZA_KONT)),
#            .data$NAUKA_BS2ST %in% 1)
#
#   n_dist = n_distinct(x$ID_ABS)
#
#   tab = x %>%
#     group_by(.data$BRANZA_KONT) %>%
#     count() %>%
#     ungroup() %>%
#     mutate(odsetek = .data$n / n_dist)
#
#   if (nrow(tab) %in% 0) {
#     return(list())
#   } else {
#     tab %>%
#       as.list() %>%
#       return()
#   }
# }
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności absolwentów kontynuujących
#' naukę w szkołach branżowych 2. stopnia w podziale na branże. Dodatkowo,
#' funkcja liczy liczebności absolwentów w branżach, a informacja ta służy jako
#' podstawa do definiowania warunków w szablonie raportu dla szkół branżowych 1.
#' stopnia. Funkcja ma sens tylko dla absolwentów szkół branżowych 1. stopnia,
#' ponieważ tylko oni mogą kontynuować naukę w szkołach branżowych 2. stopnia.
#' @param x ramka danych pośrednich P3
#' @param branza_kont_df ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej branży (tabela danych pośrednich P2 lub zawierająca
#' analogiczne informacje oraz te same nazwy kolumn co tabela P2)
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie
#' grudzień
#' @return lista
#' @importFrom dplyr %>% filter .data count mutate select left_join n_distinct
#' slice_max
liczebnosc_branze_bs1_ad1 = function(x, branza_kont_df, raport = 0, mies = 12) {
  stopifnot(is.data.frame(x),
            is.data.frame(branza_kont_df),
            raport %in% c(0, 1),
            mies %in% c(1:12))

  # mies_kont = 2020 * 12
  mies = 2020 * 12 + mies

  branza_kont_df = branza_kont_df %>%
    # filter(.data$OKRES_KONT %in% mies_kont) %>%
    select(ID_ABS, ROK_ABS, BRANZA_KONT)

  x = x %>%
    filter(.data$OKRES %in% mies) %>%
    left_join(branza_kont_df,
              by = c("ID_ABS", "ROK_ABS")) %>%
    filter(.data$NAUKA_BS2ST %in% 1) %>%
    filter(!(is.na(.data$BRANZA_KONT)))

  if (nrow(x) %in% 0) {
    return(list())
  } else {
    n_dist = n_distinct(x$ID_ABS)

    tab = x %>%
      count(.data$BRANZA_KONT) %>%
      mutate(odsetek = .data$n / n_dist) %>%
      slice_max(n = 10, order_by = .data$n) %>%
      filter(n >= 10)
    if (nrow(tab) %in% 0) {
      return(list())
    } else {
      tab %>%
        as.list() %>%
        return()
    }
  }
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca na potrzeby szablonu raportu odsetek pracujących
#' w danym miesiącu w podziale na pobierających i nie pobierających nauki. W
#' raporcie w 2021 roku będzie to "Odsetek miesięcy, w których absolwenci
#' pracowali od \emph{września 2020} do \emph{grudnia 2020} roku.
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param od początek okresu wyliczania wskaźnika wyrażony miesiącem
#' @param do koniec okresu wyliczania wskaźnika wyrażony miesiącem
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się czy nie uczących się
#' @return lista
#' @importFrom dplyr %>% .data filter count full_join mutate n_distinct between
Z4_ods_prac_mies_ad1 = function(x, raport = 0, od = 9, do = 12, nauka) {
  stopifnot(is.data.frame(x),
            raport %in% c(0, 1),
            od %in% c(1:12),
            do %in% c(1:12),
            is.logical(nauka))

  l_od = min(x$ROK) * 12 + od
  l_do = max(x$ROK) * 12 + do

  x = x %>%
    filter(.data$OKRES %in% seq(l_od, l_do, by = 1))

  if (nauka) {
    ucz = x %>%
      filter(.data$NAUKA2 %in% 1 | .data$NAUKA_SZK_ABS %in% 1) %>%
      count(.data$ID_ABS, name = "l_mies_ucz")

    ucz_prac = x %>%
      filter((.data$NAUKA2 %in% 1 | .data$NAUKA_SZK_ABS %in% 1) & .data$PRACA %in% c(1, 2, 4:7)) %>%
      count(.data$ID_ABS, name = "l_mies_ucz_prac")

    ucz %>%
      full_join(ucz_prac, by = "ID_ABS") %>%
      mutate(ods_ucz_prac = ifelse(is.na(.data$l_mies_ucz_prac), 0, .data$l_mies_ucz_prac / .data$l_mies_ucz)) %>%
      summarise(
        n = n_distinct(.data$ID_ABS),
        srednia = mean(.data$ods_ucz_prac, na.rm = TRUE),
        med = median(.data$ods_ucz_prac, na.rm = TRUE),
        p0 = sum(.data$ods_ucz_prac %in% 0) / n_distinct(.data$ID_ABS),
        p25 = sum(.data$ods_ucz_prac %in% 0.25) / n_distinct(.data$ID_ABS),
        p33 = sum(round(.data$ods_ucz_prac, 2) %in% 0.33) / n_distinct(.data$ID_ABS),
        p50 = sum(.data$ods_ucz_prac %in% 0.5) / n_distinct(.data$ID_ABS),
        p67 = sum(round(.data$ods_ucz_prac, 2) %in% 0.67) / n_distinct(.data$ID_ABS),
        p75 = sum(.data$ods_ucz_prac %in% 0.75) / n_distinct(.data$ID_ABS),
        p100 = sum(.data$ods_ucz_prac %in% 1) / n_distinct(.data$ID_ABS)) %>%
      as.list() %>%
      return()
  } else {
    ucz = x %>%
      filter(.data$NAUKA2 %in% 0) %>%
      count(.data$ID_ABS, name = "l_mies_nucz")

    ucz_prac = x %>%
      filter(.data$NAUKA2 %in% 0 & .data$PRACA != 0) %>%
      count(.data$ID_ABS, name = "l_mies_nucz_prac")

    ucz %>%
      full_join(ucz_prac, by = "ID_ABS") %>%
      mutate(ods_nucz_prac = ifelse(is.na(.data$l_mies_nucz_prac), 0, round(.data$l_mies_nucz_prac / .data$l_mies_nucz, 2))) %>%
      summarise(
        n = n_distinct(.data$ID_ABS),
        srednia = mean(.data$ods_nucz_prac, na.rm = TRUE),
        med = median(.data$ods_nucz_prac, na.rm = TRUE),
        p0 = sum(.data$ods_nucz_prac %in% 0) / n_distinct(.data$ID_ABS),
        p25 = sum(.data$ods_nucz_prac %in% 0.25) / n_distinct(.data$ID_ABS),
        p33 = sum(round(.data$ods_nucz_prac, 2) %in% 0.33) / n_distinct(.data$ID_ABS),
        p50 = sum(.data$ods_nucz_prac %in% 0.5) / n_distinct(.data$ID_ABS),
        p67 = sum(round(.data$ods_nucz_prac, 2) %in% 0.67) / n_distinct(.data$ID_ABS),
        p75 = sum(.data$ods_nucz_prac %in% 0.75) / n_distinct(.data$ID_ABS),
        p100 = sum(.data$ods_nucz_prac %in% 1) / n_distinct(.data$ID_ABS)) %>%
      as.list() %>%
      return()
  }
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów wykonujących dane formy
#' pracy w danym miesiącu (w raportach jest to grudzień). Wskaźnik może być
#' liczony albo dla absolwentów pracujących i kontunuujących naukę (\code{nauka
#' = TRUE}) lub dla absolwentów pracujących i nie kontunuujących nauki
#' (\code{nauka = FALSE}). Formy pracy:
#' \itemize{
#'  \item{\code{ucz_uop_niesam}  - Uczący się, zatrudnieni na podstawie umowy o
#'  pracę, brak samozatrudnienia},
#'  \item{\code{ucz_nieuop_sam}  - Uczący się, prowadzący działalność
#'  gospodarczą (samozatrudnienie), brak umowy o pracę},
#'  \item{\code{ucz_uop_sam}  - Uczący się, zatrudnieni na podstawie umowy o
#'  pracę i prowadzący działalność gospodarczą},
#'  \item{\code{nieucz_uop_nieinne}  - Nieuczący się, zatrudnieni na podstawie
#'  umowy o pracę, brak innej formy},
#'  \item{\code{nieucz_sam_nieinne}  - Nieuczący się, prowadzący działalność
#'  gospodarczą (samozatrudnienie), brak innej formy},
#'  \item{\code{nieucz_inne}  - Nieuczący się, zatrudnieni w innej formie (umowa
#'  zlecenie)},
#'  \item{\code{nieucz_uop_sam}  - Nieuczący się, zatrudnieni na podstawie umowy
#'  o pracę i prowadzący działalność gospodarczą},
#'  \item{\code{nieucz_uop_inne} - Nieuczący się, zatrudnieni na podstawie umowy
#'  o pracę i zatrudnieni w innej formie},
#'  \item{\code{nieucz_sam_inne} - Nieuczący się, działalność gospodarcza i
#'  zatrudnienie w innej formie},
#'  \item{\code{nieucz_uop_sam_inne}  - Nieuczący się, zatrudnienie na podstawie
#'  umowy o pracę i działalność gospodarcza i zatrudnienie w innej formie},
#' }
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się czy nie uczących się
#' @return lista
#' @importFrom dplyr %>% filter .data summarise n_distinct
Z8_formy_prac_mies_ad1 = function(x, raport = 0, mies = 12, nauka) {
  stopifnot(is.data.frame(x),
            raport %in% c(0, 1),
            mies %in% c(1:12),
            is.logical(nauka))

  mies = 2020 * 12 + mies

  x = x %>%
    filter(.data$OKRES %in% mies)

  if (nauka) {
    x %>%
      filter((.data$NAUKA2 %in% 1 | .data$NAUKA_SZK_ABS %in% 1) & .data$PRACA %in% c(1, 2, 4)) %>%
      summarise(
        n = n_distinct(.data$ID_ABS),
        ucz_uop_niesam = sum((.data$NAUKA2 %in% 1 | .data$NAUKA_SZK_ABS %in% 1) & .data$PRACA %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
        ucz_nieuop_sam = sum((.data$NAUKA2 %in% 1 | .data$NAUKA_SZK_ABS %in% 1) & .data$PRACA %in% 2, na.rm = TRUE) / n_distinct(.data$ID_ABS),
        ucz_uop_sam = sum((.data$NAUKA2 %in% 1 | .data$NAUKA_SZK_ABS %in% 1) & .data$PRACA %in% 4, na.rm = TRUE) / n_distinct(.data$ID_ABS)) %>%
      as.list() %>%
      return()
  } else {
    x %>%
      filter(.data$NAUKA2 %in% 0 & .data$PRACA != 0) %>%
      summarise(
        n = n_distinct(.data$ID_ABS),
        nieucz_uop_nieinne = sum(.data$NAUKA2 %in% 0 & .data$PRACA %in% 1, na.rm = TRUE) / n_distinct(.data$ID_ABS),
        nieucz_sam_nieinne = sum(.data$NAUKA2 %in% 0 & .data$PRACA %in% 2, na.rm = TRUE) / n_distinct(.data$ID_ABS),
        nieucz_inne = sum(.data$NAUKA_SZK_ABS %in% 0 & .data$PRACA %in% 3, na.rm = TRUE) / n_distinct(.data$ID_ABS),
        nieucz_uop_sam = sum(.data$NAUKA2 %in% 0 & .data$PRACA %in% 4, na.rm = TRUE) / n_distinct(.data$ID_ABS),
        nieucz_uop_inne = sum(.data$NAUKA2 %in% 0 & .data$PRACA %in% 5, na.rm = TRUE) / n_distinct(.data$ID_ABS),
        nieucz_sam_inne = sum(.data$NAUKA2 %in% 0 & .data$PRACA %in% 6, na.rm = TRUE) / n_distinct(.data$ID_ABS),
        nieucz_uop_sam_inne = sum(.data$NAUKA2 %in% 0 & .data$PRACA %in% 7, na.rm = TRUE) / n_distinct(.data$ID_ABS)) %>%
      as.list() %>%
      return()
  }
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów, którzy w danym miesiącu (w
#' raporcie jest to wrzesień) kontynuowali zatrudnienie u pracodawcy, który
#' wcześniej (kiedy byli jeszcze uczniami) zatrduniał ich jako pracowników
#' młodocianych. Wyróżnianych jest 5 form kontynuowania pracy u pracodawcy
#' zatrudniającego poprzednio jako młodocianego:
#' \itemize{
#'  \item{\code{nieucz_niekontuop} - Nieuczący się, nie kontynuujący pracy u
#'  danego pracodawcy}
#'  \item{\code{nieucz_kont_uop} - Nieuczący się, kontynuujący pracę u danego
#'  pracodawcy na podstawie umowy o pracę}
#'  \item{\code{nieucz_kont_inne} - Nieuczący się, kontynuujący pracę u danego
#'  pracodawcy w formie innej niż umowa  o pracę}
#'  \item{\code{ucz_niekontuop} - Uczący się, nie kontynuujący pracy u danego
#'  pracodawcy na podstawie umowy o pracę}
#'  \item{\code{ucz_kontuop} - Uczący się, kontynuujący pracę u danego
#'  pracodawcy na podstawie umowy o pracę}
#' }
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @return lista
#' @importFrom dplyr %>% filter summarise n_distinct
Z9_kont_mlod_ad1 = function(x, raport = 0, mies = 9) {
  stopifnot(is.data.frame(x),
            raport %in% c(0, 1),
            mies %in% c(1:12))

  mies = 2020 * 12 + mies

  x %>%
    filter(.data$OKRES %in% mies,
           !is.na(.data$KONT_MLODOC_PRAC)) %>%
    summarise(
      n = n_distinct(.data$ID_ABS),
      nieucz_niekontuop = sum(.data$KONT_MLODOC_PRAC %in% 1) / n_distinct(.data$ID_ABS),
      nieucz_kont_uop = sum(.data$KONT_MLODOC_PRAC %in% 2) / n_distinct(.data$ID_ABS),
      nieucz_kont_inne = sum(.data$KONT_MLODOC_PRAC %in% 3) / n_distinct(.data$ID_ABS),
      ucz_niekontuop = sum(.data$KONT_MLODOC_PRAC %in% 4) / n_distinct(.data$ID_ABS),
      ucz_kontuop = sum(.data$KONT_MLODOC_PRAC %in% 5) / n_distinct(.data$ID_ABS)) %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca średni miesięczny przychód z pracy w danym
#' okresie. W raporcie w 2021 roku będzie okres od \emph{września 2020} do
#' \emph{grudnia 2020} roku. Średnia liczona jest oddzielnie dla uczących i nie
#' uczących się absolwentów.
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param od początek okresu wyliczania wskaźnika wyrażony miesiącem
#' @param do koniec okresu wyliczania wskaźnika wyrażony miesiącem
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się czy nie uczących się
#' @return lista
#' @importFrom dplyr %>% .data filter group_by summarise ungroup n_distinct
W1_sr_doch_ad1 = function(x, raport = 0, od = 9, do = 12, nauka) {
  stopifnot(is.data.frame(x),
            raport %in% c(0, 1),
            od %in% c(1:12),
            do %in% c(1:12),
            is.logical(nauka))

  l_od = min(x$ROK) * 12 + od
  l_do = max(x$ROK) * 12 + do

  x = x %>%
    filter(.data$OKRES %in% seq(l_od, l_do, by = 1))

  if (nauka) {
    x %>%
      filter((.data$NAUKA2 %in% 1 | .data$NAUKA_SZK_ABS %in% 1) & !is.na(.data$WYNAGRODZENIE),
             .data$WYNAGRODZENIE > 0) %>%
      group_by(.data$ID_ABS) %>%
      summarise(
        sred_ind = mean(.data$WYNAGRODZENIE)) %>%
      ungroup() %>%
      summarise(
        n = n_distinct(.data$ID_ABS),
        sred = round(mean(sred_ind), 2),
        q5 = unname(round(quantile(sred_ind, 0.05), 2)),
        q25 = unname(round(quantile(sred_ind, 0.25), 2)),
        med = unname(round(quantile(sred_ind, 0.5), 2)),
        q75 = unname(round(quantile(sred_ind, 0.75), 2)),
        q95 = unname(round(quantile(sred_ind, 0.95), 2))) %>%
      as.list() %>%
      return()
  } else {
    x %>%
      filter(.data$NAUKA2 %in% 0 & !is.na(.data$WYNAGRODZENIE),
             .data$WYNAGRODZENIE > 0) %>%
      group_by(.data$ID_ABS) %>%
      summarise(
        sred_ind = mean(.data$WYNAGRODZENIE)) %>%
      ungroup() %>%
      summarise(
        n = n_distinct(.data$ID_ABS),
        sred = round(mean(sred_ind), 2),
        q5 = unname(round(quantile(sred_ind, 0.05), 2)),
        q25 = unname(round(quantile(sred_ind, 0.25), 2)),
        med = unname(round(quantile(sred_ind, 0.5), 2)),
        q75 = unname(round(quantile(sred_ind, 0.75), 2)),
        q95 = unname(round(quantile(sred_ind, 0.95), 2))) %>%
      as.list() %>%
      return()
  }
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca średni względny miesięczny przychód z pracy
#' \emph{etatowej} w danym okresie w odniesieniu do zarobków w zamieszkiwanym
#' powiecie. W raporcie w 2021 roku będzie okres od \emph{września 2020} do
#' \emph{grudnia 2020} roku. Średnia liczona jest oddzielnie dla uczących i nie
#' uczących się absolwentów.
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param od początek okresu wyliczania wskaźnika wyrażony miesiącem
#' @param do koniec okresu wyliczania wskaźnika wyrażony miesiącem
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się czy nie uczących się
#' @return lista
#' @importFrom dplyr %>% filter group_by summarise ungroup
#' n_distinct
W3_sr_doch_uop_ad1 = function(x, raport = 0, od = 9, do = 12, nauka) {
  stopifnot(is.data.frame(x),
            raport %in% c(0, 1),
            od %in% c(1:12),
            do %in% c(1:12),
            is.logical(nauka))

  l_od = min(x$ROK) * 12 + od
  l_do = max(x$ROK) * 12 + do

  x = x %>%
    filter(.data$OKRES %in% seq(l_od, l_do, by = 1))

  if (nauka) {
    x %>%
      filter((.data$NAUKA_SZK_ABS %in% 0 | .data$NAUKA_SZK_ABS %in% 1) & !is.na(.data$WYNAGRODZENIE) & !is.na(.data$POWIAT_SR_WYNAGRODZENIE),
             .data$WYNAGRODZENIE > 0,
             .data$POWIAT_SR_WYNAGRODZENIE > 0) %>%
      group_by(.data$ID_ABS, .data$OKRES) %>%
      summarise(
        rel_sred_ind_mies = .data$WYNAGRODZENIE / .data$POWIAT_SR_WYNAGRODZENIE
      ) %>%
      ungroup() %>%
      group_by(.data$ID_ABS) %>%
      summarise(
        rel_sred_ind = mean(.data$rel_sred_ind_mies, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      summarise(
        n = n_distinct(.data$ID_ABS),
        sred = round(mean(.data$rel_sred_ind), 2),
        q5 = unname(round(quantile(.data$rel_sred_ind, 0.05), 2)),
        q25 = unname(round(quantile(.data$rel_sred_ind, 0.25), 2)),
        med = unname(round(quantile(.data$rel_sred_ind, 0.5), 2)),
        q75 = unname(round(quantile(.data$rel_sred_ind, 0.75), 2)),
        q95 = unname(round(quantile(.data$rel_sred_ind, 0.95), 2))) %>%
      as.list() %>%
      return()
  } else {
    x %>%
      filter(.data$NAUKA2 %in% 0 & !is.na(.data$WYNAGRODZENIE) & !is.na(.data$POWIAT_SR_WYNAGRODZENIE),
             .data$WYNAGRODZENIE > 0,
             .data$POWIAT_SR_WYNAGRODZENIE > 0) %>%
      group_by(.data$ID_ABS, .data$OKRES) %>%
      summarise(
        rel_sred_ind_mies = .data$WYNAGRODZENIE / .data$POWIAT_SR_WYNAGRODZENIE
      ) %>%
      ungroup() %>%
      group_by(.data$ID_ABS) %>%
      summarise(
        rel_sred_ind = mean(.data$rel_sred_ind_mies, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      summarise(
        n = n_distinct(.data$ID_ABS),
        sred = round(mean(.data$rel_sred_ind), 2),
        q5 = unname(round(quantile(.data$rel_sred_ind, 0.05), 2)),
        q25 = unname(round(quantile(.data$rel_sred_ind, 0.25), 2)),
        med = unname(round(quantile(.data$rel_sred_ind, 0.5), 2)),
        q75 = unname(round(quantile(.data$rel_sred_ind, 0.75), 2)),
        q95 = unname(round(quantile(.data$rel_sred_ind, 0.95), 2))) %>%
      as.list() %>%
      return()
  }
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca na potrzeby szablonu raportu rozkład liczby
#' miesięcy bezrobocia rejestrowanego wśród absolwentów w danym okresie. W
#' raporcie w 2021 roku będzie to "Liczba miesięcy bezrobocia rejestrowanego od
#' \emph{września 2020} do \emph{grudnia 2020} roku.
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param od początek okresu wyliczania wskaźnika wyrażony miesiącem
#' @param do koniec okresu wyliczania wskaźnika wyrażony miesiącem
#' @return lista
#' @importFrom dplyr %>% filter .data group_by summarise ungroup count mutate
#' across
B2_ods_bezrob_ad1 = function(x, raport = 0, od = 9, do = 12) {
  stopifnot(is.data.frame(x),
            raport %in% c(0, 1),
            od %in% c(1:12),
            do %in% c(1:12))

  l_od = 2020 * 12 + od
  l_do = 2020 * 12 + do

  x = x %>%
    filter(.data$OKRES %in% seq(l_od, l_do, by = 1)) %>%
    group_by(.data$ID_ABS, .data$OKRES) %>%
    summarise(
      l_mies_bezrob = sum(.data$BEZROBOCIE %in% 1, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(across(.data$l_mies_bezrob,
                  ~ifelse(. > 1, 1, .))) %>%
    group_by(.data$ID_ABS) %>%
    summarise(
      l_mies_bezrob = sum(.data$l_mies_bezrob %in% 1, na.rm = TRUE)
    ) %>%
    ungroup()

  ods = x %>%
    count(l_mies_bezrob) %>%
    mutate(value = n / sum(n)) %>%
    filter(l_mies_bezrob %in% 0:4)

  if (nrow(ods) != 5) {
    tab_uzup_szk = structure(tibble(
      l_mies_bezrob = setdiff(0:4, ods$l_mies_bezrob),
      n = as.integer(rep(0, 5 - nrow(ods))),
      value = rep(0, 5 - nrow(ods))
    ))
  }

  if (exists("tab_uzup_szk")) {
    ods = rbind(ods, tab_uzup_szk) %>%
      arrange(l_mies_bezrob) %>%
      as.list()
  } else {
    ods = as.list(ods)
  }

  descr = x %>%
    summarise(
      srednia = mean(.data$l_mies_bezrob),
      mediana = median(.data$l_mies_bezrob)
    ) %>%
    as.list()

  c(ods, descr) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca na potrzeby szablonu raportu rozkład liczby
#' miesięcy bierności zawodowej wśród absolwentów w danym okresie. W
#' raporcie w 2021 roku będzie to "Liczba miesięcy bierności edukacyjnej i
#' zawodowej od \emph{września 2020} do \emph{grudnia 2020} roku.
#' @param x ramka danych pośrednich P3
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param od początek okresu wyliczania wskaźnika wyrażony miesiącem
#' @param do koniec okresu wyliczania wskaźnika wyrażony miesiącem
#' @return lista
#' @importFrom dplyr %>% filter .data group_by summarise ungroup count mutate
N2_ods_biernosc_ad1 = function(x, raport = 0, od = 9, do = 12) {
  stopifnot(is.data.frame(x),
            raport %in% c(0, 1),
            od %in% c(1:12),
            do %in% c(1:12))

  l_od = 2020 * 12 + od
  l_do = 2020 * 12 + do

  x = x %>%
    filter(.data$OKRES %in% seq(l_od, l_do, by = 1)) %>%
    group_by(.data$ID_ABS) %>%
    summarise(
      l_mies_bier = sum(.data$NAUKA %in% 0 & .data$BIERNOSC %in% 1, na.rm = TRUE)
    ) %>%
    ungroup()

  ods = x %>%
    count(.data$l_mies_bier) %>%
    mutate(value = n / sum(n)) %>%
    filter(l_mies_bier %in% 0:4)

  if (nrow(ods) != 5) {
    tab_uzup_szk = structure(tibble(
      l_mies_bier = setdiff(0:4, ods$l_mies_bier),
      n = as.integer(rep(0, 5 - nrow(ods))),
      value = rep(0, 5 - nrow(ods))
    ))
  }

  if (exists("tab_uzup_szk")) {
    ods = rbind(ods, tab_uzup_szk) %>%
      arrange(l_mies_bier) %>%
      as.list()
  } else {
    ods = as.list(ods)
  }

  descr = x %>%
    summarise(
      srednia = mean(.data$l_mies_bier),
      mediana = median(.data$l_mies_bier)
    ) %>%
    as.list()

  c(ods, descr) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności w zawodach na potrzeby
#' raportu wojewódzko-branżowego.
#' @param x ramka danych pośrednich P4
#' @return lista
#' @importFrom dplyr %>% filter .data count mutate
licz_zawody_ad1 = function(x) {
  stopifnot(is.data.frame(x))

  x = x %>%
    filter(!(is.na(.data$NAZWA_ZAW)))

  if (nrow(x) %in% 0) {
    return(list())
  } else {
    n_dist = n_distinct(x$ID_ABS)

    tab = x %>%
      count(.data$NAZWA_ZAW) %>%
      mutate(odsetek = .data$n / n_dist)

    tab %>%
      as.list() %>%
      return()
  }
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności absolwentów kontynuujących
#' naukę na studiach w podziale na dziedziny. Funkcja liczy wskaźnik tylko dla
#' absolwentów techników.
#' @param x ramka danych pośrednich P3
#' @param dziedzina_kont_df ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej dziedzinie (tabela danych pośrednich P2 lub zawierająca
#' analogiczne informacje oraz te same nazwy kolumn co tabela P2)
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie
#' grudzień
#' @return lista
#' @importFrom dplyr %>% filter .data count mutate select left_join n_distinct
#' slice_max
liczebnosc_dziedziny_tech_ad1 = function(x, dziedzina_kont_df, raport = 0, mies = 12) {
  stopifnot(is.data.frame(x),
            is.data.frame(dziedzina_kont_df),
            raport %in% c(0, 1),
            mies %in% c(1:12))

  # mies_kont = 2020 * 12
  mies = 2020 * 12 + mies



  dziedzina_kont_df = dziedzina_kont_df %>%
    select(ID_ABS, ROK_ABS, DZIEDZINA_KONT, OKRES_KONT, TYP_SZK) %>%
    filter(.data$TYP_SZK %in% "Technikum") %>% # .data$OKRES_KONT %in% mies_kont,
    select(-c(OKRES_KONT, TYP_SZK))


  x = x %>%
    filter(.data$OKRES %in% mies,
           .data$TYP_SZK %in% "Technikum") %>%
    left_join(dziedzina_kont_df,
              by = c("ID_ABS", "ROK_ABS")) %>%
    filter(.data$NAUKA_STUDIA %in% 1) %>%
    filter(!(is.na(.data$DZIEDZINA_KONT)))

  if (nrow(x) %in% 0) {
    return(list())
  } else {
    n_dist = n_distinct(x$ID_ABS)

    tab = x %>%
      count(.data$DZIEDZINA_KONT) %>%
      mutate(odsetek = .data$n / n_dist)
    if (nrow(tab) %in% 0) {
      return(list())
    } else {
      tab %>%
        as.list() %>%
        return()
    }
  }
}
#' @title Obliczanie wskaznikow dla 1. fali monitoringu - dane administracyjne
#' @description Funkcja licząca rozkład liczebności absolwentów kontynuujących
#' naukę na studiach w podziale na dyscypliny Funkcja liczy wskaźnik tylko dla
#' absolwentów techników.
#' @param x ramka danych pośrednich P3
#' @param dyscyplina_kont_df ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej dyscyplinie (tabela danych pośrednich P2 lub zawierająca
#' analogiczne informacje oraz te same nazwy kolumn co tabela P2)
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie
#' grudzień
#' @return lista
#' @importFrom dplyr %>% filter .data count mutate select left_join n_distinct
#' slice_max
liczebnosc_dyscypliny_tech_ad1 = function(x, dyscyplina_kont_df, raport = 0, mies = 12) {
  stopifnot(is.data.frame(x),
            is.data.frame(dyscyplina_kont_df),
            raport %in% c(0, 1),
            mies %in% c(1:12))

  # mies_kont = 2020 * 12
  mies = 2020 * 12 + mies

  dyscyplina_kont_df = dyscyplina_kont_df %>%
    select(ID_ABS, ROK_ABS, DYSCYPLINA_WIODACA_KONT, OKRES_KONT, TYP_SZK) %>%
    filter(.data$TYP_SZK %in% "Technikum") %>% # .data$OKRES_KONT %in% mies_kont
    select(-c(OKRES_KONT, TYP_SZK))


  x = x %>%
    filter(.data$OKRES %in% mies,
           .data$TYP_SZK %in% "Technikum") %>%
    left_join(dyscyplina_kont_df,
              by = c("ID_ABS", "ROK_ABS")) %>%
    filter(.data$NAUKA_STUDIA %in% 1) %>%
    filter(!(is.na(.data$DYSCYPLINA_WIODACA_KONT)))

  if (nrow(x) %in% 0) {
    return(list())
  } else {
    n_dist = n_distinct(x$ID_ABS)

    tab = x %>%
      count(.data$DYSCYPLINA_WIODACA_KONT) %>%
      mutate(odsetek = .data$n / n_dist)
    if (nrow(tab) %in% 0) {
      return(list())
    } else {
      tab %>%
        as.list() %>%
        return()
    }
  }
}
