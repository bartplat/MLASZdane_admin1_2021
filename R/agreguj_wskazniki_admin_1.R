#' @title Obliczanie wskaznikow na poziomie zagregowanym dla 1. fali monitoringu
#'  - dane administracyjne
#' @description Tylko dla Branżowych Szkół 1. stopnia
#' @param wsk2 ramka danych z tabeli pośredniej nr 2 (P2) z wynikami z 1.
#' rundy monitoringu na danych administracyjnych
#' @param wsk3 ramka danych z tabeli pośredniej nr 3 (P3) z wynikami z 1.
#' rundy monitoringu na danych administracyjnych
#' @param wsk4 ramka danych z tabeli pośredniej nr 4 (P4) z wynikami z 1.
#' rundy monitoringu na danych administracyjnych
#' @param grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param monitoring rok, w którym przeprowadzany jest monitoring
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'  \item{\code{\link{dane_szkoly_ad1}},}
#'  \item{\code{\link{l_abs_ad1}},}
#'  \item{\code{\link{l_kobiet_ad1}},}
#'  \item{\code{\link{l_abs_zrodla_ad1}},}
#'  \item{\code{\link{formy_ad1}},}
#'  \item{\code{\link{S3_mies_ad1}},}
#'  \item{\code{\link{S2_mies_ad1}},}
#'  \item{\code{\link{zawody_S3_ad1}},}
#'  \item{\code{\link{E2_nauka_kontyn_ad1}},}
#'  \item{\code{\link{liczebnosc_branze_bs1_ad1}},}
#'  \item{\code{\link{Z4_ods_prac_mies_ad1}},}
#'  \item{\code{\link{Z8_formy_prac_mies_ad1}},}
#'  \item{\code{\link{Z9_kont_mlod_ad1}},}
#'  \item{\code{\link{W1_sr_doch_ad1}},}
#'  \item{\code{\link{W3_sr_doch_uop_ad1}},}
#'  \item{\code{\link{liczebnosc_branze_ucz_bs1_ad1}}}
#'  }
#' @export
#' @importFrom dplyr %>% filter .data left_join
agreguj_bs1_admin_1 = function(wsk2, wsk3, wsk4, grupy, raport, monitoring = 2021) {
  stopifnot(is.data.frame(wsk2),
            is.data.frame(wsk3),
            is.data.frame(wsk4),
            is.data.frame(grupy),
            raport %in% c(0, 1),
            is.numeric(monitoring))

  if (raport %in% 0) {
    wsk4 = wsk4 %>%
      filter(ROK_ABS %in% (monitoring - 1))
    wsk3 = wsk3 %>%
      filter(ROK_ABS %in% (monitoring - 1),
             ROK %in% (monitoring - 1))
    wsk2 = wsk2 %>%
      filter(ROK_ABS %in% (monitoring - 1))
  }

  stopifnot(
    c("ID_SZK", "ID_ABS", "ROK_ABS", "TYP_SZK", "TERYT_WOJ_SZK", "BRANZA") %in% names(wsk2),
    c("ID_SZK", "ID_ABS", "ROK_ABS", "TYP_SZK", "TERYT_WOJ_SZK", "BRANZA") %in% names(wsk3),
    c("ID_SZK", "ID_ABS", "ROK_ABS", "TYP_SZK", "TERYT_WOJ_SZK", "BRANZA") %in% names(wsk4)
  )

  # tworzenie obiektów ze wskaźnikami
  wskazniki_4 = agreguj_wskazniki(
    wsk4, grupy,
    dane_szkoly = dane_szkoly_ad1(.data),
    l_abs = l_abs_ad1(.data),
    l_kobiet = l_kobiet_ad1(.data),
    l_abs_zrodla = l_abs_zrodla_ad1(.data),
    formy = formy_ad1(.data),
    liczebnosc_branze_ucz = liczebnosc_branze_ucz_bs1_ad1(.data))

  wskazniki_3 = agreguj_wskazniki(
    wsk3, grupy, list("raport" = raport, "wsk2" = wsk2),
    ## wskaźnik S3
    S3_0_01 = S3_mies_ad1(.data, raport, 1),
    S3_0_02 = S3_mies_ad1(.data, raport, 2),
    S3_0_03 = S3_mies_ad1(.data, raport, 3),
    S3_0_04 = S3_mies_ad1(.data, raport, 4),
    S3_0_05 = S3_mies_ad1(.data, raport, 5),
    S3_0_06 = S3_mies_ad1(.data, raport, 6),
    S3_0_07 = S3_mies_ad1(.data, raport, 7),
    S3_0_08 = S3_mies_ad1(.data, raport, 8),
    S3_0_09 = S3_mies_ad1(.data, raport, 9),
    S3_0_10 = S3_mies_ad1(.data, raport, 10),
    S3_0_11 = S3_mies_ad1(.data, raport, 11),
    S3_0_12 = S3_mies_ad1(.data, raport, 12),
    ## wskaźnik S2
    S2_0_12 = S2_mies_ad1(.data, raport, 12),
    ## pozostałe wskaźniki
    tab_s3_zaw = zawody_S3_ad1(.data, raport, 12),
    E2_nauka_kontyn = E2_nauka_kontyn_ad1(.data, raport, 12),
    Z4_ucz = Z4_ods_prac_mies_ad1(.data, raport, 9, 12, TRUE),
    Z4_nie_ucz = Z4_ods_prac_mies_ad1(.data, raport, 9, 12, FALSE),
    Z8_formy_ucz = Z8_formy_prac_mies_ad1(.data, raport, 12, TRUE),
    Z8_formy_nie_ucz = Z8_formy_prac_mies_ad1(.data, raport, 12, FALSE),
    Z9_mlod_wrz = Z9_kont_mlod_ad1(.data, raport, 9),
    W1_ucz = W1_sr_doch_ad1(.data, raport, 9, 12, TRUE),
    W1_nie_ucz = W1_sr_doch_ad1(.data, raport, 9, 12, FALSE),
    W3_ucz = W3_sr_doch_uop_ad1(.data, raport, 9, 12, TRUE),
    W3_nie_ucz = W3_sr_doch_uop_ad1(.data, raport, 9, 12, FALSE),
    B2_bezrob = B2_ods_bezrob_ad1(.data, raport, 9, 12),
    N2_biernosc = N2_ods_biernosc_ad1(.data, raport, 9, 12),
    liczebnosc_branze_kont = liczebnosc_branze_bs1_ad1(.data, wsk2, raport, 12)
  )

  gr = wskazniki_4$grupy %>%
    left_join(wskazniki_3$grupy, by = names(grupy))
  od = wskazniki_4$grupyOdniesienia %>%
    left_join(wskazniki_3$grupyOdniesienia, by = names(grupy))

  wskazniki = list(grupy = gr, grupyOdniesienia = od)
  return(wskazniki)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym dla 1. fali monitoringu
#'  - dane administracyjne
#' @description Wskaźniki dla raportu branżowo-wojewódzkiego
#' @param wsk2 ramka danych z tabeli pośredniej nr 2 (P2) z wynikami z 1.
#' rundy monitoringu na danych administracyjnych
#' @param wsk3 ramka danych z tabeli pośredniej nr 3 (P3) z wynikami z 1.
#' rundy monitoringu na danych administracyjnych
#' @param wsk4 ramka danych z tabeli pośredniej nr 4 (P4) z wynikami z 1.
#' rundy monitoringu na danych administracyjnych
#' @param grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param monitoring rok, w którym przeprowadzany jest monitoring
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'  \item{\code{\link{dane_szkoly_ad1}},}
#'  \item{\code{\link{l_abs_ad1}},}
#'  \item{\code{\link{l_kobiet_ad1}},}
#'  \item{\code{\link{l_abs_zrodla_ad1}},}
#'  \item{\code{\link{formy_ad1}},}
#'  \item{\code{\link{licz_zawody_ad1}},}
#'  \item{\code{\link{zawody_S3_ad1}},}
#'  \item{\code{\link{S3_mies_ad1}},}
#'  \item{\code{\link{S2_mies_ad1}},}
#'  \item{\code{\link{liczebnosc_branze_bs1_ad1}},}
#'  \item{\code{\link{Z4_ods_prac_mies_ad1}},}
#'  \item{\code{\link{W1_sr_doch_ad1}},}
#'  \item{\code{\link{liczebnosc_branze_ucz_bs1_ad1}}}
#'  }
#' @export
#' @importFrom dplyr %>% filter .data left_join
agreguj_woj_branz_admin_1 = function(wsk2, wsk3, wsk4, grupy, raport, monitoring = 2021) {
  stopifnot(is.data.frame(wsk2),
            is.data.frame(wsk3),
            is.data.frame(wsk4),
            is.data.frame(grupy),
            raport %in% c(0, 1),
            is.numeric(monitoring))

  if (raport %in% 0) {
    wsk4 = wsk4 %>%
      filter(ROK_ABS %in% (monitoring - 1))
    wsk3 = wsk3 %>%
      filter(ROK_ABS %in% (monitoring - 1),
             ROK %in% (monitoring - 1))
    wsk2 = wsk2 %>%
      filter(ROK_ABS %in% (monitoring - 1))
  }

  stopifnot(
    c("ID_SZK", "ID_ABS", "ROK_ABS", "TYP_SZK", "TERYT_WOJ_SZK", "BRANZA") %in% names(wsk2),
    c("ID_SZK", "ID_ABS", "ROK_ABS", "TYP_SZK", "TERYT_WOJ_SZK", "BRANZA") %in% names(wsk3),
    c("ID_SZK", "ID_ABS", "ROK_ABS", "TYP_SZK", "TERYT_WOJ_SZK", "BRANZA") %in% names(wsk4)
  )

  # tworzenie obiektów ze wskaźnikami
  wskazniki_4 = agreguj_wskazniki(
    wsk4, grupy,
    dane_szkoly = dane_szkoly_ad1(.data),
    l_abs = l_abs_ad1(.data),
    l_kobiet = l_kobiet_ad1(.data),
    l_abs_zrodla = l_abs_zrodla_ad1(.data),
    formy = formy_ad1(.data),
    liczebnosc_branze_ucz = liczebnosc_branze_ucz_bs1_ad1(.data),
    liczebnosc_zawody = licz_zawody_ad1(.data))

  wskazniki_3 = agreguj_wskazniki(
    wsk3, grupy, list("raport" = raport, "wsk2" = wsk2),
    ## wskaźnik S3
    S3_0_01 = S3_mies_ad1(.data, raport, 1),
    S3_0_02 = S3_mies_ad1(.data, raport, 2),
    S3_0_03 = S3_mies_ad1(.data, raport, 3),
    S3_0_04 = S3_mies_ad1(.data, raport, 4),
    S3_0_05 = S3_mies_ad1(.data, raport, 5),
    S3_0_06 = S3_mies_ad1(.data, raport, 6),
    S3_0_07 = S3_mies_ad1(.data, raport, 7),
    S3_0_08 = S3_mies_ad1(.data, raport, 8),
    S3_0_09 = S3_mies_ad1(.data, raport, 9),
    S3_0_10 = S3_mies_ad1(.data, raport, 10),
    S3_0_11 = S3_mies_ad1(.data, raport, 11),
    S3_0_12 = S3_mies_ad1(.data, raport, 12),
    ## wskaźnik S2
    S2_0_12 = S2_mies_ad1(.data, raport, 12),
    ## pozostałe wskaźniki
    tab_s3_zaw = zawody_S3_ad1(.data, raport, 12),
    Z4_ucz = Z4_ods_prac_mies_ad1(.data, raport, 9, 12, TRUE),
    Z4_nie_ucz = Z4_ods_prac_mies_ad1(.data, raport, 9, 12, FALSE),
    W1_ucz = W1_sr_doch_ad1(.data, raport, 9, 12, TRUE),
    W1_nie_ucz = W1_sr_doch_ad1(.data, raport, 9, 12, FALSE),
    B2_bezrob = B2_ods_bezrob_ad1(.data, raport, 9, 12)
  )

  gr = wskazniki_4$grupy %>%
    left_join(wskazniki_3$grupy, by = names(grupy))
  od = wskazniki_4$grupyOdniesienia %>%
    left_join(wskazniki_3$grupyOdniesienia, by = names(grupy))

  wskazniki = list(grupy = gr, grupyOdniesienia = od)
  return(wskazniki)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym dla 1. fali monitoringu
#'  - dane administracyjne
#' @description Tylko dla Techników i Szkół policealnych
#' @param wsk2 ramka danych z tabeli pośredniej nr 2 (P2) z wynikami z 1.
#' rundy monitoringu na danych administracyjnych
#' @param wsk3 ramka danych z tabeli pośredniej nr 3 (P3) z wynikami z 1.
#' rundy monitoringu na danych administracyjnych
#' @param wsk4 ramka danych z tabeli pośredniej nr 4 (P4) z wynikami z 1.
#' rundy monitoringu na danych administracyjnych
#' @param grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @param raport rodzaj raportu: po roku (wartość 0) lub po 2 latach (wartość 1)
#' @param monitoring rok, w którym przeprowadzany jest monitoring
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'  \item{\code{\link{dane_szkoly_ad1}},}
#'  \item{\code{\link{l_abs_ad1}},}
#'  \item{\code{\link{l_kobiet_ad1}},}
#'  \item{\code{\link{l_abs_zrodla_ad1}},}
#'  \item{\code{\link{formy_ad1}},}
#'  \item{\code{\link{S3_mies_ad1}},}
#'  \item{\code{\link{S2_mies_ad1}},}
#'  \item{\code{\link{zawody_S3_ad1}},}
#'  \item{\code{\link{E2_nauka_kontyn_ad1}},}
#'  \item{\code{\link{liczebnosc_branze_bs1_ad1}},}
#'  \item{\code{\link{Z4_ods_prac_mies_ad1}},}
#'  \item{\code{\link{Z8_formy_prac_mies_ad1}},}
#'  \item{\code{\link{Z9_kont_mlod_ad1}},}
#'  \item{\code{\link{W1_sr_doch_ad1}},}
#'  \item{\code{\link{W3_sr_doch_uop_ad1}},}
#'  \item{\code{\link{liczebnosc_dyscypliny_tech_ad1}},}
#'  \item{\code{\link{liczebnosc_dziedziny_tech_ad1}}}
#'  }
#' @export
#' @importFrom dplyr %>% filter .data left_join
agreguj_tech_spolic_admin_1 = function(wsk2, wsk3, wsk4, grupy, raport, monitoring = 2021) {
  stopifnot(is.data.frame(wsk2),
            is.data.frame(wsk3),
            is.data.frame(wsk4),
            is.data.frame(grupy),
            raport %in% c(0, 1),
            is.numeric(monitoring))

  if (raport %in% 0) {
    wsk4 = wsk4 %>%
      filter(ROK_ABS %in% (monitoring - 1))
    wsk3 = wsk3 %>%
      filter(ROK_ABS %in% (monitoring - 1),
             ROK %in% (monitoring - 1))
    wsk2 = wsk2 %>%
      filter(ROK_ABS %in% (monitoring - 1))
  }

  stopifnot(
    c("ID_SZK", "ID_ABS", "ROK_ABS", "TYP_SZK", "TERYT_WOJ_SZK", "BRANZA") %in% names(wsk2),
    c("ID_SZK", "ID_ABS", "ROK_ABS", "TYP_SZK", "TERYT_WOJ_SZK", "BRANZA") %in% names(wsk3),
    c("ID_SZK", "ID_ABS", "ROK_ABS", "TYP_SZK", "TERYT_WOJ_SZK", "BRANZA") %in% names(wsk4)
  )

  # tworzenie obiektów ze wskaźnikami
  wskazniki_4 = agreguj_wskazniki(
    wsk4, grupy,
    dane_szkoly = dane_szkoly_ad1(.data),
    l_abs = l_abs_ad1(.data),
    l_kobiet = l_kobiet_ad1(.data),
    l_abs_zrodla = l_abs_zrodla_ad1(.data),
    formy = formy_ad1(.data),
    liczebnosc_branze_ucz = liczebnosc_branze_ucz_bs1_ad1(.data))

  wskazniki_3 = agreguj_wskazniki(
    wsk3, grupy, list("raport" = raport, "wsk2" = wsk2),
    ## wskaźnik S3
    S3_0_01 = S3_mies_ad1(.data, raport, 1),
    S3_0_02 = S3_mies_ad1(.data, raport, 2),
    S3_0_03 = S3_mies_ad1(.data, raport, 3),
    S3_0_04 = S3_mies_ad1(.data, raport, 4),
    S3_0_05 = S3_mies_ad1(.data, raport, 5),
    S3_0_06 = S3_mies_ad1(.data, raport, 6),
    S3_0_07 = S3_mies_ad1(.data, raport, 7),
    S3_0_08 = S3_mies_ad1(.data, raport, 8),
    S3_0_09 = S3_mies_ad1(.data, raport, 9),
    S3_0_10 = S3_mies_ad1(.data, raport, 10),
    S3_0_11 = S3_mies_ad1(.data, raport, 11),
    S3_0_12 = S3_mies_ad1(.data, raport, 12),
    ## wskaźnik S2
    S2_0_12 = S2_mies_ad1(.data, raport, 12),
    ## pozostałe wskaźniki
    tab_s3_zaw = zawody_S3_ad1(.data, raport, 12),
    E2_nauka_kontyn = E2_nauka_kontyn_ad1(.data, raport, 12),
    Z4_ucz = Z4_ods_prac_mies_ad1(.data, raport, 9, 12, TRUE),
    Z4_nie_ucz = Z4_ods_prac_mies_ad1(.data, raport, 9, 12, FALSE),
    Z8_formy_ucz = Z8_formy_prac_mies_ad1(.data, raport, 12, TRUE),
    Z8_formy_nie_ucz = Z8_formy_prac_mies_ad1(.data, raport, 12, FALSE),
    Z9_mlod_wrz = Z9_kont_mlod_ad1(.data, raport, 9),
    W1_ucz = W1_sr_doch_ad1(.data, raport, 9, 12, TRUE),
    W1_nie_ucz = W1_sr_doch_ad1(.data, raport, 9, 12, FALSE),
    W3_ucz = W3_sr_doch_uop_ad1(.data, raport, 9, 12, TRUE),
    W3_nie_ucz = W3_sr_doch_uop_ad1(.data, raport, 9, 12, FALSE),
    B2_bezrob = B2_ods_bezrob_ad1(.data, raport, 9, 12),
    N2_biernosc = N2_ods_biernosc_ad1(.data, raport, 9, 12),
    liczebnosc_dziedziny = liczebnosc_dziedziny_tech_ad1(.data, wsk2, raport, 12),
    liczebnosc_dyscypliny = liczebnosc_dyscypliny_tech_ad1(.data, wsk2, raport, 12)
  )

  gr = wskazniki_4$grupy %>%
    left_join(wskazniki_3$grupy, by = names(grupy))
  od = wskazniki_4$grupyOdniesienia %>%
    left_join(wskazniki_3$grupyOdniesienia, by = names(grupy))

  wskazniki = list(grupy = gr, grupyOdniesienia = od)
  return(wskazniki)
}
