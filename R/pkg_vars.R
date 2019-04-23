pkg.env <- new.env(parent = emptyenv())

pkg.env$ep_ch_fb <- c("GA01/40", "GA02/60", "GA03/40", "GA04/60", "GA05/40", "GA06/60", "GA07/40", "GA08/60", "GA09/40", "GA10/60")

pkg.env$ep_ch_l <- c("L11/40", "L12/60", "L13/40", "L14/60", "L15/40", "L16/60", "L18/60", "L19/60", "L20/60")

pkg.env$ep_ch_m <- c("M21/40", "M22/60", "M23/40", "M24/60", "M25/60", "M26/60", "M27/60", "M28/60", "M29/60")

pkg.env$ep_ch_s <- c("GEORG", "S1/60", "S10/60", "S31/60", "S32/60")

pkg.env$ep_ch_u21 <- c(pkg.env$ep_ch_fb, pkg.env$ep_ch_l, pkg.env$ep_ch_m)

pkg.env$ep_ch_r <- c(pkg.env$ep_ch_l, pkg.env$ep_ch_m)

pkg.env$ep_ch_n <- c(pkg.env$ep_ch_m, pkg.env$ep_ch_s)

pkg.env$kur <- c("LK", "MK", "GEORGK")

pkg.env$riders <- readr::read_delim(file = here::here("data-raw/SVPSPE30.txt"),
                                    delim = ";",
                                    col_names = c("Licence", "Title", "LastName", "FirstName", "Business", "Address", "Address2",
                                                  "Country", "ZIP", "Place", "PhoneP", "PhoneB", "PhoneM", "Email", "LicenceTyp",
                                                  "Birthday"),
                                    col_types = "_iccccccccccccc_____c_c_",
                                    locale = readr::locale(encoding = 'ISO-8859-1'))


pkg.env$horses <- readr::read_delim(file = here::here("data-raw/SVPSPF30.txt"),
                                    delim = ";",
                                    col_names = c("Passport", "Name", "ShortRace", "Race", "Origin", "Color", "Sex", "Birthday"),
                                    col_types = "_ic_ccc__c_cc______________________")
