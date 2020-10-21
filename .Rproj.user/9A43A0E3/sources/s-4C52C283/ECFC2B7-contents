#import the data
library(readr)
eavs_2018 <- read_csv("data/EAVS_2018_for_Public_Release_Updates3.csv")
eavs_2016 <- read_csv("data/EAVS_2016_Final_Data_for_Public_Release_v3.csv")

#separate Arkansas
library(dplyr)
ar_eavs_2018 <- dplyr::filter(eavs_2018, State_Abbr =="AR")
ar_eavs_2016 <- dplyr::filter(eavs_2016, State =="AR")

#filter by mail for Arkansas, 2018
ar_eavs_2018_bymail <-dplyr::select(ar_eavs_2018, FIPSCode, Jurisdiction_Name, A1a, C1a, C1b, C1c, C1d, C1e, C1f, C1g_Other, C1g, C1h_Other, C1h, C1i_Other, C1i, C1Comments, C2a, C2Comments, C3a, C3Comments, C4a, C4b, C4c, C4d, C4e, C4f, C4g, C4h, C4i, C4j, C4k, C4l, C4m, C4n, C4o, C4p_Other, C4p, C4q_Other, C4q, C4r_Other, C4r, C4Comments, D1a)
transform(ar_eavs_2018_bymail, D1a = as.numeric(D1a)) %>%
  filter(!is.na(D1a)) %>%
  summarize(.,
            total_2018_votes=sum(D1a))
ar_eavs_2018_bymail_notcounted <- dplyr::select(ar_eavs_2018_bymail, FIPSCode, Jurisdiction_Name, A1a, C1a, C1b, C3a, C4a, C4b, D1a) %>%
  dplyr::filter(., C1b != "Data not available" & C4a != "Data not available") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C3a = as.numeric(C3a), C4a = as.numeric(C4a), C4b = as.numeric(C4b), D1a = as.numeric(D1a)) %>%
  mutate(., pct_counted=C3a/C1b, pct_rejected=C4a/C1b, do_add_up=(C3a+C4a)/C1b)
dplyr::filter(ar_eavs_2018_bymail_notcounted, do_add_up==1) %>%
  dplyr::summarize(.,
                 total_regvoters_2018=sum(A1a),
                 total_mail_trans_2018=sum(C1a),
                 total_mail_returnedforcount_2018=sum(C1b),
                 total_mail_counted=sum(C3a),
                 total_mail_rejected_2018=sum(C4a),
                 total_mail_rej_deadline_2018=sum(C4b),
                 total_votes_2018=sum(D1a),
                 pct_absrejected=sum(C4a)/sum(C1b),
                 pct_absrejected_overall=sum(C4a)/sum(D1a),
                 pct_abs_overall=sum(C1b)/sum(D1a))
ar_eavs_2018_bymail_notcounted_deadline <- dplyr::select(ar_eavs_2018_bymail, FIPSCode, Jurisdiction_Name, A1a, C1a, C1b, C4a, C4b, D1a) %>%
  dplyr::filter(., C1a != "Data not available" & C1b != "Data not available" & C4a != "Data not available" & C4b != "Data not available" & C4b != "Does not apply") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C4a = as.numeric(C4a), C4b = as.numeric(C4b), D1a = as.numeric(D1a)) %>%
  mutate(., pct_rej_deadline=C4b/C4a)
dplyr::summarize(ar_eavs_2018_bymail_notcounted_deadline,
                 total_regvoters_2018=sum(A1a),
                 total_mail_trans_2018=sum(C1a),
                 total_mail_returnedforcount_2018=sum(C1b),
                 total_mail_rejected_2018=sum(C4a),
                 total_mail_rej_deadline_2018=sum(C4b),
                 total_votes_2018=sum(D1a))
ar_eavs_2018_bymail_notcounted_missig <- dplyr::select(ar_eavs_2018_bymail, FIPSCode, Jurisdiction_Name, A1a, C1a, C1b, C4a, C4b, C4e, D1a) %>%
  dplyr::filter(., C1a != "Data not available" & C1b != "Data not available" & C4a != "Data not available" & C4e != "Data not available" & C4e != "Does not apply") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C4a = as.numeric(C4a), C4e = as.numeric(C4e), D1a = as.numeric(D1a)) %>%
  mutate(., pct_rej_missig=C4e/C4a)
dplyr::summarize(ar_eavs_2018_bymail_notcounted_missig,
                 total_returendforcount_2018=sum(C1b),
                 total_rejected_2018=sum(C4a),
                 total_rej_misssig=sum(C4e))

#filter to provisional for Arkansas, 2018
ar_eavs_2018_provisional <-dplyr::select(ar_eavs_2018, FIPSCode, Jurisdiction_Name, A1a, E1a, E1b, E1c, E1d) %>%
  dplyr::filter(., E1a != "Data not available") %>%
  transform(E1a = as.numeric(E1a), E1d = as.numeric(E1d))
ar_eavs_2018_provisional[is.na(ar_eavs_2018_provisional)] = 0
dplyr::summarize(ar_eavs_2018_provisional,
                 total_provisional=sum(E1a),
                 total_prov_rejected=sum(E1d))

#filter by mail for Arkansas, 2016
ar_eavs_2016_bymail <-dplyr::select(ar_eavs_2016, FIPSCode, JurisdictionName, A1a, C1a, C1b, C1c, C1d, C1e, C1f_Other, C1f, C1g_Other, C1g, C1h_Other, C1h, C1Comments, C2, C2Comments, C3, C3Comments, C4a, C4b, C4c, C4d, C4Comments, C5a, C5b, C5c, C5d, C5e, C5f, C5g, C5h, C5i, C5j, C5k, C5l, C5m, C5n, C5o_Other, C5o, C5p_Other, C5p, C5q_Other, C5q, C5r_Other, C5r, C5s_Other, C5s, C5t_Other, C5t, C5u_Other, C5u, C5v_Other, C5v, C5Comments)
ar_eavs_2016_bymail_notcounted <- dplyr::select(ar_eavs_2016_bymail, FIPSCode, JurisdictionName, A1a, C1a, C1b, C4a, C4b) %>%
  dplyr::filter(., C1a != "-999999: Data Not Available" & C1b != "-999999: Data Not Available" & C1b != "-888888: Not Applicable" & C4a != "-888888: Not Applicable" & C4a != "-999999: Data Not Available" & C4b != "-999999: Data Not Available") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C4a = as.numeric(C4a), C4b = as.numeric(C4b)) %>%
  mutate(., pct_counted=C4a/C1b, pct_rejected=C4b/C1b, do_add_up=(C4a+C4b)/C1b)
ar_eavs_2016_bymail_returnedforcount <- transform(ar_eavs_2016_bymail, C1b = as.numeric(C1b)) %>%
  filter(., C1b != "-999999: Data Not Available" & C1b != "-888888: Not Applicable") %>%
  summarize(.,
            total_mail_returnedforcount_2016=sum(C1b))
transform(ar_eavs_2016_bymail, C1a = as.numeric(C1a)) %>%
  filter(., C1a != "-999999: Data Not Available" & C1a != "-888888: Not Applicable") %>%
  summarize(.,
            total_mail_returnedforcount_2016=sum(C1a))
dplyr::filter(ar_eavs_2016_bymail_notcounted, do_add_up==1) %>%
  dplyr::summarize(.,
                 total_regvoters_2016=sum(A1a),
                 total_mail_trans_2016=sum(C1a),
                 total_mail_returnedforcount_2016=sum(C1b),
                 total_mail_counted_2016=sum(C4a),
                 total_mail_rejected_2016=sum(C4b),
                 pct_absrejected_2016=sum(C4b)/sum(C1b),
                 pct_absrejected_overall=sum(C4b)/1137772,
                 pct_abs_overall=sum(C1b)/1137772)
ar_eavs_2016_bymail_notcounted_deadline <- dplyr::select(ar_eavs_2016_bymail, FIPSCode, JurisdictionName, A1a, C1a, C1b, C4a, C4b, C5a) %>%
  dplyr::filter(., C1a != "-999999: Data Not Available" & C1b != "-999999: Data Not Available" & C1b != "-888888: Not Applicable" & C4a != "-888888: Not Applicable" & C4a != "-999999: Data Not Available" & C4b != "-999999: Data Not Available" & C5a != "-888888: Not Applicable" & C5a != "-999999: Data Not Available") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C4a = as.numeric(C4a), C4b = as.numeric(C4b), C5a = as.numeric(C5a)) %>%
  mutate(., pct_counted=C4a/C1b, pct_rejected=C4b/C1b, pct_rej_deadline=C5a/C4b)
dplyr::summarize(ar_eavs_2016_bymail_notcounted_deadline,
                 total_regvoters_2016=sum(A1a),
                 total_mail_trans_2016=sum(C1a),
                 total_mail_returnedforcount_2016=sum(C1b),
                 total_mail_counted_2016=sum(C4a),
                 total_mail_rejected_2016=sum(C4b),
                 total_mail_rejected_deadline_2016=sum(C5a))
ar_eavs_2016_bymail_notcounted_missignature <- dplyr::select(ar_eavs_2016_bymail, FIPSCode, JurisdictionName, A1a, C1a, C1b, C4a, C4b, C5a, C5b, C5c, C5d, D1a) %>%
  dplyr::filter(., C1a != "-999999: Data Not Available" & C1b != "-999999: Data Not Available" & C1b != "-888888: Not Applicable" & C4a != "-888888: Not Applicable" & C4a != "-999999: Data Not Available" & C4b != "-999999: Data Not Available" & C5d != "-888888: Not Applicable" & C5d != "-999999: Data Not Available"  & D1a != "-999999: Data Not Available") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C4a = as.numeric(C4a), C4b = as.numeric(C4b), C5b = as.numeric(C5b), C5c = as.numeric(C5c), C5d = as.numeric(C5d), D1a = as.numeric(D1a)) %>%
  mutate(., pct_counted=C4a/C1b, pct_rejected=C4b/C1b, pct_rej_missignature=C5d/C4b)
dplyr::summarize(ar_eavs_2016_bymail_notcounted_deadline,
                 total_mail_returnedforcount_2016=sum(C1b),
                 total_mail_rejected_2016=sum(C4b))

#filter to provisional for Arkansas, 2016
ar_eavs_2016_provisional <-dplyr::select(ar_eavs_2016, FIPSCode, JurisdictionName, A1a, E1a, E1b, E1c, E1d) %>%
  dplyr::filter(., E1a != "Data not available" & E1b != "Data not available" & E1c != "Data not available") %>%
  transform(E1a = as.numeric(E1a), E1d = as.numeric(E1d))
ar_eavs_2016_provisional[is.na(ar_eavs_2016_provisional)] = 0
dplyr::summarize(ar_eavs_2016_provisional,
                 total_provisional=sum(E1a),
                 total_prov_rejected=sum(E1d))

#import Arkansas SoS absentee ballots requested so far
library(stringr)
ar_abs2020_requested <- read_csv("data/ar_sos_abs_20201016.csv")
ar_abs2020_requested_countytotals <- dplyr::group_by(ar_abs2020_requested, County) %>%
  summarize(., abs_requested=n())
dplyr::summarize

#import Arkansas ballots requested in 2016
ar_abs_roster_2016 <- read_csv("data/ar_abs_roster_20161109.csv")
ar_abs_roster_2016_bycounty <- dplyr::group_by(ar_abs_roster_2016, County) %>%
  summarize(., abs_roster=n())

#National comparison on absentee, 2018
eavs_2018_bymail <-dplyr::select(eavs_2018, FIPSCode, Jurisdiction_Name, A1a, C1a, C1b, C1c, C1d, C1e, C1f, C1g_Other, C1g, C1h_Other, C1h, C1i_Other, C1i, C1Comments, C2a, C2Comments, C3a, C3Comments, C4a, C4b, C4c, C4d, C4e, C4f, C4g, C4h, C4i, C4j, C4k, C4l, C4m, C4n, C4o, C4p_Other, C4p, C4q_Other, C4q, C4r_Other, C4r, C4Comments, D1a)
eavs_2018_bymail_notcounted <- dplyr::select(eavs_2018_bymail, FIPSCode, Jurisdiction_Name, A1a, C1a, C1b, C3a, C4a, C4b, C4e, D1a) %>%
  dplyr::filter(., C1a != "Data not available" & C1b != "Data not available" & C4a != "Data not available") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C3a = as.numeric(C3a), C4a = as.numeric(C4a), C4b = as.numeric(C4b), D1a = as.numeric(D1a)) %>%
  mutate(., pct_counted=C3a/C1b, pct_rejected=C4a/C1b, do_add_up=(C3a+C4a)/C1b)
dplyr::filter(eavs_2018_bymail_notcounted, do_add_up==1) %>%
  dplyr::summarize(.,
                 total_regvoters_2018=sum(A1a),
                 total_mail_trans_2018=sum(C1a),
                 total_mail_returnedforcount_2018=sum(C1b),
                 total_mail_rejected_2018=sum(C4a),
                 total_mail_rej_deadline_2018=sum(C4b),
                 total_votes_2018=sum(D1a),
                 pct_absrejected=sum(C4a)/sum(C1b),
                 pct_absrejected_overall=sum(C4a)/sum(D1a),
                 pct_abs_overall=sum(C1b)/sum(D1a))
eavs_2018_bymail_notcounted_deadline <- dplyr::select(eavs_2018_bymail, FIPSCode, Jurisdiction_Name, A1a, C1a, C1b, C4a, C4b, D1a) %>%
  dplyr::filter(., C1a != "Data not available" & C1b != "Data not available" & C4a != "Data not available" & C4b != "Data not available" & C4b != "Does not apply") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C4a = as.numeric(C4a), C4b = as.numeric(C4b), D1a = as.numeric(D1a)) %>%
  mutate(., pct_rej_deadline=C4b/C4a)
dplyr::summarize(eavs_2018_bymail_notcounted_deadline,
                 total_regvoters_2018=sum(A1a),
                 total_mail_trans_2018=sum(C1a),
                 total_mail_returnedforcount_2018=sum(C1b),
                 total_mail_rejected_2018=sum(C4a),
                 total_mail_rej_deadline_2018=sum(C4b),
                 total_votes_2018=sum(D1a))
eavs_2018_bymail_notcounted_missig <- dplyr::select(eavs_2018_bymail, FIPSCode, Jurisdiction_Name, A1a, C1a, C1b, C4a, C4b, C4e, D1a) %>%
  dplyr::filter(., C1a != "Data not available" & C1b != "Data not available" & C4a != "Data not available" & C4e != "Data not available" & C4e != "Does not apply") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C4a = as.numeric(C4a), C4e = as.numeric(C4e), D1a = as.numeric(D1a)) %>%
  mutate(., pct_rej_missig=C4e/C4a)

#National comparison on provisional, 2018
eavs_2018_provisional <-dplyr::select(eavs_2018, FIPSCode, Jurisdiction_Name, A1a, E1a, E1b, E1c, E1d) %>%
  dplyr::filter(., E1a != "Data not available" & E1b != "Data not available" & E1c != "Data not available") %>%
  transform(E1a = as.numeric(E1a), E1d = as.numeric(E1d))
eavs_2018_provisional[is.na(eavs_2018_provisional)] = 0
dplyr::summarize(eavs_2018_provisional,
                 total_provisional=sum(E1a),
                 total_prov_rejected=sum(E1d))

#National comparison on absentee, 2016
eavs_2016_bymail <-dplyr::select(eavs_2016, FIPSCode, JurisdictionName, A1a, C1a, C1b, C1c, C1d, C1e, C1f_Other, C1f, C1g_Other, C1g, C1h_Other, C1h, C1Comments, C2, C2Comments, C3, C3Comments, C4a, C4b, C4c, C4d, C4Comments, C5a, C5b, C5c, C5d, C5e, C5f, C5g, C5h, C5i, C5j, C5k, C5l, C5m, C5n, C5o_Other, C5o, C5p_Other, C5p, C5q_Other, C5q, C5r_Other, C5r, C5s_Other, C5s, C5t_Other, C5t, C5u_Other, C5u, C5v_Other, C5v, C5Comments, D1a)
eavs_2016_bymail_notcounted <- dplyr::select(eavs_2016_bymail, FIPSCode, JurisdictionName, A1a, C1a, C1b, C4a, C4b, D1a) %>%
  dplyr::filter(., C1a != "-999999: Data Not Available" & C1b != "-999999: Data Not Available" & C1b != "-888888: Not Applicable" & C4a != "-888888: Not Applicable" & C4a != "-999999: Data Not Available" & C4b != "-888888: Not Applicable" & C4b != "-999999: Data Not Available" & D1a != "-999999: Data Not Available") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C4a = as.numeric(C4a), C4b = as.numeric(C4b), D1a = as.numeric(D1a)) %>%
  mutate(., pct_counted=C4a/C1b, pct_rejected=C4b/C1b, do_add_up=(C4a+C4b)/C1b)
dplyr::filter(eavs_2016_bymail_notcounted, do_add_up==1) %>%
  dplyr::summarize(.,
                 total_regvoters_2016=sum(A1a),
                 total_mail_trans_2016=sum(C1a),
                 total_mail_returnedforcount_2016=sum(C1b),
                 total_mail_counted_2016=sum(C4a),
                 total_mail_rejected_2016=sum(C4b),
                 pct_absrejected=sum(C4b)/sum(C1b),
                 pct_absrejected_overall=sum(C4b)/138846571,
                 pct_abs_overall=sum(C1b)/138846571)
eavs_2016_bymail_notcounted_deadline <- dplyr::select(eavs_2016_bymail, FIPSCode, JurisdictionName, A1a, C1a, C1b, C4a, C4b, C5a, D1a) %>%
  dplyr::filter(., C1a != "-999999: Data Not Available" & C1b != "-999999: Data Not Available" & C1b != "-888888: Not Applicable" & C4a != "-888888: Not Applicable" & C4a != "-999999: Data Not Available" & C4b != "-888888: Not Applicable" & C4b != "-999999: Data Not Available" & C5a != "-888888: Not Applicable" & C5a != "-999999: Data Not Available" & D1a != "-999999: Data Not Available") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C4a = as.numeric(C4a), C4b = as.numeric(C4b), C5a = as.numeric(C5a), D1a = as.numeric(D1a)) %>%
  mutate(., pct_counted=C4a/C1b, pct_rejected=C4b/C1b, pct_rej_deadline=C5a/C4b)
dplyr::summarize(eavs_2016_bymail_notcounted_deadline,
                 total_regvoters_2016=sum(A1a),
                 total_mail_trans_2016=sum(C1a),
                 total_mail_returnedforcount_2016=sum(C1b),
                 total_mail_counted_2016=sum(C4a),
                 total_mail_rejected_2016=sum(C4b),
                 total_mail_rejected_deadline_2016=sum(C5a))
eavs_2016_bymail_notcounted_missignature <- dplyr::select(eavs_2016_bymail, FIPSCode, JurisdictionName, A1a, C1a, C1b, C4a, C4b, C5a, C5b, C5c, C5d, D1a) %>%
  dplyr::filter(., C1a != "-999999: Data Not Available" & C1b != "-999999: Data Not Available" & C1b != "-888888: Not Applicable" & C4a != "-888888: Not Applicable" & C4a != "-999999: Data Not Available" & C4b != "-999999: Data Not Available" & C5d != "-888888: Not Applicable" & C5d != "-999999: Data Not Available"  & D1a != "-999999: Data Not Available") %>%
  transform(A1a = as.numeric(A1a), C1a = as.numeric(C1a), C1b = as.numeric(C1b), C4a = as.numeric(C4a), C4b = as.numeric(C4b), C5b = as.numeric(C5b), C5c = as.numeric(C5c), C5d = as.numeric(C5d), D1a = as.numeric(D1a)) %>%
  mutate(., pct_counted=C4a/C1b, pct_rejected=C4b/C1b, pct_rej_missignature=C5d/C4b)

#National comparison on provisional, 2016
eavs_2016_provisional <-dplyr::select(eavs_2016, FIPSCode, Jurisdiction_Name, A1a, E1a, E1b, E1c, E1d) %>%
  dplyr::filter(., E1a != "Data not available" & E1b != "Data not available" & E1c != "Data not available") %>%
  transform(E1a = as.numeric(E1a), E1d = as.numeric(E1d))
eavs_2016_provisional[is.na(eavs_2016_provisional)] = 0
dplyr::summarize(eavs_2016_provisional,
                 total_provisional=sum(E1a),
                 total_prov_rejected=sum(E1d))

#UOCAVA ballots, 2016
ar_eavs_2016_uocava <-dplyr::select(ar_eavs_2016, FIPSCode, JurisdictionName, A1a, B1a, B2a, B8a) %>%
  dplyr::filter(., A1a != "-999999: Data Not Available" & B2a != "-999999: Data Not Available" & B8a != "-999999: Data Not Available" & B8a != "-999991") %>%
  transform(A1a = as.numeric(A1a), B1a = as.numeric(B1a), B2a = as.numeric(B2a), B8a = as.numeric(B8a))
ar_eavs_2016_uocava[is.na(ar_eavs_2016_uocava)] = 0
dplyr::summarize(ar_eavs_2016_uocava,
                 uocava_transmitted=sum(B1a),
                 uocava_counted=sum(B8a),
                 uocava_returned=sum(B2a))

#Export for mapping
ar_eavs_2016_bymail_notcounted_mapping <- dplyr::filter(ar_eavs_2016_bymail_notcounted, do_add_up==1)
write.csv(ar_eavs_2016_bymail_notcounted_mapping, "ar_eavs_2016_bymail_notcounted_mapping.csv")
write.csv(ar_eavs_2018_bymail_notcounted, "ar_eavs_2018_bymail_notcounted.csv")
write.csv(ar_eavs_2016_bymail_notcounted, "ar_eavs_2016_bymail_notcounted.csv")

