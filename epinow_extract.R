#rm(list=ls())
#load packages
library(EpiNow2)
library(tidyverse)

#pull data set of clusters
clusters <- read_csv("P:/COVID-19/R0 Malaysia/data/cluster data/clusters_dashboard.csv", 
                     col_types = cols(no = col_number(), bangau = col_number(), 
                                      benteng = col_number(), benteng_pk = col_number(), 
                                      date = col_date(format = "%d/%m/%Y"), 
                                      dti = col_number(), import = col_number(), 
                                      kepayan = col_number(), laut = col_number(), 
                                      meru = col_number(), penjara_jawi = col_number(), 
                                      penjara_reman = col_number(), penjara_sandakan = col_number(), 
                                      penjara_tapah = col_number(), pts_tawau = col_number(), 
                                      pulau = col_number(), rumah_merah = col_number(), 
                                      sivagangga = col_number(), sungai = col_number(), 
                                      tabligh = col_number(), tawar = col_number(), 
                                      tembok = col_number(), gk_tawau=col_number(), matambai=col_number(), 
                                      jalan_harapan=col_number(), bakti=col_number(), tembok_gajah=col_number(), 
                                      kolam_air=col_number(), hala_mutiara=col_number(), pompod=col_number(),
                                      pagar_siput=col_number(), pagar_bentong=col_number(), tembok_mempaga=col_number(),
                                      telok_mas=col_number(), tropika=col_number(), tembok_kemus=col_number(), 
                                      tembok_nenas=col_number(), teratai=col_number(),sungai_jelok=col_number(),
                                      tembok_renggam=col_number(), tembok_sgudang=col_number(), tembok_taiping=col_number(),
                                      bukit_besi=col_number(), pengkalan_chepa=col_number(), tembok_bendera=col_number(),
                                      jln_awang=col_number(), dti_persiaran_wawasan=col_number(), dti_machap_umboo=col_number(),
                                      tembok_muhasabah=col_number(), imigresen_semuja=col_number(), dti_juru=col_number(),
                                      dti_jln_duta=col_number(), bukit_chagar=col_number(), sri_aman=col_number(), 
                                      padang_hijau=col_number(), choh2=col_number(), jln_salleh=col_number(),
                                      dti_lenggeng=col_number(), dti_sandakan=col_number(), pagar_siput2=col_number(),
                                      sri_lalang=col_number(), kg_selamat=col_number(), pagar_rimba=col_number(),
                                      meru_utama=col_number(), dti_tanah_merah=col_number(), dti_bukit_jalil2=col_number(),
                                      total_prison=col_number()))


#pull new data set
df <- read_csv("P:/COVID-19/R0 Malaysia/data/incidence/covid-19_my.csv", 
               col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                new_cases = col_number(), new_deaths = col_number(), 
                                total_cases = col_number(), total_deaths = col_number(), 
                                recover = col_number(), total_recover = col_number(), 
                                icu = col_number(), support = col_number()))

#lock down one set#NO MANIPULATION#
core_set <- df

#cut out the dates to join with cluster
new <- df[21:as.numeric(Sys.Date()-18260),c(1,3,4,7)] # TODO: if elapsed by a day +1 to the constant (contstant=18260) (+1 for each elapsed day)
new$no <- seq(1:as.numeric(Sys.Date()-18280)) # TODO: if elapsed by a day +1 to the constant (contstant=18280)  (+1 for each elapsed day)
new$date <- as.Date(new$date)

#merge the two sets
df <- full_join(new, clusters, by=c("no","date"))

#create a special variable for all immigration and prison outbreaks
df$ins <- df$dti+df$tembok+df$benteng_pk+df$penjara_reman+df$penjara_jawi+df$kepayan+df$penjara_tapah+
  df$rumah_merah+df$pts_tawau+df$penjara_sandakan+df$gk_tawau+df$matambai+df$jalan_harapan+df$bakti+df$sibuga+
  df$tembok_gajah + df$kolam_air + df$hala_mutiara + df$pompod + df$pagar_siput + df$pagar_bentong + 
  df$tembok_mempaga + df$telok_mas +df$tropika + df$tembok_kemus + df$tembok_nenas +df$sungai_jelok +df$tembok_renggam +
  df$tembok_sgudang +  df$tembok_taiping + df$bukit_besi + df$pengkalan_chepa + df$tembok_bendera +df$jln_awang + 
  df$dti_persiaran_wawasan + df$dti_machap_umboo + df$tembok_muhasabah +df$imigresen_semuja + df$dti_juru + 
  df$ dti_jln_duta + df$bukit_chagar +df$sri_aman + df$padang_hijau + df$choh2 + df$jln_salleh + df$dti_lenggeng +
  df$dti_sandakan + df$pagar_siput2 + df$sri_lalang + df$kg_selamat + df$pagar_rimba + df$meru_utama + df$dti_tanah_merah +
  df$dti_bukit_jalil2 +df$total_prison

##########################################################################################
#arbitrary cut of point for visualisation of clusters with at least 150 cases or more
##########################################################################################
#join the 2 sets
df$daily <- df$new_cases-df$ins-df$import#benteng ld has begun in an institution but quickly spiralled into community as well


#pick a small sample
df1 <- df %>% filter(date>as.Date("2020-03-01")&date<as.Date("2020-03-30"))
df2 <- df %>% filter(date>as.Date("2020-03-01")&date<as.Date("2020-03-14"))
df3 <- df %>% filter(date>as.Date("2020-03-14")&date<as.Date("2020-03-30"))

##########################################################################################
#run a sample of epi now
##########################################################################################
#define parameters
reporting_delay <- list(
  mean = convert_to_logmean(2, 1), mean_sd = 0.1,
  sd = convert_to_logsd(2, 1), sd_sd = 0.1,
  max = 10
)
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
reported_cases <- data.frame(date=df$date[39:531], confirm=df$daily[39:531])
head(reported_cases)

#run for the first batch of cases
estimates <- epinow(reported_cases = reported_cases,
                    generation_time = generation_time,
                    delays = delay_opts(incubation_period, reporting_delay),
                    rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                    stan = stan_opts(cores = 4))
plot(estimates)

#extracct from estimates
rt <- summary(estimates, type = "parameters", params = "R")
infections <- summary(estimates, output = "estimated_reported_cases")

#extrract as csv
write.csv(rt, "rt_epinow.csv")
write.csv(infections, "infections_epinow.csv")


##########################################################################################
#test for last 30 days
##########################################################################################
#define parameters
reporting_delay <- list(
  mean = convert_to_logmean(2, 1), mean_sd = 0.1,
  sd = convert_to_logsd(2, 1), sd_sd = 0.1,
  max = 10
)
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
reported_cases <- data.frame(date=df$date[500:531], confirm=df$daily[500:531])
head(reported_cases)

#run for the first batch of cases
estimates2 <- epinow(reported_cases = reported_cases,
                    generation_time = generation_time,
                    delays = delay_opts(incubation_period, reporting_delay),
                    rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                    stan = stan_opts(cores = 4))
plot(estimates2)

#extracct from estimates
rt2 <- summary(estimates2, type = "parameters", params = "R")
infections2 <- summary(estimates2, output = "estimated_reported_cases")

#extrract as csv
write.csv(rt2, "rt2_epinow.csv")
write.csv(infections2, "infections2_epinow.csv")

##########################################################################################
#test for last 100 days
##########################################################################################
#define parameters
reporting_delay <- list(
  mean = convert_to_logmean(2, 1), mean_sd = 0.1,
  sd = convert_to_logsd(2, 1), sd_sd = 0.1,
  max = 10
)
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
reported_cases <- data.frame(date=df$date[430:531], confirm=df$daily[430:531])
head(reported_cases)

#run for the first batch of cases
estimates3 <- epinow(reported_cases = reported_cases,
                     generation_time = generation_time,
                     delays = delay_opts(incubation_period, reporting_delay),
                     rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                     stan = stan_opts(cores = 4))
plot(estimates3)

#extracct from estimates
rt3 <- summary(estimates3, type = "parameters", params = "R")
infections3 <- summary(estimates3, output = "estimated_reported_cases")

#extrract as csv
write.csv(rt3, "rt3_epinow.csv")
write.csv(infections3, "infections3_epinow.csv")
