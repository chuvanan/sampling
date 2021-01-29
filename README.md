# sampling



```r

library(dplyr)
names(data) = tolower(names(data))


# Lap danh sach `dsdn` va `dsqtd` -----------------------------------------


dsdn = data %>%
    filter(!loaihinhkt %in% c(1, 2, 3, 4, 11, 12, 13)) %>%
    filter(!(as.numeric(nganhc2) <= 43 & solaodong_cuoinam >= 100)) %>%
    filter(!(as.numeric(nganhc2) >= 45 & solaodong_cuoinam >= 50)) %>%
    filter(!nganhc2 %in% c("01", "02", "03", "55", "79", "85", "86", "87")) %>%
    filter(!manganhsxkd_chinh %in% c("41010", "49110", "49120", "50111", "50112",
                                     "50121", "50122", "51101", "51109", "51201", "51209", "64190", "64910")) %>%
    filter((as.numeric(nganhc2) >= 5 & as.numeric(nganhc2) <= 39) |
               (as.numeric(nganhc2) >= 49 & as.numeric(nganhc2) <= 53))

dsqtd = data %>%
    filter((manganhsxkd_chinh %in% c("64190") & loaihinhkt %in% c(3, 5)) |
               (manganhsxkd_chinh %in% c("64910") & loaihinhkt %in% c(3, 5)))


# Chon mau theo tinh/thanh pho --------------------------------------------


dsdn = dsdn %>%
    mutate(nhom_laodong = case_when(
        solaodong_cuoinam <= 9 ~ "Nhom 1",
        solaodong_cuoinam >= 10 & solaodong_cuoinam <= 49 ~ "Nhom 2",
        solaodong_cuoinam >= 50 & solaodong_cuoinam <= 99 ~ "Nhom 3",
        TRUE ~ NA_real_
    ))

dsdn_nhom1 = dsdn %>%
    filter(nhom_laodong %in% "Nhom 1")

mau_dsdn_nhom1 = local({

    .m1 = dsdn_nhom1 %>%
        filter(dn_matinh %in% c("01", "79")) %>%
        group_by(dn_matinh, manganhsxkd_chinh) %>%
        sample_frac(size = 5 / 100) %>%
        ungroup()

    .m2 = dsdn_nhom1 %>%
        filter(dn_matinh %in% c("31", "48", "75", "74")) %>%
        group_by(dn_matinh, manganhsxkd_chinh) %>%
        sample_frac(size = 7 / 100) %>%
        ungroup()

    .m3 = dsdn_nhom1 %>%
        filter(!dn_matinh %in% c("01", "79", "31", "48", "75", "74")) %>%
        group_by(dn_matinh, manganhsxkd_chinh) %>%
        sample_frac(size = 10 / 100) %>%
        ungroup()

    .m = bind_rows(.m1, .m2, .m3)
    .m
})

dsdn_nhom2 = dsdn %>%
    filter(nhom_laodong %in% "Nhom 2")

mau_dsdn_nhom2 = local({

    .m1 = dsdn_nhom2 %>%
        filter(dn_matinh %in% c("01", "79")) %>%
        group_by(dn_matinh, manganhsxkd_chinh) %>%
        sample_frac(size = 10 / 100) %>%
        ungroup()

    .m2 = dsdn_nhom2 %>%
        filter(dn_matinh %in% c("31", "48", "75", "74")) %>%
        group_by(dn_matinh, manganhsxkd_chinh) %>%
        sample_frac(size = 15 / 100) %>%
        ungroup()

    .m3 = dsdn_nhom2 %>%
        filter(!dn_matinh %in% c("01", "79", "31", "48", "75", "74")) %>%
        group_by(dn_matinh, manganhsxkd_chinh) %>%
        sample_frac(size = 20 / 100) %>%
        ungroup()

    .m = bind_rows(.m1, .m2, .m3)
    .m
})

dsdn_nhom3 = dsdn %>%
    filter(nhom_laodong %in% "Nhom 3")

mau_dsdn_nhom3 = dsdn_nhom3 %>%
    group_by(dn_matinh, manganhsxkd_chinh) %>%
    sample_frac(size = 50 / 100) %>%
    ungroup()

mau_dsdn = bind_rows(mau_dsdn_nhom1, mau_dsdn_nhom2, mau_dsdn_nhom3)


# Chon mau `dsqtd` --------------------------------------------------------

mau_dsqtd = dsqtd %>%
    group_by(dn_matinh) %>%
    sample_frac(size = 10 / 100) %>%
    ungroup()


# Ghep mau ----------------------------------------------------------------

mau_khaosat = bind_rows(mau_dsdn, mau_dsqtd)
ds_tinh = unique(mau_khaosat$dn_matinh)
N = length(ds_tinh)

for (i in seq_len(N)) {
    writexl::write_xlsx(x = mau_khaosat[mau_khaosat$dn_matinh %in% ds_tinh[i], ],
                        path = paste0("DS_KHAOSAT_", ds_tinh[i], ".xlsx"))
}

```
