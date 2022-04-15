library(tidyverse)
library(RPostgres)
library(httr)
library(chronicle)

con = DBI::dbConnect(odbc::odbc(), "PostgreSAKiRP")
waeo = dbReadTable(con,"waeos")
district = dbReadTable(con, "districts")
ward = dbReadTable(con,"wards")
village = dbReadTable(con,"villages")
faab = dbReadTable(con,"faab_coach")
amcos = dbReadTable(con,"amcos")

stock_url = "https://kc.humanitarianresponse.info/api/v1/data/1026275.csv"
stock_rawdata = GET(stock_url,authenticate(Sys.getenv("sakirp_user"),Sys.getenv("sakirp_pw")))
stock_content = content(stock_rawdata,"raw",encoding="UTF-8")
stock_data_raw = read_csv(stock_content) %>% 
  janitor::clean_names()

stock_data_clean = stock_data_raw %>% 
  mutate(
    survey_info_waeo = na_if(survey_info_waeo,"n/a"),
    survey_info_waeo = as.numeric(survey_info_waeo),
    survey_info_faab = na_if(survey_info_faab,"n/a"),
    survey_info_faab = as.numeric(survey_info_faab)
  ) %>% 
  rename(
    njano_in = ends_with("njano_in"),
    stock_beans = ends_with("closing_stock_bt"),
    stock_calima = survey_info_summary_beans_closing_stock_calimat,
    stock_red = survey_info_summary_beans_closing_stock_redt,
    stock_uyole = survey_info_summary_beans_closing_stock_njano_uyolet,
    stock_njano = survey_info_summary_beans_closing_stock_njanot
  ) %>% 
  left_join(district, by=c("survey_info_district"="id")) %>% 
  left_join(ward, by=c("survey_info_ward"="id")) %>% 
  left_join(waeo, by=c("survey_info_waeo"="id")) %>% 
  left_join(faab, by=c("survey_info_faab"="id")) %>% 
  left_join(amcos, by=c("survey_info_amcos"="id")) %>%
  mutate(
    data_collector = case_when(
      !is.na(waeo) ~ waeo,
      !is.na(name) ~ name
    )
  )

faab_data = stock_data_clean %>% 
  filter(!is.na(survey_info_faab))

waeo_data = stock_data_clean %>% 
  filter(!is.na(survey_info_waeo))

warehouse_stock = function(data) {
  data %>% 
    select(endtime,district=district.x,ward=ward.x,data_collector,
           amcos,stock_beans) %>% 
    group_by(district,ward,amcos) %>% 
    filter(endtime == max(endtime)) %>% arrange(amcos)
}

warehouse_variety_stock = function(data) {
  data %>% 
    select(endtime,District=district.x,
           Ward = ward.x,amcos,data_collector,stock_calima,stock_red,stock_njano,
           stock_uyole,stock_beans) %>% 
    group_by(District,Ward,amcos) %>% 
    filter(endtime == max(endtime)) %>% 
    arrange(amcos)
}

faab_data_summary = warehouse_stock(faab_data)
waeo_data_summary = warehouse_stock(waeo_data)

variety_data_summary = warehouse_variety_stock(faab_data)

ggplot(faab_data_summary, aes(reorder(amcos,stock_beans),stock_beans)) +
  geom_col() + 
  coord_flip() +
  theme_bw() +
  xlab(NULL) + 
  ylab(NULL)


stock_report = add_text(text_title = "AMCOS Beans Aggregation",
                        text = "WAEO Data.",
                        title_level = 1) %>%
  add_table(table = waeo_data_summary %>% arrange(-stock_beans),
            table_title = "Warehouse stock",
            html_table_type = "kable",
            title_level = 1) %>% 
  add_barplot(dt = waeo_data_summary,
               bars = 'amcos',
               value = 'stock_beans',
              horizontal = TRUE,
              sort_by_value = TRUE,
              sort_decreasing = TRUE,
              ggtheme = 'minimal',
              barplot_title = "Beans stock by AMCOS (WAEO reporting)",
              x_axis_label = "",y_axis_label = "Kg"
  ) %>% 
  add_table(table = faab_data_summary %>% arrange(-stock_beans),
            table_title = "Warehouse stock",
            html_table_type = "kable",
            title_level = 1) %>% 
  add_barplot(dt = faab_data_summary,
                    bars = 'amcos',
                    value = 'stock_beans',
                    horizontal = TRUE,
                    sort_by_value = TRUE,
                    sort_decreasing = TRUE,
                    ggtheme = 'minimal',
                    barplot_title = "Beans stock by AMCOS (FAAB reporting)",
                    x_axis_label = "",y_axis_label = "Kg"
  )

render_report(report = stock_report,
              title = "AMCOS Beans Aggregation",
              filename = "beans_aggregation",
              keep_rmd = TRUE,
              output_format = 'pdf')

faab_amcos_bean_varieties = stock_data_clean %>% filter(survey_info_data_collector==1) %>% 
  select(endtime,amcos,data_collector,stock_calima,stock_red,stock_njano,
         stock_uyole,stock_beans) %>% 
  group_by(amcos) %>% 
  filter(endtime == max(endtime))
