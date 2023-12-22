df_income <- tribble(
  ~'month', ~'institution', ~'income',
  'January', 'YNU', 26775,
  'February', NA, 0,
  'March', 'juku', 11851,
  'March', 'YNU', 21420,
  'April', 'YNU', 56763,
  'May', 'juku', 20843,
  'May', 'YNU', 34152,
  'June', 'UTEcon', 13572,
  'June', 'YNU', 41064,
  'June', 'RIETI', 72122,
  'July', 'UTEcon', 90117,
  'July', 'YNU', 41064,
  'July', 'RIETI', 70958,
  'August', 'UTEcon', 97484,
  'August', 'YNU', 49421,
  'August', 'RIETI', 72122,
  'September', 'UTEcon', 65837,
  'September', 'YNU', 55177,
  'September', 'RIETI', 20939,
  'October', 'UTEcon', 97167,
  'October', 'YNU', 95760
)

df_income <- df_income |> 
  janitor::adorn_totals(name = '合計')
