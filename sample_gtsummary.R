# ==========================================================
# gtsummary サンプルスクリプト（broom.helpers 不使用）
# ==========================================================
library(gtsummary)
library(gt)

# ----------------------------------------------------------
# 0. 出力先ディレクトリ（環境変数から組み立て）
# ----------------------------------------------------------
env_vars <- c("AEGIS_MOLECULE", "AEGIS_STUDY", "AEGIS_ENV")
unset <- env_vars[!nzchar(Sys.getenv(env_vars))]
if (length(unset) > 0) {
  stop("未設定の環境変数があります: ", paste(unset, collapse = ", "))
}

out_dir <- paste0(
  "/", Sys.getenv("AEGIS_MOLECULE"),
  "/", Sys.getenv("AEGIS_STUDY"),
  "/ReportingEvent10/", Sys.getenv("AEGIS_ENV"),
  "/output"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
print(out_dir)

# ----------------------------------------------------------
# 1. 記述統計表（tbl_summary）※ broom.helpers 不要
# ----------------------------------------------------------
tbl1 <- trial |>
  tbl_summary(
    by = trt,
    include = c(age, stage, grade, response, death),
    label = list(
      age      ~ "年齢（歳）",
      stage    ~ "病期",
      grade    ~ "分化度",
      response ~ "奏効",
      death    ~ "死亡"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany"
  ) |>
  add_p() |>
  add_overall() |>
  modify_header(label ~ "**項目**") |>
  bold_labels()

tbl1

tbl1 |>
  gtsummary::as_gt() |>
  gt::gtsave(filename = file.path(out_dir, "summary_table.html"))

# ----------------------------------------------------------
# 2. ロジスティック回帰（base R + gt で表を作成）
#    tbl_regression() の代わり
# ----------------------------------------------------------
model <- glm(
  response ~ trt + age + stage,
  data   = trial,
  family = binomial
)

coefs <- summary(model)$coefficients
ci    <- suppressMessages(confint.default(model))  # Wald 信頼区間

reg_df <- data.frame(
  term    = rownames(coefs),
  OR      = exp(coefs[, "Estimate"]),
  CI_low  = exp(ci[, 1]),
  CI_high = exp(ci[, 2]),
  p.value = coefs[, "Pr(>|z|)"],
  row.names = NULL
)
reg_df <- reg_df[reg_df$term != "(Intercept)", ]

tbl2 <- reg_df |>
  gt() |>
  cols_merge(columns = c(CI_low, CI_high), pattern = "{1}, {2}") |>
  fmt_number(columns = c(OR, CI_low, CI_high), decimals = 2) |>
  fmt(columns = p.value, fns = function(x) gtsummary::style_pvalue(x)) |>
  cols_label(
    term    = "項目",
    OR      = "OR",
    CI_low  = "95% CI",
    p.value = "p値"
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = p.value, rows = p.value < 0.05)
  )

tbl2

gt::gtsave(tbl2, filename = file.path(out_dir, "regression_table.html"))

# flextable / officer が未インストールの場合は先に入れてください
# install.packages(c("flextable", "officer"))
library(flextable)
library(officer)

# ----------------------------------------------------------
# 回帰表（docx 用）の作成
# ----------------------------------------------------------
reg_out <- data.frame(
  term    = reg_df$term,
  or      = sprintf("%.2f", reg_df$OR),
  ci      = sprintf("%.2f, %.2f", reg_df$CI_low, reg_df$CI_high),
  p_value = gtsummary::style_pvalue(reg_df$p.value),
  stringsAsFactors = FALSE
)

ft <- flextable(reg_out) |>
  set_header_labels(
    term    = "項目",
    or      = "OR",
    ci      = "95% CI",
    p_value = "p値"
  ) |>
  bold(i = which(reg_df$p.value < 0.05), j = "p_value") |>
  autofit()

# ----------------------------------------------------------
# docx として保存
# ----------------------------------------------------------
save_as_docx(
  ft,
  path = file.path(out_dir, "regression_table.docx")
)