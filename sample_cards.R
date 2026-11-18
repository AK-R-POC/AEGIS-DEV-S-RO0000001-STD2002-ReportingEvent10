# パッケージの読み込み
library(cards)

# 1. サンプルデータの確認（cardsパッケージに同梱されている臨床試験データ ADSL）
data("ADSL", package = "cards")

# 2. 連続変数の要約統計量（ARD）の作成
# 治療群（ARM）ごとに、年齢（AGE）の統計量を算出
ard_cont <- ard_continuous(
  data = ADSL,
  by = ARM,
  variables = AGE
)

# 結果の表示
print(ard_cont)

# 3. カテゴリ変数の要約統計量（ARD）の作成
# 治療群（ARM）ごとに、年齢層（AGEGR1）と性別（SEX）の例数・割合を算出
ard_cat <- ard_categorical(
  data = ADSL,
  by = ARM,
  variables = c(AGEGR1, SEX)
)

# 結果の表示
print(ard_cat)

# 4. 作成した複数のARDを1つに結合する
ard_combined <- bind_ard(
  ard_cont,
  ard_cat
)

# 結合結果の全体を表示
print(ard_combined)
