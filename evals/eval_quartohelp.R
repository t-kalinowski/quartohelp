library(vitals)
library(ellmer)
library(readr)
source("R/store.r")
source("R/client.R")

quarto_evaluation_suite <- read_csv(
  "evals/quarto_evaluation_suite.csv",
  col_types = cols(input = col_character(), target = col_character())
)

cat(
  "Loaded evaluation suite with",
  nrow(quarto_evaluation_suite),
  "test cases\n"
)

vitals::vitals_log_dir_set("./logs")

tsk <- Task$new(
  dataset = quarto_evaluation_suite,
  solver = generate(solver_chat = chat_quartohelp),
  scorer = model_graded_qa(
    scorer_chat = chat_openai(model = "gpt-5-nano-2025-08-07"),
    partial_credit = TRUE
  )
)

cat("Total test cases:", nrow(quarto_evaluation_suite), "\n\n")

tsk$eval(view = FALSE)

bundle_dir <- "./quarto_eval_bundle"
vitals_bundle(output_dir = bundle_dir, overwrite = TRUE)

cat("\n✅ Evaluation complete!\n")
cat("📦 Bundle created at: ", bundle_dir, "\n", sep = "")
