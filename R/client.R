#' Configure a chat object for Quarto Help
#'
#' Attaches the Quarto knowledge store retrieval tool and system prompt to a
#' chat instance. By default this creates a fresh OpenAI chat via
#' [ellmer::chat_openai()] and registers
#' [ragnar::ragnar_register_tool_retrieve] so that every response can cite
#' relevant Quarto documentation.
#'
#' @param proto An `ellmer::Chat` object to configure. When `NULL`, a default OpenAI
#'   chat is created. You can also set a global prototype via the
#'   `quartohelp.proto_chat` option, which may be either a function returning a chat or
#'   an existing `ellmer::Chat` instance.
#' @param top_k Number of excerpts to request from the knowledge store for each
#'   retrieval.
#' @param store A `ragnar::RagnarStore` object (for example, from
#'   `quartohelp_ragnar_store()`).
#' @return The configured `chat` object.
#' @keywords internal
as_quartohelp_chat <- function(
  proto = getOption("quartohelp.proto_chat"),
  top_k = 8,
  store = quartohelp_ragnar_store()
) {
  if (inherits(proto, "Chat")) {
    chat <- proto
  } else if (is.null(proto)) {
    hosted <-
      Sys.getenv("QUARTOHELP_HOSTED", "FALSE") %in% c("TRUE", "1", "true")
    chat <- ellmer::chat_openai(
      model = "gpt-5.2",
      api_args = if (hosted) list(store = TRUE) else list(),
      params = ellmer::params(
        # # gpt-5.1 default reasoning effort is 'none'
        # reasoning_effort = "low"
        text = list(verbosity = "low")
      ),
      echo = "none"
    )
  } else if (is.function(proto)) {
    chat <- proto()
    if (!inherits(chat, "Chat")) {
      stop(
        "`proto()` must return an object that inherits from 'Chat'.",
        call. = FALSE
      )
    }
  } else {
    stop(
      "`proto` must be NULL, a function returning a 'Chat', or a 'Chat' object.",
      call. = FALSE
    )
  }

  stopifnot(inherits(chat, "Chat"))
  quartohelp_prompt <- glue::trim(
    "
    You are an expert in Quarto documentation. You are concise.
    Always perform a search of the Quarto knowledge store for each user request.
    Every response must include links to official documentation sources.
    If the request is ambiguous, search first, then ask a clarifying question.
    If docs are unavailable, or if search fails, or if docs do not contain an answer
    to the question, inform the user and do NOT answer the question.
    Avoid calling the search tool more than once per user turn.

    Always give answers that include a minimal fully self-contained quarto document.

    To display Quarto code blocks, use oversized markdown fences, like this:

    ````` markdown
    PROSE HERE
    ```{r}
    CODE HERE
    ```
    ```{python}
    CODE HERE
    ```
    `````
    "
  )

  existing_prompt <- chat$get_system_prompt()

  if (is.null(existing_prompt)) {
    chat$set_system_prompt(quartohelp_prompt)
  } else if (!any(grepl(quartohelp_prompt, existing_prompt, fixed = TRUE))) {
    combined <- paste(quartohelp_prompt, existing_prompt, sep = "\n\n")
    chat$set_system_prompt(combined)
  }

  ragnar::ragnar_register_tool_retrieve(chat, store, top_k = top_k)

  # Register OpenTelemetry tracing callbacks for Langfuse if tracing is enabled
  if (otel::is_tracing_enabled()) {
    register_langfuse_tracing(chat)
  }

  chat
}

#' Create a Quarto Help chat
#'
#' This is a convenience wrapper around [as_quartohelp_chat()] that returns a fresh
#' `ellmer::Chat` instance configured with the Quarto knowledge store.
#'
#' @inheritParams as_quartohelp_chat
#' @return A configured `ellmer::Chat` object.
#' @export
chat_quartohelp <- function(
  top_k = 8,
  store = quartohelp_ragnar_store(),
  proto = NULL
) {
  as_quartohelp_chat(
    proto = proto,
    top_k = top_k,
    store = store
  )
}
