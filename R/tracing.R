#' Register Langfuse tracing callbacks on a Chat object
#'
#' Adds OpenTelemetry tracing to an ellmer::Chat object using tool callbacks.
#' This creates spans with LLM-specific attributes that Langfuse displays as
#' "generations".
#'
#' @param chat An ellmer::Chat object
#' @return The chat object (invisibly), modified with tracing callbacks
#' @keywords internal
register_langfuse_tracing <- function(chat) {
  if (!inherits(chat, "Chat")) {
    stop("`chat` must inherit from 'Chat'.", call. = FALSE)
  }

  get_model_name <- function(obj) {
    tryCatch(
      obj$.__enclos_env__$private$provider$model,
      error = function(e) "gpt-5.1-2025-11-13"
    ) %||% "gpt-5.1-2025-11-13"
  }

  model_name <- get_model_name(chat)

  # Create an environment to store span state across callbacks
  trace_env <- new.env(parent = emptyenv())
  trace_env$current_span <- NULL

  # Register callback for tool requests
  chat$on_tool_request(function(request) {
    # Start a span for the tool call
    span <- otel::start_span(
      name = "llm.tool_request",
      attributes = list(
        model = model_name,
        `gen_ai.request.model` = model_name,
        `gen_ai.system` = "openai",
        `tool.name` = request$name %||% "unknown"
      )
    )
    trace_env$current_span <- span
  })

  # Register callback for tool results
  chat$on_tool_result(function(result) {
    if (!is.null(trace_env$current_span)) {
      otel::end_span(trace_env$current_span)
      trace_env$current_span <- NULL
    }
  })

  invisible(chat)
}

#' Set up conversation tracing for a shinychat module
#'
#' Observes user inputs and assistant responses from a shinychat module
#' and creates OpenTelemetry spans for Langfuse.
#'
#' @param module The module object returned by shinychat::chat_mod_server()
#' @param session The Shiny session object
#' @return NULL (called for side effects)
#' @keywords internal
setup_conversation_tracing <- function(module, session) {
  trace_env <- new.env(parent = emptyenv())
  trace_env$last_input <- NULL

  get_model_name <- function(obj) {
    tryCatch(
      obj$.__enclos_env__$private$provider$model,
      error = function(e) "gpt-4o"
    ) %||% "gpt-4o"
  }

  safe_text <- function(turn) {
    tryCatch(
      {
        if (is.character(turn)) {
          turn
        } else if (inherits(turn, "Turn") || inherits(turn, "S7_object")) {
          txt <- tryCatch(turn@text, error = function(e) NULL)
          if (is.null(txt) || !nzchar(txt)) {
            txt <- paste(utils::capture.output(print(turn)), collapse = "\n")
          }
          txt
        } else {
          paste(utils::capture.output(print(turn)), collapse = "\n")
        }
      },
      error = function(e) "<response>"
    )
  }

  model_name <- get_model_name(module$client)

  shiny::observeEvent(
    module$last_input(),
    {
      input_text <- module$last_input()
      if (!is.null(input_text) && nzchar(input_text)) {
        trace_env$last_input <- input_text
      }
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    module$last_turn(),
    {
      turn <- module$last_turn()
      if (is.null(turn)) {
        return()
      }

      response_text <- safe_text(turn)

      input_text <- trace_env$last_input %||% "<user question>"

      span <- otel::start_span(
        name = "llm.conversation",
        attributes = list(
          model = model_name,
          `gen_ai.request.model` = model_name,
          `gen_ai.system` = "openai",
          `gen_ai.prompt` = input_text,
          `gen_ai.completion` = response_text,
          input = input_text,
          output = response_text
        )
      )
      otel::end_span(span)

      trace_env$last_input <- NULL
    },
    ignoreInit = TRUE
  )

  invisible(NULL)
}
