#' Register Langfuse tracing callbacks on a Chat object
#'
#' Adds OpenTelemetry tracing to an ellmer::Chat object using tool callbacks.
#' This creates spans with LLM-specific attributes that Langfuse displays as
#' "generations".
#'
#' @param chat An ellmer::Chat object
#' @return The chat object, modified with tracing callbacks
#' @keywords internal
register_langfuse_tracing <- function(chat) {
  if (!inherits(chat, "Chat")) {
    stop("`chat` must inherit from 'Chat'.", call. = FALSE)
  }

  # Get model name from chat object
  model_name <- chat$get_model()
  provider_name <- chat$get_provider()@name

  # Create an environment to store span state across callbacks
  trace_env <- new.env(parent = emptyenv())
  trace_env$current_span <- NULL

  # Register callback for tool requests
  chat$on_tool_request(function(request) {
    # Serialize tool arguments to JSON
    args_json <- tryCatch(
      jsonlite::toJSON(request@arguments, auto_unbox = TRUE, pretty = FALSE),
      error = function(e) paste0("<error serializing args>: ", e$message)
    )

    # Start a span for the tool call
    span <- otel::start_span(
      name = "llm.tool_request",
      attributes = list(
        model = model_name,
        `gen_ai.request.model` = model_name,
        `gen_ai.system` = provider_name,
        `tool.name` = request@name %||% "unknown",
        `tool.call.id` = request@id %||% "",
        `tool.arguments` = as.character(args_json)
      )
    )
    trace_env$current_span <- span
  })

  # Register callback for tool results
  chat$on_tool_result(function(result) {
    if (!is.null(trace_env$current_span)) {
      # Serialize tool result value
      result_text <- tryCatch(
        {
          val <- result@value
          if (is.character(val)) {
            paste0(val, collapse = "\n")
          } else {
            as.character(
              jsonlite::toJSON(val, auto_unbox = TRUE, pretty = TRUE)
            )
          }
        },
        error = function(e) {
          tryCatch(
            paste0(utils::capture.output(print(result@value)), collapse = "\n"),
            error = function(e2) paste0("<error serializing result>: ", e2$message)
          )
        }
      )

      # Record any error
      error_text <- tryCatch(
        {
          err <- result@error
          if (is.null(err)) {
            ""
          } else if (is.character(err)) {
            err
          } else if (inherits(err, "condition")) {
            conditionMessage(err)
          } else {
            as.character(err)
          }
        },
        error = function(e) ""
      )

      trace_env$current_span$set_attribute("tool.result", result_text)
      trace_env$current_span$set_attribute("tool.error", error_text)

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

  model_name <- module$client$get_model()
  provider_name <- module$client$get_provider()@name

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

      response_text <- tryCatch(
        {
          txt <- if (is.character(turn)) {
            turn
          } else if (inherits(turn, ellmer::Turn)) {
            turn@text
          } else {
            utils::capture.output(print(turn))
          }
          paste0(txt, collapse = "\n")
        },
        error = function(e) {
          paste0("<error generating response>: ", conditionMessage(e))
        }
      )

      input_text <- trace_env$last_input %||% "<user question>"

      span <- otel::start_span(
        name = "llm.conversation",
        attributes = list(
          model = model_name,
          gen_ai.request.model = model_name,
          gen_ai.system = provider_name,
          gen_ai.prompt = input_text,
          gen_ai.completion = response_text,
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
