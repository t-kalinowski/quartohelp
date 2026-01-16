# Keeps track of whether we've mounted the app's static assets
.register_assets <- local({
  added <- FALSE
  function() {
    if (added) {
      return(invisible(NULL))
    }
    dir <- system.file("quartohelp-app", "www", package = "quartohelp")
    if (!nzchar(dir) || !dir.exists(dir)) {
      warning(
        "quartohelp assets directory not found; app resources may be missing.",
        call. = FALSE
      )
      return(invisible(NULL))
    }
    shiny::addResourcePath("quartohelp", dir)
    added <<- TRUE
    invisible(NULL)
  }
})

chat_panel_id <- function(index) {
  paste0("chat_panel_", index)
}

chat_panel_container_id <- function(module_id) {
  paste0(module_id, "_container")
}

chat_panel_ui <- function(module_id) {
  shiny::div(
    id = chat_panel_container_id(module_id),
    class = "h-100 d-flex flex-column",
    shinychat::chat_mod_ui(
      module_id,
      height = "100%",
      fill = TRUE
    )
  )
}

#' Internal UI for the Quarto Help app
#' @noRd
quartohelp_app_ui <- function() {
  .register_assets()
  hosted <- Sys.getenv("QUARTOHELP_HOSTED", "FALSE") %in% c("TRUE", "1", "true")

  bslib::page_fillable(
    title = "Quarto Help",
    theme = bslib::bs_theme(version = 5),
    shiny::tags$head(
      shiny::singleton(
        shiny::tags$link(
          rel = "icon",
          type = "image/png",
          href = "quartohelp/favicon.png"
        )
      ),
      shiny::singleton(
        shiny::tags$script(src = "quartohelp/quartohelp-app.js")
      )
    ),
    shiny::tagList(
      shiny::tags$style(
        shiny::HTML(
          "
          .w-40 { width: 40%; }

          .split-resizer { width: 6px; cursor: col-resize; background: transparent; position: relative; }
          .split-resizer::after { content: ''; position: absolute; top: 50%; height: 30px; left: 2px; width: 2px; background: var(--bs-border-color, #dee2e6); }

          #toggle-chat-show { display: none; }

          .collapsed-left #toggle-chat-show { display: inline-flex; }
          .collapsed-left .left-pane, .collapsed-left .split-resizer { display: none !important; }
          .collapsed-left .split-reveal { display: flex !important; }
          .collapsed-left .right-pane { flex: 1 1 auto; }

          .resizing iframe { pointer-events: none !important; }
          .resizing, .resizing * { cursor: col-resize !important; }
          "
        )
      ),
      shiny::div(
        class = "h-100 d-flex flex-column gap-2",
        shiny::div(
          class = "content-split d-flex flex-row flex-grow-1 w-100 gap-1",
          style = "min-height: 0;",
          shiny::div(
            class = "left-pane w-40 flex-grow-1 flex-sm-grow-0",
            bslib::card(
              class = "h-100 d-flex flex-column",
              bslib::card_header(
                shiny::div(
                  class = "d-flex align-items-center justify-content-between gap-2",
                  shiny::tags$span("Chat"),
                  shiny::div(
                    class = "btn-group btn-group-sm",
                    shiny::actionButton(
                      inputId = "clear_chat",
                      label = "Clear",
                      icon = shiny::icon("broom"),
                      class = "btn-outline-secondary"
                    ),
                    shiny::tags$button(
                      id = "toggle-chat",
                      type = "button",
                      class = "btn btn-sm btn-outline-secondary d-none d-sm-block",
                      title = "Collapse chat",
                      `aria-label` = "Collapse chat",
                      shiny::icon("chevron-left")
                    )
                  )
                )
              ),
              bslib::card_body(
                class = "flex flex-column gap-0 overflow-scroll",
                if (hosted) {
                  shiny::div(
                    class = "alert alert-warning alert-dismissible fade show",
                    role = "alert",
                    shiny::tags$button(
                      type = "button",
                      class = "btn-close",
                      `data-bs-dismiss` = "alert",
                      `aria-label` = "Close"
                    ),
                    shiny::p(
                      class = "mb-1 small text-muted",
                      shiny::tags$strong("© 2025 Posit Software, PBC."),
                      " Quarto Help activity may be reviewed to monitor abuse and improve the service. Please avoid sharing personal, sensitive, or confidential information."
                    ),
                    shiny::p(
                      class = "m-0 small text-muted",
                      "By using this app you agree to Posit's ",
                      shiny::tags$a(
                        href = "https://posit.co/about/posit-service-terms-of-use/",
                        target = "_blank",
                        style = "color: #888; text-decoration: none;",
                        "Terms & Conditions"
                      ),
                      " and ",
                      shiny::tags$a(
                        href = "https://posit.co/about/privacy-policy/",
                        target = "_blank",
                        style = "color: #888; text-decoration: none;",
                        "Privacy Policy"
                      ),
                      "."
                    )
                  )
                },
                shiny::div(
                  id = "chat-pane",
                  chat_panel_ui(chat_panel_id(1L))
                )
              )
            )
          ),
          shiny::div(
            id = "split-resizer",
            class = "split-resizer d-none d-sm-block"
          ),
          shiny::div(
            class = "right-pane flex-grow-0 flex-sm-grow-1 d-none d-sm-block",
            bslib::card(
              class = "h-100 d-flex flex-column",
              bslib::card_header(
                shiny::div(
                  class = "d-flex align-items-center justify-content-between gap-3",
                  shiny::div(
                    class = "d-flex align-items-center gap-2",
                    shiny::tags$button(
                      id = "toggle-chat-show",
                      type = "button",
                      class = "btn btn-sm btn-outline-secondary",
                      title = "Show chat",
                      `aria-label` = "Show chat",
                      shiny::icon("chevron-right")
                    ),
                    shiny::tags$button(
                      id = "iframe-back",
                      type = "button",
                      class = "btn btn-sm btn-secondary",
                      title = "Back",
                      `aria-label` = "Back",
                      shiny::HTML("&#8592;")
                    ),
                    shiny::tags$button(
                      id = "iframe-forward",
                      type = "button",
                      class = "btn btn-sm btn-secondary",
                      title = "Forward",
                      `aria-label` = "Forward",
                      shiny::HTML("&#8594;")
                    ),
                    shiny::tags$span("Docs")
                  ),
                  shiny::div(
                    class = "d-flex align-items-center gap-3",
                    shiny::tags$button(
                      id = "open-preview-external",
                      type = "button",
                      class = "btn btn-sm btn-outline-secondary",
                      title = "Open in new tab",
                      `aria-label` = "Open in new tab",
                      shiny::icon("external-link-alt")
                    )
                  )
                )
              ),
              bslib::card_body(
                class = "p-0",
                shiny::div(
                  id = "iframe-container",
                  class = "iframe-wrap flex-grow-1",
                  shiny::p(
                    id = "iframe-placeholder",
                    "Loading documentation..."
                  ),
                  shiny::tags$iframe(
                    id = "content-iframe",
                    name = "content-iframe",
                    src = "https://quarto.org",
                    style = "width:100%; height:100%; border:0;"
                  )
                )
              )
            )
          )
        ),
        if (hosted) {
          shiny::span(
            class = "align-middle",
            shiny::p(
              class = "m-0",
              "Built with ",
              shiny::tags$a(
                href = "https://shiny.posit.co",
                target = "_blank",
                "Shiny"
              ),
              ", ",
              shiny::tags$a(
                href = "http://ragnar.tidyverse.org",
                target = "_blank",
                "ragnar"
              ),
              " and ",
              shiny::tags$a(
                href = "http://ellmer.tidyverse.org",
                target = "_blank",
                "ellmer"
              ),
              ", hosted in ",
              shiny::tags$a(
                href = "https://connect.posit.cloud",
                target = "_blank",
                "Posit Connect Cloud"
              )
            )
          )
        }
      )
    )
  )
}

#' Internal server for the Quarto Help app
#' @noRd
quartohelp_app_server <- function(
  initial_chat = NULL,
  new_chat = chat_quartohelp,
  initial_question = NULL
) {
  if (!is.null(initial_chat) && !inherits(initial_chat, "Chat")) {
    stop("`initial_chat` must inherit from 'Chat'.", call. = FALSE)
  }
  if (!is.function(new_chat)) {
    stop("`new_chat` must be a function that returns a 'Chat'.", call. = FALSE)
  }
  force(new_chat)

  if (!is.null(initial_chat)) {
    new_chat <- local({
      supplied_new_chat <- new_chat
      function() {
        out <- initial_chat
        initial_chat <<- NULL
        new_chat <<- supplied_new_chat
        out
      }
    })
  }

  function(input, output, session) {
    chat <- shiny::reactiveVal(new_chat())
    chat_gen <- shiny::reactiveVal(1L)
    pending_question <- shiny::reactiveVal(initial_question)
    active_module_id <- shiny::reactiveVal(chat_panel_id(1L))

    swap_chat_ui <- function(module_id) {
      shiny::insertUI(
        selector = "#chat-pane",
        where = "beforeEnd",
        ui = chat_panel_ui(module_id),
        session = session,
        immediate = TRUE
      )

      previous_id <- shiny::isolate(active_module_id())
      if (!is.null(previous_id) && !identical(previous_id, module_id)) {
        shiny::removeUI(
          selector = paste0("#", chat_panel_container_id(previous_id)),
          session = session,
          immediate = TRUE
        )
      }

      active_module_id(module_id)
    }

    shiny::observeEvent(
      chat_gen(),
      {
        module_id <- chat_panel_id(chat_gen())
        if (chat_gen() > 1L) {
          swap_chat_ui(module_id)
        }
        module <- shinychat::chat_mod_server(module_id, chat())

        # Set up Langfuse tracing for conversation if enabled
        if (otel::is_tracing_enabled()) {
          setup_conversation_tracing(module, session)
        }

        question <- pending_question()
        if (!is.null(question)) {
          pending_question(NULL)
          session$onFlushed(
            function() {
              module$update_user_input(value = question, submit = TRUE)
            },
            once = TRUE
          )
        }
      },
      ignoreNULL = FALSE
    )

    shiny::observeEvent(input$clear_chat, {
      # The ragnar retrieve tool is stateful; module$clear() only resets UI history
      # and leaves the old client (with its registered tools) in place. Instead we
      # swap in a fresh client and new module id so the tool is reinitialized.
      chat(new_chat())
      pending_question(NULL)
      chat_gen(shiny::isolate(chat_gen()) + 1L)
    })
  }
}
