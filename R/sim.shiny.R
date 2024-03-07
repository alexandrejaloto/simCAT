#' @title CAT simulation in Shiny
#' @name sim.shiny
#'
#' @description CAT simulation in a Shiny application.
#'
#' @details
#' Uses `simCAT` function in a more friendly way. For now, this application
#' only supports simulation with dichotomous items and one replication.
#'
#' @return This function does not return a value. Instead, it
#' generates a Shiny application for interactive
#' Computerized Adaptive Testing simulations.
#'
#' @author Alexandre Jaloto
#'
#' @export

sim.shiny <- function()
{
  ui <- shiny::fluidPage(
    shiny::titlePanel("CAT simulation"),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Configuration",
        shiny::fluidRow(
          shiny::column(5, shiny::wellPanel(
            shiny::radioButtons("import_or_generate", "Choose the origin of data:",
                                choices = c("Upload", "Generate randomly"),
                                selected = "Upload"),

            shiny::conditionalPanel(
              condition = "input.import_or_generate == 'Upload'",
              shiny::fileInput("item_file", "Upload CSV file with item parameters", accept = ".csv"),
              shiny::fileInput("response_file", "Upload response bank", accept = ".csv")
            ),

            shiny::conditionalPanel(
              condition = "input.import_or_generate == 'Generate randomly'",
              shiny::numericInput("num_items", "Number of Items", value = 10),

              shiny::selectInput("disc_dist", "Distribution of discrimination",
                                 choices = c("Normal", "Lognormal")),
              shiny::numericInput("disc_mean", "Mean of discrimination", value = 0),
              shiny::numericInput("disc_sd", "Standard deviation of discrimination", value = 1),

              "Difficulty distribution: normal",
              shiny::numericInput("diff_mean", "Mean of difficulty", value = 0),
              shiny::numericInput("diff_sd", "Standard deviation of difficulty", value = 1),

              "Pseudoguessing distribution: beta",
              shiny::numericInput("guess_mean", "Shape1 parameter of guessing", value = 1),
              shiny::numericInput("guess_sd", "Shape2 parameter of guessing", value = 1),

              shiny::numericInput("num_subjects", "Number of subjects", value = 100),
              "Theta distribution: normal",
              shiny::numericInput("theta_mean", "Mean of theta", value = 0),
              shiny::numericInput("theta_sd", "Standard deviation of theta", value = 1)
            )
          )),
          shiny::column(5, shiny::wellPanel(
            shiny::h4("Options for simCAT function:"),
            shiny::selectInput("model", "Model", choices = c("3PL")),
            shiny::selectInput("sel_method", "Item Selection Method", choices = c("MFI", "Progressive", "Random")),
            shiny::conditionalPanel(
              condition = "input.sel_method == 'MFI'",
              shiny::numericInput("start_theta", "First Theta", value = 0)
            ),
            shiny::conditionalPanel(
              condition = "input.sel_method == 'Progressive'",
              shiny::selectInput("cat_type", "CAT Type", choices = c("variable", "fixed length")),
              shiny::numericInput("acceleration", "Acceleration", value = 1),
              shiny::numericInput("threshold", "Threshold", value = 0.3),
            ),
            shiny::numericInput("rmax", "Maximum Item Exposure Rate", value = 1),
            shiny::selectInput("met_content", "Content Balancing Method", choices = c("none", "MCCAT", "CCAT", "MMM")),
            shiny::conditionalPanel(
              condition = "input.met_content != 'none'",
              shiny::fileInput("content_file", "Upload CSV file with test contents and desired proportion", accept = ".csv"),
            ),
            shiny::h5('Stopping rules'),
            shiny::numericInput("se", "Minimum Standard Error", value = 0.7),
            shiny::numericInput("delta_theta", "Minimum Absolute Difference in Theta", value = 0.1),
            shiny::numericInput("hypo", "Minimum Standard Error Reduction", value = 0.01),
            shiny::numericInput("hyper", "Minimum Standard Error Reduction After Achieving SE", value = 0.02),
            shiny::numericInput("info", "Maximum Information of an Available Item", value = 1),
            shiny::numericInput("max_items", "Maximum Number of Items", value = 10),
            shiny::numericInput("min_items", "Minimum Number of Items", value = 5),
            shiny::numericInput("fixed", "Fixed Number of Items", value = 8)
          ))
        )
      ),

      shiny::tabPanel(
        "Simulation",
        shiny::mainPanel(
          shiny::actionButton("run_simulation", "Run simulation"),
          shiny::downloadButton("download_results", "Download results"),
          shiny::uiOutput("plot_with_spinner")
        )
      )
    )
  )

  server <- function(input, output, session) {

    shiny::observeEvent(input$item_file, {
      shiny::showModal(shiny::modalDialog(
        title = "Warning",
        "Values should be separated by commas, use a dot as the decimal
      separator, the first row should be the header, the first three
      columns should represent parameters a, b, and c, and the fourth
      column should contain the item content.",
        footer = shiny::tagList(
          shiny::actionButton("confirm_item_file", "OK")
        )
      ))
    })

    shiny::observeEvent(input$confirm_item_file, {
      shiny::removeModal()
    })

    shiny::observeEvent(input$response_file, {
      shiny::showModal(shiny::modalDialog(
        title = "Warning",
        "Values should be separated by commas, the first row should be the
      header, and the columns should contain the dichotomized responses
      (with the last column corresponding to the theta values of the
      subjects).",
        footer = shiny::tagList(
          shiny::actionButton("confirm_response_file", "OK")
        )
      ))
    })

    shiny::observeEvent(input$confirm_response_file, {
      shiny::removeModal()
    })

    shiny::observeEvent(input$content_file, {
      shiny::showModal(shiny::modalDialog(
        title = "Aviso",
        "Values should be separated by commas, the first row should be
      the header, the first column should correspond to the test content,
      and the second column should represent the proportion of each
      content in the test.",
        footer = shiny::tagList(
          shiny::actionButton("confirm_content_file", "OK")
        )
      ))
    })

    shiny::observeEvent(input$confirm_content_file, {
      shiny::removeModal()
    })

    shiny::observeEvent(input$run_simulation, {

      if (input$import_or_generate == "Upload") {

        if (!is.null(input$item_file$datapath) && !is.null(input$response_file$datapath)) {

          params <- utils::read.csv(input$item_file$datapath)
          resp.bank <- utils::read.csv(input$response_file$datapath)

          theta <- resp.bank[,ncol(resp.bank)]
          resps <- resp.bank[,-ncol(resp.bank)]

        } else {
          shiny::showModal(
            shiny::modalDialog(
              title = "Error",
              "Please select both files."
            )
          )
          return(NULL)
        }
      } else if (input$import_or_generate == "Generate randomly") {

        num_items <- input$num_items

        disc_param <- get_distribution(input$disc_dist, num_items, input$disc_mean, input$disc_sd)
        diff_param <- get_distribution('Normal', num_items, input$diff_mean, input$diff_sd)
        guess_param <- get_distribution('Beta', num_items, input$guess_mean, input$guess_sd)

        params <- data.frame(Discrimination = disc_param,
                             Difficulty = diff_param,
                             Guessing = guess_param)

        theta <- get_distribution('Normal', input$num_subjects, input$theta_mean, input$theta_sd)

        resps <- gen.resp(theta = theta, bank = params)
        names(resps) <- paste0('I', 1:ncol(resps))
      }

      output$plot_with_spinner <- shiny::renderUI({
        shinycssloaders::withSpinner(
          shiny::uiOutput("simulation_result"),
          type = 4
        )
      })

      output$simulation_result <- shiny::renderUI({

        sim_result <- (simCAT(
          resps = resps,
          bank = params[,1:3],
          start.theta = input$start_theta,
          sel.method = input$sel_method,
          cat.type = input$cat_type,
          acceleration = input$acceleration,
          threshold = input$threshold,
          stop = list(se = .7, max.items = 10),
          progress = FALSE
        ))

        cat.eval <- (cat.evaluation(
          results = sim_result,
          true.scores = theta,
          item.name = names(resps),
          rmax = 1
        ))

        cat_eval_table1 <- shiny::renderTable({
          cat.eval[[1]]
        }, rownames = TRUE)

        names(cat.eval[[2]]) <- round(as.numeric(names(cat.eval[[2]])),2)

        cat_eval_table2 <- shiny::renderTable({
          cat.eval[[2]]
        }, rownames = TRUE)

        output$download_results <- shiny::downloadHandler(
          filename = function() {
            "resultados_simulacao.rds"
          },
          content = function(file) {
            saveRDS(list(simulation = sim_result, evaluation = cat.eval), file)
          }
        )

        shiny::tagList(
          shiny::h3("Overall evaluation of the simulation"),
          cat_eval_table1,
          shiny::h3("Evaluation of the simulation conditioned on theta"),
          cat_eval_table2
        )
      })

    })
  }

  get_distribution <- function(dist, num_items, mean, sd) {
    if (dist == "Normal") {
      return(stats::rnorm(num_items, mean, sd))
    } else if (dist == "Lognormal") {
      return(stats::rlnorm(num_items, mean, sd))
    } else if (dist == "Beta") {
      return(stats::rbeta(num_items, mean, sd))
    } else {
      stop("Distribution not recognized.")
    }
  }

  shiny::shinyApp(ui, server)
}
