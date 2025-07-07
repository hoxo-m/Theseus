#' @import dplyr ggplot2 stringr
#'
#' @export
ShipOfTheseus <- R6::R6Class(
  "ShipOfTheseus",

  private = list(
    labels = NULL,
    compute_scores = NULL,
    compute_contribution = NULL,
    compute_info = NULL,
    compute_size = NULL
  ),

  public = list(

    initialize = function(data1, data2, labels = c("Original", "Refitted")) {
      data1 <- data1 |>
        mutate_if(~ is.character(.x) | is.factor(.x), ~ forcats::fct_na_value_to_level(.x, level = "(Missing)"))
      data2 <- data2 |>
        mutate_if(~ is.character(.x) | is.factor(.x), ~ forcats::fct_na_value_to_level(.x, level = "(Missing)"))

      private$labels <- labels

      private$compute_scores <- memoise::memoise(function(column_name) {
        score1 <- data1 |> summarise(score = mean(y)) |> pull(score)
        score2 <- data2 |> summarise(score = mean(y)) |> pull(score)
        c(score1, score2)
      })

      private$compute_contribution <- memoise::memoise(function(column_name) {
        df1 <- data1 |>
          group_by(!!rlang::sym(column_name)) |>
          summarise(y = sum(y), n = n(), rate = y / n)
        df2 <- data2 |>
          group_by(!!rlang::sym(column_name)) |>
          summarise(y = sum(y), n = n(), rate = y / n)

        names1 <- df1[[column_name]]
        names2 <- df2[[column_name]]

        scores <- private$compute_scores(column_name)
        score1 <- scores[1]
        score2 <- scores[2]

        result <- tibble::tibble()
        for (name in names2) {
          df_temp <- df1
          if (name %in% names1) {
            df_temp[df_temp[[column_name]] == name, ] <- df2[df2[[column_name]] == name, ]
          } else {
            df_temp <- rbind(df_temp, df2[df2[[column_name]] == name, ])
          }

          score_new <- df_temp |> summarise(score = sum(y) / sum(n)) |> pull(score)
          diff <- score_new - score1
          res <- tibble::tibble(items = name, amount = diff)
          result <- rbind(result, res)
        }
        for (name in names1) {
          df_temp <- df2
          if (name %in% names2) {
            df_temp[df_temp[[column_name]] == name, ] <- df1[df1[[column_name]] == name, ]
          } else {
            df_temp <- rbind(df_temp, df1[df1[[column_name]] == name, ])
          }

          score_new <- df_temp |> summarise(score = sum(y) / sum(n)) |> pull(score)
          diff <- score2 - score_new
          res <- tibble::tibble(items = name, amount = diff)
          result <- rbind(result, res)
        }

        result |>
          group_by(items) |>
          summarise(contrib = mean(amount)) |>
          mutate(contrib = (score2 - score1) * contrib / sum(contrib))
      })

      private$compute_info <- memoise::memoise(function(column_name) {
        data1_info <- data1 |>
          group_by(items = !!rlang::sym(column_name)) |>
          summarise(n1 = n(), x1 = sum(y), rate1 = x1 / n1)
        data2_info <- data2 |>
          group_by(items = !!rlang::sym(column_name)) |>
          summarise(n2 = n(), x2 = sum(y), rate2 = x2 / n2)
        data1_info |> full_join(data2_info, by = "items") |>
          select(items, starts_with("n"), starts_with("x"), starts_with("rate")) |>
          tidyr::replace_na(list(n1 = 0L, n2 = 0L, x1 = 0L, x2 = 0L))
      })

      private$compute_size <- memoise::memoise(function(column_name) {
        data1_size <- data1 |>
          count(items = !!rlang::sym(column_name)) |>
          mutate(type = labels[1])
        data2_size <- data2 |>
          count(items = !!rlang::sym(column_name)) |>
          mutate(type = labels[2])
        rbind(data1_size, data2_size)
      })

    },

    table = function(column_name) {
      column_name <- rlang::ensym(column_name) |> rlang::as_string()
      data_contrib <- private$compute_contribution(column_name)
      data_info <- private$compute_info(column_name)
      result <- data_contrib |>
        left_join(data_info, by = "items") |>
        arrange(desc(abs(contrib)))
      names(result)[1] <- column_name
      result
    },

    plot = function(column_name, main_item = NULL, bar_max_value = NULL,
                    levels = NULL) {
      column_name <- rlang::ensym(column_name) |> rlang::as_string()

      labels <- private$labels

      score1 <- private$compute_scores(column_name)[1]
      data_size <- private$compute_size(column_name)

      result <- private$compute_contribution(column_name) |> arrange(contrib)

      if (!is.null(levels)) {
        levels <- as.character(levels)
        result <- data.frame(items = levels) |> inner_join(result, by = "items")
      }
      names <- result$items
      result <- tibble::tibble(items = labels[1], contrib = score1) |>
        rbind(result)|>
        mutate(contrib = round(contrib * 100, digits = 3L))

      p <- waterfalls::waterfall(
        result, calc_total = TRUE, total_axis_text = labels[2],
        total_rect_text_color = "black", total_rect_color = "#00BFC4")

      if (is.null(main_item) & is.null(bar_max_value)) {
        data_max <- result |> tail(-1) |> filter(abs(contrib) == max(abs(contrib)))
        max_item <- data_max |> pull(items)
        max_amount <- data_max |> pull(contrib) |> abs()
        n_max <- data_size |> filter(items == max_item) |> pull(n) |> max()
      } else if(!is.null(main_item)) {
        max_amount <- result |> filter(items == main_item) |> pull(contrib) |> abs()
        n_max <- data_size |> filter(items == main_item) |> pull(n) |> max()
      } else if(!is.null(bar_max_value)) {
        max_amount <- bar_max_value
        n_max <- data_size |> filter(n == max(n)) |> pull(n) |> max()
      }

      levels <- c(labels[1], names, labels[2])
      data_size <- p$data |> select(x) |> distinct() |>
        left_join(data_size, by = c(x = "items")) |>
        tidyr::replace_na(list(n = 0L)) |>
        mutate(x = factor(x, levels = levels), type = factor(type, levels = labels)) |>
        mutate(n = n / n_max * max_amount)

      p <- p +
        geom_col(data = data_size, aes(x, n, fill = type), width = 0.7, position = position_dodge()) +
        scale_fill_manual(values = c("#7CAE00", "#C77CFF"), guide = "none")
      p$layers <- append(head(p$layers, -1), tail(p$layers, 1), 1)
      p
    },

    plot_flip = function(column_name, main_item = NULL, bar_max_value = NULL,
                         levels = NULL) {
      column_name <- rlang::ensym(column_name) |> rlang::as_string()

      labels <- private$labels

      score2 <- private$compute_scores(column_name)[2]
      data_size <- private$compute_size(column_name)

      result <- private$compute_contribution(column_name) |>
        mutate(contrib = -contrib) |> arrange(contrib)

      if (!is.null(levels)) {
        levels <- as.character(levels) |> rev()
        result <- data.frame(items = levels) |> inner_join(result, by = "items")
      }
      names <- result$items
      result <- tibble::tibble(items = labels[2], contrib = score2) |>
        rbind(result)|>
        mutate(contrib = round(contrib * 100, digits = 3L))

      colors <- if_else(result$contrib > 0, "#F8766D", "#00BFC4")
      colors[1] <- "#00BFC4"
      p <- waterfalls::waterfall(
        result, calc_total = TRUE, total_axis_text = labels[1],
        total_rect_text_color = "black", fill_colours = colors,
        fill_by_sign = FALSE, total_rect_color = "#00BFC4") +
        coord_flip()

      reverse_sign <- function(x) {
        x <- str_replace(x, "\u2212", "-")
        x <- -as.numeric(x)
        x <- as.character(x)
        str_replace(x, "-", "\u2212")
      }

      is_after <- TRUE
      for (i in seq_along(p$layers |> head(-2))) {
        if ("GeomText" %in% class(p$layers[[i]]$geom)) {
          if (is_after) {
            is_after <- FALSE
            next
          }
          p$layers[[i]]$aes_params$label <- reverse_sign(p$layers[[i]]$aes_params$label)
        }
      }

      if (is.null(main_item) & is.null(bar_max_value)) {
        data_max <- result |> tail(-1) |> filter(abs(contrib) == max(abs(contrib)))
        max_item <- data_max |> pull(items)
        max_amount <- data_max |> pull(contrib) |> abs()
        n_max <- data_size |> filter(items == max_item) |> pull(n) |> max()
      } else if(!is.null(main_item)) {
        max_amount <- result |> filter(items == main_item) |> pull(contrib) |> abs()
        n_max <- data_size |> filter(items == main_item) |> pull(n) |> max()
      } else if(!is.null(bar_max_value)) {
        max_amount <- bar_max_value
        n_max <- data_size |> filter(n == max(n)) |> pull(n) |> max()
      }

      levels <- c(labels[2], names, labels[1])
      data_size <- p$data |> select(x) |> distinct() |>
        left_join(data_size, by = c(x = "items")) |>
        tidyr::replace_na(list(n = 0L)) |>
        mutate(x = factor(x, levels = levels), type = factor(type, levels = rev(labels))) |>
        mutate(n = n / n_max * max_amount)

      p <- p +
        geom_col(data = data_size, aes(x, n, fill = type), width = 0.7, position = position_dodge()) +
        scale_fill_manual(values = c("#C77CFF", "#7CAE00"), guide = "none")
      p$layers <- append(head(p$layers, -1), tail(p$layers, 1), 1)
      p
    },

    overhaul = function() {
      data1 <- private$data1 |>
        select_if(~ is.character(.x) | is.factor(.x))
      data2 <- private$data2 |>
        select_if(~ is.character(.x) | is.factor(.x))

      vars1 <- names(data1)
      vars2 <- names(data2)

      vars <- intersect(vars1, vars2)

      result <- tibble::tibble()
      pb <- txtProgressBar(max = length(vars), style = 3L)
      for (i in seq_along(vars)) {
        var <- vars[i]
        if (var == "y") next
        t <- ship$table(!!rlang::sym(var)) |>
          mutate(score = abs(mean / (size1 + size2))) |>
          arrange(desc(score))
        res <- t |> head(1) |> mutate(var = var) |> select(var, everything())
        result <- rbind(result, res)
        setTxtProgressBar(pb, i)
      }
      close(pb)

      result |> arrange(desc(score))
    }
  )
)
