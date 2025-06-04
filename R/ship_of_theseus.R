#' @import dplyr ggplot2 stringr
#'
#' @export
ShipOfTheseus <- R6::R6Class(
  "ShipOfTheseus",

  private = list(
    data1 = NULL,
    data2 = NULL,
    labels = NULL
  ),

  public = list(
    initialize = function(data1, data2, labels = c("Original", "Refitted")) {
      private$data1 <- data1
      private$data2 <- data2
      private$labels <- labels
    },

    table = function(target_col) {
      target_col <- rlang::ensym(target_col) |> rlang::as_string()

      data1 <- private$data1
      data2 <- private$data2

      df1 <- data1 |>
        group_by(!!rlang::sym(target_col)) |>
        summarise(y = sum(y), n = n(), rate = y / n)
      df2 <- data2 |>
        group_by(!!rlang::sym(target_col)) |>
        summarise(y = sum(y), n = n(), rate = y / n)

      names1 <- df1[[target_col]]
      names2 <- df2[[target_col]]

      score1 <- data1 |> summarise(score = mean(y)) |> pull(score)
      score2 <- data2 |> summarise(score = mean(y)) |> pull(score)

      result <- tibble::tibble()
      for (name in names2) {
        df_temp <- df1
        if (name %in% names1) {
          df_temp[df_temp[[target_col]] == name, ] <- df2[df2[[target_col]] == name, ]
        } else {
          df_temp <- rbind(df_temp, df2[df2[[target_col]] == name, ])
        }

        score_new <- df_temp |> summarise(score = sum(y) / sum(n)) |> pull(score)
        diff <- score_new - score1
        res <- tibble::tibble(items = name, amount = diff)
        result <- rbind(result, res)
      }
      for (name in names1) {
        df_temp <- df2
        if (name %in% names2) {
          df_temp[df_temp[[target_col]] == name, ] <- df1[df1[[target_col]] == name, ]
        } else {
          df_temp <- rbind(df_temp, df1[df1[[target_col]] == name, ])
        }

        score_new <- df_temp |> summarise(score = sum(y) / sum(n)) |> pull(score)
        diff <- score2 - score_new
        res <- tibble::tibble(items = name, amount = diff)
        result <- rbind(result, res)
      }

      data1_size <- data1 |> count(items = !!rlang::sym(target_col), name = "size1")
      data2_size <- data2 |> count(items = !!rlang::sym(target_col), name = "size2")
      data_size <- data1_size |> full_join(data2_size, by = "items") |>
        tidyr::replace_na(list(size1 = 0L, size2 = 0L))

      result |>
        group_by(items) |>
        summarise(mean = mean(amount), min = min(amount), max = max(amount)) |>
        left_join(data_size, by = "items") |>
        arrange(desc(abs(mean)))
    },

    plot = function(target_col, main_item = NULL) {
      target_col <- rlang::ensym(target_col) |> rlang::as_string()

      data1 <- private$data1
      data2 <- private$data2
      labels <- private$labels

      df1 <- data1 |>
        group_by(!!rlang::sym(target_col)) |>
        summarise(y = sum(y), n = n(), rate = y / n)
      df2 <- data2 |>
        group_by(!!rlang::sym(target_col)) |>
        summarise(y = sum(y), n = n(), rate = y / n)

      names1 <- df1[[target_col]]
      names2 <- df2[[target_col]]

      score1 <- data1 |> summarise(score = mean(y)) |> pull(score)
      score2 <- data2 |> summarise(score = mean(y)) |> pull(score)

      result <- tibble::tibble()
      for (name in names2) {
        df_temp <- df1
        if (name %in% names1) {
          df_temp[df_temp[[target_col]] == name, ] <- df2[df2[[target_col]] == name, ]
        } else {
          df_temp <- rbind(df_temp, df2[df2[[target_col]] == name, ])
        }

        score_new <- df_temp |> summarise(score = sum(y) / sum(n)) |> pull(score)
        diff <- score_new - score1
        res <- tibble::tibble(items = name, amount = diff)
        result <- rbind(result, res)
      }
      for (name in names1) {
        df_temp <- df2
        if (name %in% names2) {
          df_temp[df_temp[[target_col]] == name, ] <- df1[df1[[target_col]] == name, ]
        } else {
          df_temp <- rbind(df_temp, df1[df1[[target_col]] == name, ])
        }

        score_new <- df_temp |> summarise(score = sum(y) / sum(n)) |> pull(score)
        diff <- score2 - score_new
        res <- tibble::tibble(items = name, amount = diff)
        result <- rbind(result, res)
      }

      data1_size <- data1 |> count(items = !!rlang::sym(target_col)) |> mutate(type = labels[1])
      data2_size <- data2 |> count(items = !!rlang::sym(target_col)) |> mutate(type = labels[2])
      data_size <- rbind(data1_size, data2_size)

      result <- result |>
        group_by(items) |>
        summarise(amount = mean(amount)) |>
        arrange(amount)
      names <- result$items
      result <- tibble::tibble(items = labels[1], amount = score1) |>
        rbind(result)|>
        mutate(amount = round(amount * 100, digits = 3L))

      p <- waterfalls::waterfall(
        result, calc_total = TRUE, total_axis_text = labels[2],
        total_rect_text_color = "black", total_rect_color = "#00BFC4")

      if (is.null(main_item)) {
        data_max <- result |> tail(-1) |> filter(abs(amount) == max(abs(amount)))
        max_item <- data_max |> pull(items)
        max_amount <- data_max |> pull(amount) |> abs()
        n_max <- data_size |> filter(items == max_item) |> pull(n) |> max()
      } else {
        max_amount <- result |> filter(items == main_item) |> pull(amount) |> abs()
        n_max <- data_size |> filter(items == main_item) |> pull(n) |> max()
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

    plot_flip = function(target_col, main_item = NULL) {
      target_col <- rlang::ensym(target_col) |> rlang::as_string()

      data1 <- private$data1
      data2 <- private$data2
      labels <- private$labels

      df1 <- data1 |>
        group_by(!!rlang::sym(target_col)) |>
        summarise(y = sum(y), n = n(), rate = y / n)
      df2 <- data2 |>
        group_by(!!rlang::sym(target_col)) |>
        summarise(y = sum(y), n = n(), rate = y / n)

      names1 <- df1[[target_col]]
      names2 <- df2[[target_col]]

      score1 <- data1 |> summarise(score = mean(y)) |> pull(score)
      score2 <- data2 |> summarise(score = mean(y)) |> pull(score)

      result <- tibble::tibble()
      for (name in names2) {
        df_temp <- df1
        if (name %in% names1) {
          df_temp[df_temp[[target_col]] == name, ] <- df2[df2[[target_col]] == name, ]
        } else {
          df_temp <- rbind(df_temp, df2[df2[[target_col]] == name, ])
        }

        score_new <- df_temp |> summarise(score = sum(y) / sum(n)) |> pull(score)
        diff <- score_new - score1
        res <- tibble::tibble(items = name, amount = diff)
        result <- rbind(result, res)
      }
      for (name in names1) {
        df_temp <- df2
        if (name %in% names2) {
          df_temp[df_temp[[target_col]] == name, ] <- df1[df1[[target_col]] == name, ]
        } else {
          df_temp <- rbind(df_temp, df1[df1[[target_col]] == name, ])
        }

        score_new <- df_temp |> summarise(score = sum(y) / sum(n)) |> pull(score)
        diff <- score2 - score_new
        res <- tibble::tibble(items = name, amount = diff)
        result <- rbind(result, res)
      }

      data1_size <- data1 |> count(items = !!rlang::sym(target_col)) |> mutate(type = labels[1])
      data2_size <- data2 |> count(items = !!rlang::sym(target_col)) |> mutate(type = labels[2])
      data_size <- rbind(data1_size, data2_size)

      result <- result |>
        group_by(items) |>
        summarise(amount = -mean(amount)) |>
        arrange(amount)
      names <- result$items
      result <- tibble::tibble(items = labels[2], amount = score2) |>
        rbind(result)|>
        mutate(amount = round(amount * 100, digits = 3L))

      colors <- if_else(result$amount > 0, "#F8766D", "#00BFC4")
      colors[1] <- "#00BFC4"
      p <- waterfalls::waterfall(
        result, calc_total = TRUE, total_axis_text = labels[1],
        total_rect_text_color = "black", fill_colours = colors,
        fill_by_sign = FALSE, total_rect_color = "#00BFC4") +
        coord_flip()

      reverse_sign <- function(x) {
        x <- str_replace(x, "−", "-")
        x <- -as.numeric(x)
        x <- as.character(x)
        str_replace(x, "-", "−")
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

      if (is.null(main_item)) {
        data_max <- result |> tail(-1) |> filter(abs(amount) == max(abs(amount)))
        max_item <- data_max |> pull(items)
        max_amount <- data_max |> pull(amount) |> abs()
        n_max <- data_size |> filter(items == max_item) |> pull(n) |> max()
      } else {
        max_amount <- result |> filter(items == main_item) |> pull(amount) |> abs()
        n_max <- data_size |> filter(items == main_item) |> pull(n) |> max()
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
    }
  )
)
