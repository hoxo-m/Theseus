#' @import dplyr ggplot2 stringr
#'
#' @export
ShipOfTheseus <- R6::R6Class(
  "ShipOfTheseus",

  private = list(
    labels = NULL,
    compute_scores = NULL,
    to_factor = NULL,
    compute_contribution = NULL,
    compute_info = NULL,
    compute_size = NULL
  ),

  public = list(

    #' @importFrom forcats fct_na_value_to_level
    initialize = function(data1, data2, outcome, labels) {
      outcome <- rlang::quo_squash(outcome) |> rlang::as_string()

      data1 <- data1 |>
        mutate_if(is.character, ~ fct_na_value_to_level(.x, level = "(Missing)") |> as.character()) |>
        mutate_if(is.factor, ~ fct_na_value_to_level(.x, level = "(Missing)")) |>
        rename(.outcome = !!rlang::sym(outcome))
      data2 <- data2 |>
        mutate_if(is.character, ~ fct_na_value_to_level(.x, level = "(Missing)") |> as.character()) |>
        mutate_if(is.factor, ~ fct_na_value_to_level(.x, level = "(Missing)")) |>
        rename(.outcome = !!rlang::sym(outcome))

      private$labels <- labels

      private$compute_scores <- memoise::memoise(function(column_name) {
        score1 <- data1 |> summarise(score = mean(.outcome)) |> pull(score)
        score2 <- data2 |> summarise(score = mean(.outcome)) |> pull(score)
        c(score1, score2)
      })

      private$to_factor <- memoise::memoise(function(column_name, config_continuous) {
        if (is.null(config_continuous$breaks)) {
          values <- c(data1[[column_name]], data2[[column_name]])
          break_num <- config_continuous$n
          if (config_continuous$split == "width") {
            if (any(is.na(values))) break_num <- break_num - 1L
            min <- min(values, na.rm = TRUE)
            max <- max(values, na.rm = TRUE)
            breaks <- seq(min, max, length.out = break_num + 1)
          } else if(config_continuous$split == "count") {
            breaks <- compute_breaks(values, break_num = break_num)
          } else {
            breaks <- compute_breaks(values, break_num = break_num * 20L)
            breaks <- sort(unique(breaks))
            d1 <- data1 |>
              select(x = !!rlang::sym(column_name), y = .outcome) |>
              filter(!is.na(x))
            d2 <- data2 |>
              select(x = !!rlang::sym(column_name), y = .outcome) |>
              filter(!is.na(x))
            while (length(breaks) > break_num + 1L) {
              data1_tmp <- d1 |>
                mutate(x = cut(x, breaks = breaks, include.lowest = TRUE)) |>
                group_by(x) |>
                summarise(y = mean(y)) |>
                mutate(diff1 = abs(lead(y) - y))
              data2_tmp <- d1 |>
                mutate(x = cut(x, breaks = breaks, include.lowest = TRUE)) |>
                group_by(x) |>
                summarise(y = mean(y)) |>
                mutate(diff2 = abs(lead(y) - y))
              data_tmp <- data1_tmp |> left_join(data2_tmp, by = "x") |>
                mutate(diff = sqrt(diff1^2 + diff2^2))
              breaks <- breaks[-(which.min(data_tmp$diff) + 1L)]
            }
          }
          if (config_continuous$pretty) {
            breaks <- pretty_breaks(breaks)
            if (any(table(breaks) >= 2)) {
              warning("Prettying breaks reduced the number of breaks. Try pretty = FALSE.")
              breaks <- unique(breaks)
            }
          }
        } else {
          breaks <- config_continuous$breaks
        }

        df1 <- data1
        df2 <- data2

        df1[[column_name]] <- cut(df1[[column_name]], breaks = breaks, include.lowest = TRUE, dig.lab = 50)
        df2[[column_name]] <- cut(df2[[column_name]], breaks = breaks, include.lowest = TRUE, dig.lab = 50)

        df1[[column_name]] <- fct_na_value_to_level(df1[[column_name]], level = "(Missing)")
        df2[[column_name]] <- fct_na_value_to_level(df2[[column_name]], level = "(Missing)")

        list(df1, df2)
      })

      private$compute_contribution <- memoise::memoise(function(column_name, config_continuous) {
        if (is.numeric(data1[[column_name]])) {
          data_list <- private$to_factor(column_name, config_continuous)
          data1 <- data_list[[1]]
          data2 <- data_list[[2]]
        }

        df1 <- data1 |>
          group_by(!!rlang::sym(column_name)) |>
          summarise(y = sum(.outcome), n = n(), rate = y / n)
        df2 <- data2 |>
          group_by(!!rlang::sym(column_name)) |>
          summarise(y = sum(.outcome), n = n(), rate = y / n)

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

        if (is.factor(names1)) {
          names <- forcats::fct_c(names1, names2)
          result <- result |>
            mutate(items = factor(items, levels = levels(names)))
        }
        result |>
          group_by(items) |>
          summarise(contrib = mean(amount)) |>
          mutate(contrib = (score2 - score1) * contrib / sum(contrib))
      })

      private$compute_info <- memoise::memoise(function(column_name, config_continuous) {
        if (is.numeric(data1[[column_name]])) {
          data_list <- private$to_factor(column_name, config_continuous)
          data1 <- data_list[[1]]
          data2 <- data_list[[2]]
        }

        data1_info <- data1 |>
          group_by(items = !!rlang::sym(column_name)) |>
          summarise(n1 = n(), x1 = sum(.outcome), rate1 = x1 / n1)
        data2_info <- data2 |>
          group_by(items = !!rlang::sym(column_name)) |>
          summarise(n2 = n(), x2 = sum(.outcome), rate2 = x2 / n2)
        data1_info |> full_join(data2_info, by = "items") |>
          select(items, starts_with("n"), starts_with("x"), starts_with("rate")) |>
          tidyr::replace_na(list(n1 = 0L, n2 = 0L, x1 = 0L, x2 = 0L))
      })

      private$compute_size <- memoise::memoise(function(column_name, target, config_continuous) {
        if (is.numeric(data1[[column_name]])) {
          data_list <- private$to_factor(column_name, config_continuous)
          data1 <- data_list[[1]]
          data2 <- data_list[[2]]
        }

        data1_size <- data1 |>
          filter(!!rlang::sym(column_name) %in% target) |>
          count(items = !!rlang::sym(column_name)) |>
          mutate(type = labels[1])
        data2_size <- data2 |>
          filter(!!rlang::sym(column_name) %in% target) |>
          count(items = !!rlang::sym(column_name)) |>
          mutate(type = labels[2])
        item_names <- unique(c(data1_size$items, data2_size$items))
        other_name <- target[!(target %in% item_names)]

        if (length(other_name) == 0L) {
          bind_rows(data1_size, data2_size)
        } else {
          data1_size_other <- data1 |>
            filter(!(!!rlang::sym(column_name) %in% target)) |>
            count() |>
            mutate(type = labels[1], items = other_name)
          data2_size_other <- data1 |>
            filter(!(!!rlang::sym(column_name) %in% target)) |>
            count() |>
            mutate(type = labels[2], items = other_name)
          if (is.factor(data1_size$items)) {
            data1_size <- data1_size |> mutate(items = as.character(items))
            data2_size <- data2_size |> mutate(items = as.character(items))
            bind_rows(data1_size, data1_size_other, data2_size, data2_size_other)
          } else {
            bind_rows(data1_size, data1_size_other, data2_size, data2_size_other)
          }
        }
      })

    },

    table = function(column_name, n = Inf,
                     config_continuous = config_continuous_set()) {
      column_name <- rlang::ensym(column_name) |> rlang::as_string()
      data_contrib <- private$compute_contribution(column_name, config_continuous)
      data_info <- private$compute_info(column_name, config_continuous)
      result <- data_contrib |>
        left_join(data_info, by = "items")
      is_factor <- is.factor(result$items)
      if (is_factor) {
        levels <- levels(result$items)
        result <- result |> arrange(items)
      } else {
        result <- result |> arrange(desc(abs(contrib)))
      }
      n_items <- nrow(result)
      if (n_items > n) {
        n_other <- n_items - n + 1L
        result_head <- head(result |> arrange(desc(abs(contrib))), n - 1L) |>
          mutate(items = as.character(items))
        result_tail <- tail(result |> arrange(desc(abs(contrib))), n_other)
        result_other <- result_tail |>
          mutate(items = str_glue("Sum of {n_other} other attributes")) |>
          group_by(items) |>
          summarise_at(vars(contrib, n1, n2, x1, x2), sum) |>
          mutate(rate1 = x1 / n1, rate2 = x2 / n2)
        result <- bind_rows(result_head, result_other)
        if (is_factor) {
          levels <- c(levels, str_glue("Sum of {n_other} other attributes"))
          result <- result |>
            mutate(items = factor(items, levels = levels)) |>
            arrange(items)
        }
      }
      names(result)[1] <- column_name
      result
    },

    plot = function(column_name, n = 10L, main_item = NULL, bar_max_value = NULL,
                    levels = NULL, config_continuous = config_continuous_set()) {
      column_name <- rlang::ensym(column_name) |> rlang::as_string()

      labels <- private$labels

      score1 <- private$compute_scores(column_name)[1]

      result <- self$table(!!rlang::sym(column_name), n = n, config_continuous = config_continuous) |>
        select(items = 1L, contrib)
      is_factor <- is.factor(result$items)
      if (is_factor) {
        result <- result |> arrange(items)
      } else {
        result <- result |> arrange(contrib)
      }

      data_size <- private$compute_size(column_name, target = result$items, config_continuous = config_continuous)

      if (!is.null(levels)) {
        levels <- as.character(levels)
        result <- data.frame(items = levels) |> inner_join(result, by = "items")
      }
      names <- as.character(result$items)
      result <- tibble::tibble(items = labels[1], contrib = score1) |>
        bind_rows(result)|>
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
      p + ggplot2::ggtitle(NULL, subtitle = column_name)
    },

    plot_flip = function(column_name, n = 10L, main_item = NULL, bar_max_value = NULL,
                         levels = NULL, config_continuous = config_continuous_set()) {
      column_name <- rlang::ensym(column_name) |> rlang::as_string()

      labels <- private$labels

      score2 <- private$compute_scores(column_name)[2]

      result <- self$table(!!rlang::sym(column_name), n = n, config_continuous = config_continuous) |>
        select(items = 1L, contrib) |>
        mutate(contrib = -contrib)

      is_factor <- is.factor(result$items)
      if (is_factor) {
        result <- result |> arrange(desc(items))
      } else {
        result <- result |> arrange(contrib)
      }
      data_size <- private$compute_size(column_name, target = result$items, config_continuous = config_continuous)

      if (!is.null(levels)) {
        levels <- as.character(levels) |> rev()
        result <- data.frame(items = levels) |> inner_join(result, by = "items")
      }
      names <- as.character(result$items)
      result <- tibble::tibble(items = labels[2], contrib = score2) |>
        bind_rows(result)|>
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
      p + ggplot2::ggtitle(NULL, subtitle = column_name)
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
