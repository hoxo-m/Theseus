memoise_R6_method <- function(obj, method_name, type = "private") {
  type <- match.arg(type)
  env <- obj$.__enclos_env__[[type]]
  unlockBinding(method_name, env)
  env[[method_name]] <- memoise::memoise(env[[method_name]])
  lockBinding(method_name, env)
}
