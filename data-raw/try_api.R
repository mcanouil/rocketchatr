# library(httr)

rc_api <- function(
  path,
  url = "http://chat-new.egid.local:3000/",
  user_agent = httr::user_agent("https://github.com/mcanouil/rocketchatr")
) {
  url <- httr::modify_url(url, path = path)

  resp <- httr::GET(url, user_agent)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  if (httr::status_code(resp) != 200) {
    stop(
      sprintf(
        "REST API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "rocketchat_api"
  )
}


authenticate <- function(
  path = "/api/v1/login",
  url = "http://chat-new.egid.local:3000/",
  user_agent = httr::user_agent("https://github.com/mcanouil/rocketchatr")
) {
  url <- httr::modify_url(url, path = path)

  resp <- httr::POST(
    url = url,
    config = user_agent,
    list(
      user = rstudioapi::askForPassword("User"),
      password = rstudioapi::askForPassword("password")
    )
  )
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  if (httr::status_code(resp) != 200) {
    stop(
      sprintf(
        "REST API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "rocketchat_api"
  )
}
