setClass(
  "MetaData",
  slots = c(
    app_title = "character",
    tabs = "numeric",
    tab_titles = "character",
    tab_ids = "character",
    sidebar = "numeric",
    sidebar_labels = "character",
    sidebar_titles = "character",
    sidebar_choices_short = "list",
    sidebar_choices_long = "list",
    sidebar_skip = "numeric",
    tab_settings = "list",
    tab_inout = "list",
    tab_input = "list",
    tabItemsList = "list"
  ),
  prototype = list(

  )


)
