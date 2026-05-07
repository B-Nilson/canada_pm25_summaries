is_production <- dir.exists("/srv")

# >>> uvr >>>
local({
  lib <- file.path(getwd(), ".uvr", "library")
  if (dir.exists(lib)) {
    message("Linking to uvr library at {.path {lib}}")
    .libPaths(lib)
  }
})
# <<< uvr <<<
