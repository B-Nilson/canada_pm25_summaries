is_production <- dir.exists("/srv") & !interactive()

if (!is_production) {
  source("renv/activate.R")
}else {
  .libPaths(new = Sys.glob("./renv/library/*/*/*/"))
}
