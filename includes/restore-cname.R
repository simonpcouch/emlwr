if (!file.exists("docs/CNAME")) {
  writeLines("emlwr.org", con = "docs/CNAME", sep = "")
}