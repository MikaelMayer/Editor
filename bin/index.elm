readmefile = "README.md"

readme = nodejs.fileread readmefile
  |> Maybe.withDefault """# No @readmefile found."""

source = """<html><head></head><body>@(String.markdown readme)</body></html>"""

case __evaluate__ [] source of
  Ok x -> x
  Err x -> <html><head></head><body>Error while interpreting @readmefile:<pre>@x</pre>README content:<pre>@readme</pre></body></html>