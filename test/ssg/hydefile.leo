module = {
  test = ()
  hopla = 2
  toast = 2
}

all () =
  fs.read "src/a.md"
  |> Maybe.map (\content ->
    """<html><head></head><body>@(String.markdown content)</body></html>"""
    |> Write "output/b.html")
  |> Maybe.withDefault (Error "file src/a.md not found")