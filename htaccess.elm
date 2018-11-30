-- input: path      The path to access ("" is the root)
-- input: method    "GET" (to read) or "POST" (to write)
-- Output: Boolean  If the user has access or not.

match = Regex.matchIn

readrules = [
    (match """\.\.(?:/|\\)|(?:/|\\)\.\.|^\.\.$""", False) -- We disallow relative paths
  , (match """\.(png|jpg|ico)$""", True) -- We allow access to common media files
  , (match """\.elm$""", True)
  , (match """\.html$""", True)
  ]

writerules = [
    (match """\.\.(?:/|\\)|(?:/|\\)\.\.|^\.\.$""", False) -- We disallow relative paths
  , (match """\.(png|jpg|ico)$""", False) -- We don't allow write access to any media files for now.
  , (match """\.elm$""", True)
  , (match """\.html$""", True)
  ]

defaultPermission = False

applyRules rules = case rules of
  [] -> defaultPermission
  (matches_rule, result)::remainingRules ->
    if path |> matches_rule then
      result
    else applyRules remainingRules

(if method == "GET" then readrules else writerules) |>
 applyRules