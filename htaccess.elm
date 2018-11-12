-- input: pagename  The file to access
-- input: method (GET or POST)
-- output: Boolean  If the user has access or not.

match = Regex.matchIn

readrules = [
    (match """(\.\.)(/|\\)""", False) -- We disallow relative paths
  , (match """(/|\\)(\.\.)""", False) -- We disallow relative paths
  , (match """^(\.\.)$""", False)     -- We disallow relative paths
  , (match """\.(png|jpg|ico)$""", True) -- We allow access to all media files
  , (match """\.elm$""", True)        -- TODO: Refine if some elm files should not be accessed.
]

writerules = [
    (match """(\.\.)(/|\\)""", False) -- We disallow relative paths
  , (match """(/|\\)(\.\.)""", False) -- We disallow relative paths
  , (match """^(\.\.)$""", False)     -- We disallow relative paths
  , (match """\.(png|jpg|ico)$""", False) -- We allow access to all media files
  , (match """\.elm$""", True)        -- TODO: Refine depending on user permissions.
]

default = False

aux rules = case rules of
  [] -> default
  (rule, result)::tail ->
    if rule pagename then
      result
    else aux tail

aux (if method == "GET" then readrules else writerules)
