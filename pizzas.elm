edit  = vars |> case of {edit} -> edit; _ -> "false"
user  = vars |> case of {user} -> user; _ -> "Laurent"
hl    = vars |> case of {hl} -> hl; _ -> "en"

userdata = [
    ("Mikael", 2)
  , ("Ravi", 4)
  , ("Laurent", 2)
  ]
  
options = nodejs.fileread "data/pizzas.txt"
  |> Maybe.withDefaultReplace (freeze """[
    "$Margharita",
    "$Queen",
    "Montagnarde",
    "Barbecue"]""")
  |> evaluate

dictionnaire = [
  ("English", [ ("abbreviation", "en")
              , ("Salut", "Hey")
              , ("Tuveuxquellepizza", "Which pizza do you want")
              , ("achoisiunepizza", "wants a pizza")
              , ("Choisistapizza", "Choose your pizza")
              , ("Margharita", "Margharita")
              , ("Queen", "Queen")
              , ("Montagnarde", "Mountain")
              ]),
  ("Français", [ ("abbreviation", "fr")
               , ("Salut", "Salut")
               , ("Tuveuxquellepizza", "Tu veux quelle pizza")
               , ("achoisiunepizza", "a choisi une pizza")
               , ("Choisistapizza", "Choisis ta pizza")
               , ("Margharita", "Margherita")
               , ("Queen", "Reine")
               , ("Montagnarde", "Montagnarde")
               ])
]

abbreviations = dictionnaire |>
   List.map (\(name, trads) ->
     listDict.get "abbreviation" trads
     |> Maybe.withDefault name)

indexLangue = 
  List.findByAReturnB Tuple.second Tuple.first hl (List.zipWithIndex abbreviations)
  |> Maybe.withDefaultReplace (freeze 0)

main = Html.translate dictionnaire indexLangue <|
<html><head></head><body @(if edit == "true" then [["contenteditable", "true"]] else [])>
  <span>$Salut @user! <br>
$Tuveuxquellepizza?
@Html.select[]("$Choisistapizza"::options)(
  listDict.get user userdata
  |> Maybe.orElseReplace (freeze (Just 0))
  |> Maybe.getUnless ((==) 0))
<br><br>
@(Html.select [] (List.map Tuple.first dictionnaire) indexLangue)<br><br>
 Final choices<br>
@(List.map (\(name, id) ->
  <span>@name $achoisiunepizza @(List.findByAReturnB Tuple.first Tuple.second (id - 1) (List.zipWithIndex options) |> Maybe.withDefaultReplace (freeze "qui n'existe pas")).<br></span>
) userdata)
</span>
@clientscript</body></html>