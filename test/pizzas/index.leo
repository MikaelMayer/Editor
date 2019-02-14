user = listDict.get "user" vars |> Maybe.withDefaultReplace (freeze "Anonymous")
userdata =
  fs.read "data.txt"
  |> Maybe.withDefaultReplace "[]"
  |> evaluate

choice =
  listDict.get user userdata
  |> Maybe.withDefaultReplace (freeze 0)

pizzas = ["choisis ta pizza:", "Margherita", "Jambon", "Chorizo"]

<html><head></head><body>
<span>
Hello @user,
@(Html.select [] pizzas choice)
@(List.map (\(name, choice) ->
<li>@name a choisi @(List.findByAReturnB Tuple.first Tuple.second (choice) (List.zipWithIndex options) |> Maybe. (freeze "qui n'existe pas"))</li>) userdata)
</span>
</body></html>