user  = vars |> case of {user} -> user; _ -> "Anonymous"
userdata = [("Mikael", 2)]
options = ["Margharita", "Four Cheese", "Pepper"]
main =
<html><head></head><body>
  <span>Hello @user!<br>
  Select the pizza you want
  @Html.select[]("Choose one..."::options)(
  listDict.get user userdata
  |> Maybe.withDefaultReplace (freeze 0))<br><br>
 Final choices:<br>
@(List.map (\(name, id) ->
  <span>@name choose @(List.find (\(i, n) -> i == id) (List.zipWithIndex options)
    |> Maybe.map Tuple.second
    |> Maybe.withDefault "that does not exist").<br></span>
  ) userdata)
</span></body></html>