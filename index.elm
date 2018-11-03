listedpages = ["index.elm", "pizzas.elm"]
indexPage = 0

mbPage  = vars |> case of {page} -> Just page; _ -> Nothing

page = mbPage |> Maybe.withDefault (nth listedpages indexPage)

editedFile = nodejs.fileread page
        |> Maybe.withDefaultReplace (Update.freeze """-- @page
<span>Modify me to create @page</span>""")

pageAdder = Update.lens2 {
  apply (listedpages, indexPage) = ""
  update {input=(listedpages, indexPage), outputNew=newPage} =
    let i = List.indexOf newPage listedpages in
    Debug.log ("variable update") <|
    if i == -1 then
      Ok (Inputs [(listedpages ++ [newPage], List.length listedpages)])
    else
      Ok (Inputs [(listedpages, i)])
  } listedpages indexPage

<html><head></head><body spellcheck="false" contenteditable="true"
>@(if mbPage == Nothing then Html.select [] listedpages indexPage else <span>Editing @page</span>)<input style="margin-left:10px;width:180px" type="text"
  v=pageAdder
  placeholder="Create a new page"
  title="Enter the name of a new page here and press ENTER"
  onchange="this.setAttribute('v',this.value)">@(if page == sourcefile then <span></span> else <span>Navigate to <a href=("/"+page) contenteditable="false">@page</a></span>)<br>
 <textarea v=editedFile onchange="this.setAttribute('v', this.value)" style="margin:0px;width:734px;height:387px">@editedFile</textarea>
@clientscript
</body></html>