# Editor : The First Reversible HTTP Server

![Screenshot of Editor](/screenshot-2.png?raw=true)

Editor is an HTTP server that not *only* displays HTML (.html), Markdown (.md) and Elm (.elm [^elm-pages]) pages, but also propagates back modifications made to the pages to the source files themselves.

To visually edit a page, simply add `?edit=true` to its URL, and the content will be editable by mouse and keyboard.
Alternatively and anytime, you can use the DOM inspector of the browser of your choice to modify the source page.

## Quick launch

Install node.js and run the following command:

    npm install -g http-server-editor

Now, to launch a reversible HTTP server in any folder, run:

    editor

The longer version of this command is `http-server-editor`.  
Then, point your browser to http://127.0.0.1:3000?edit=true

## Supported features

* If you point the browser to a non-existing HTML or Elm file and modify it, it will display a default template and automatically create the file as soon as you modify the template.
* If a file `htaccess.elm` is at the root of the folder, it will be executed with an environment containing the variables `path` and `method` ("GET" or "POST") and should produce a boolean indicating if the operation is allowed: True meaning yes, False meaning no.
* If a file `server.elm` is at the root the folder, it will always execute this file with an environment containing the variables `path` containing the requested path, a record `vars` containing the query variables, and this file should output the page.
* Markdown styling can be customized by creating a `markdown.css` file. Alternatively, one can modify the inline &lt;style&gt; tag at the beginning of the document using the DOM inspector.
* Pages containing Google Analytics insert a &lt;script> to the page, that should not be back-propagated. This servers ensures that this new &lt;script> is added the attribute `isghost="true"` to prevent back-propagation.  
  To set up your own predicates concerning which inserted elements should automatically be marked as ghost, insert the following snippet at the very beginning of your body and modify the predicate:

      <script>
      (setGhostOnInserted || []).push(insertedNode =>
        insertedNode.tagName == "SCRIPT" && typeof insertedNode.getAttribute("src") == "string" &&
        insertedNode.getAttribute("src").indexOf("google-analytics.com/analytics.js") != -1
      );
      </script>

* You can drop images on a webpage, they are automatically uploaded at the current relative location, and an img html element is inserted. See how this works:

  ![Demo of image drop](/drop-image.gif?raw=true)

* To force to list the files in a folder even though there is an `index.html` or a `README.md`, just append &ls=true in the URL query parameters.

* From any view listing files, you can delete files by deleting the corresponding bullet point.
  Similarly, to rename a file, go to devtools and rename the text of the link.

## Simple dynamic example

Create a file `pizzas.elm` with the following content:

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

Now launch Editor, and point your browser to http://127.0.0.1:3000/pizzas.elm?edit=true&user=RandomDude  
Congratulations. In 17 lines of code, you created a Content Management System where all the CRUD operations (create-read-update-delete) can be done in the browser:
* Selecting/Modifying someone's pizza choice
* Add/Modify/delete pizzas
* Modify any text you see
* Display the summary of choice

Beware, the system does not support concurrent editing yet, make sure you alone edit the page at the same time.
    
#### Advanced example

Look into [`test/pizzas.elm`](https://github.com/MikaelMayer/Editor/blob/master/test/pizzas.elm) and [`CONTRIBUTING.md`](https://github.com/MikaelMayer/Editor/blob/master/CONTRIBUTING.md) for a more advanced example covering the same website with file reading, evaluation, translation to different languages, deletion of choices, modification of pizza by name, and more.

## Add edition capabilities to your webpage

When the `edit=true` search query param is set, the following style is injected to the page:

    .editor-menu { display: initial !important; }

This means that whatever had the class `editor-menu` will be displayed in this edit mode. You can use it to define your own scripts that self-modify the page. For example, adding these buttons allow you to manipulate rows on a table:

    <button style="display:none" class="editor-menu"
            onclick="duplicate(getEnclosingCaret('tr'))" contenteditable="false">Duplicate row</button>
    <button style="display:none" class="editor-menu"
            onclick="duplicate(getEnclosingCaret('tr'), {onBeforeInsert: emptyTextContent})" contenteditable="false">New row before</button>
    <button style="display:none" class="editor-menu"
            onclick="duplicate(getEnclosingCaret('tr'), {after: true, onBeforeInsert: emptyTextContent})" contenteditable="false">New row after</button>
    <button style="display:none" class="editor-menu"
            onclick="remove(getEnclosingCaret('tr'))" contenteditable="false">Remove row</button>

A default toolbar is to come soon in Editor, keep in touch.
    
## Limitations, future work and caution

### Ambiguity

There are more ambiguity than there should be. It's hard to know what changes have been back-propagated and to compare them.
We are aware of all these limitations. We are working on removing meaningless ambiguities, and on displaying a good summary of ambiguities that remain. Stay in touch !

### HTML formatting caution

- On Windows, while loading dynamic `.elm` webpages, if you use `nodejs.fileread`, make sure to convert the resulting string with `String.newlines.toUnix`. This is a reversible function that ensures that the newlines are \n and not \r\n. Else, the reverse interpreter will replace all windows-like newlines \r\n by Unix-like newlines \n anyway but this might take a looong time.

### Need for authentication

Since there is no authentication yet, everybody that has access to the server can in theory modify all the files present.
Please do not use this server for production until there is proper authentication.
If you want to contribute to authentication, a pull request is welcome. See the API to ignore some insertions of elements such as Google Analytics scripts above.

### Need for concurrent editing

In case there are two conflicting edits, they will not be merged, only the second will take place. There is a work in progress for merging edit diffs.

### Need for better diffs

Currently, updating the Elm program

    x = "user"
    <html><body>Hello @x</body></html>

with the new output value

    <html><body>Hello <b>user</b></body></html>

does not produce the expected

    x = "user"
    <html><body>Hello <b>@x</b></body></html>

but

    x = "user"
    <html><body>Hello <b>user</b></body></html>

A work in progress will overcome this issue.

### Need for templates

We did not push templates yet but we will soon, by importing them from [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch). Among the templates, we want
* Slides
* Bidirectional converters (e.g. Markdown, LaTeX)
* Self-modifying webpages.
* Recipe editor
* Worksheets
* Forms

### Need for a toolbar

We need a general-purpose HTML edition toolbar to edit all the web pages without relying only on the devtools.
Furthermore, we need the toolbar to expose udpate ambiguities and let the user choose from them.

### Need for a better WYSIWYG webpage editor.

Why not try [Slate?](https://www.slatejs.org) if it works for the entire HTML. Apparently it does.
https://github.com/ianstormtaylor/slate/blob/master/docs/reference/slate-html-serializer/index.md

Froala's editor is nice as well but is not free
https://www.froala.com/wysiwyg-editor
Note that we could add a feature to drop an image like they are doing.

## License

This technology is offered without any warranty (for the moment).
Please refer [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch)'s license to use this technology commercially.
For academic projects and webpages this should be fine.

[^elm-pages]: Similar to PHP, pages written in an Elm-like language are executed and served as HTML. For more info on how to write such pages, visit the [main project page](https://github.com/ravichugh/sketch-n-sketch)
