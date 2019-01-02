# Editor: The First Reversible HTTP Server

![Screenshot of Editor](/screenshot-2.png?raw=true)

Editor is an HTTP server that not only displays HTML (.html), Markdown (.md) and Elm (.elm [^elm-pages]) pages, but also propagates back modifications made to the pages to the source files themselves.

To visually edit a page, simply add `?edit=true` to its URL, and the content will be editable by mouse and keyboard.
Alternatively and anytime, you can use the DOM inspector of the browser of your choice to modify the source page.

## Quick launch

Install node.js and run the following command:

    npm install -g http-server-editor

Now, to launch a reversible HTTP server in any folder, run:

    editor

The longer version of this command is `http-server-editor`.  
Then, point your browser to http://127.0.0.1:3000

## Simple dynamic example

Create a file `pizzas.elm` with the following content:

    user     = listDict.get "user" vars |> Maybe.withDefaultReplace "Anonymous"
    userdata = [("Mikael", 2)]
    options  = ["Margharita", "Four Cheese", "Pepper"]
    main     = <html><head></head><body>
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

Now launch Editor, and point your browser to http://127.0.0.1:3000/pizzas.elm?user=RandomDude  
Congratulations. In 16 lines of code, you created a Content Management System where all the CRUD operations (create-read-update-delete) can be done in the browser:
* Selecting/Modifying someone's pizza choice
* Add/Modify/delete pizzas
* Modify any text you see
* Display the summary of choice

Beware, the system does not support concurrent editing yet, make sure you alone edit the page at the same time.

#### Advanced example

Look into [`test/pizzas.elm`](https://github.com/MikaelMayer/Editor/blob/master/test/pizzas.elm) and [`CONTRIBUTING.md`](https://github.com/MikaelMayer/Editor/blob/master/CONTRIBUTING.md) for more advanced examples covering the same website with file reading, evaluation, translation to different languages, deletion of choices, modification of pizza by name, and more.

## Supported features

### Administrator rights

It's possible to active the admin rights by setting `&admin=true` or `?admin=true` in the URL. With these rights, 

* If you point the browser to a non-existing HTML or Elm file and modify it, it will display a default template and automatically create the file as soon as you modify the template.
* If you modify the menus, it will create a modified `server.elm` at the root the folder instead of using the built-in one. Careful: when you upgrade Editor, you should remove this file as there might be incompatibilities.

### Access permisions based on path

If a file `htaccess.elm` is at the root of the folder, it will be executed with an environment containing the variables `path` and `method` ("GET" or "POST") and should produce a boolean (`True`/`False`)indicating if the operation is allowed.

### Custom markdown styling

Markdown styling can be customized by creating a `markdown.css` file at the root where Editor is launched.
Alternatively, one can modify the inline &lt;style&gt; tag at the beginning of the document using the DOM inspector. This action will create the `markdown.css` file directly.

### Dealing with scripts or plug-ins that modify the page

Some scripts or plugins (such as Google Analytics, Ace editor, Grammarly...) insert nodes or add special attributes. These nodes and attributes should not be back-propagated.  
Editor offers several mechanisms to prevent this back-propagation. You can either do it manually or have Editor take care of that.

#### Commands via attributes

You can add special attributes to an HTML element to mark some parts as being "ghost", that is, they should not be back-propagated.

* `isghost="true"` on an element ensures that the whole element is ignored when back-propagation occurs. Alternatively, setting `element.isghost=true` in javascript results in the same effect without modifying the DOM.  
  Never put isghost="true" on an element on the source side level, it would be automatically erased on the first back-propagation.
* `list-ghost-attributes="attr1 attr2 ... attrn"` on an element ensures that any inserted attribute with one of the name `attr1` ... `attrn` will not be back-propagated.
  Never put one of the `attr1` ... `attrn` attributes on the element directly, else it would be automatically erased on the first back-propagation.
* `children-are-ghost="true"` on an element ensures that any inserted or modified child to this element is not back-propagated.
  Never add children at the source level to an element which has this attribute, else they would be automatically erased on the first back-propagation.

#### Editor's global ghost nodes and attributes.

Editor also observes insertions and deletions and can mark some elements as ghost so you don't need to manage this yourself.
In a script at the beginning of the body:

* `(setGhostOnInserted || []).push(insertedNode => /*PREDICATE ON insertedNode*/);`: For any inserted node, if this predicate returns `true`, Editor will mark and consider it as ghost so you don't need to manage it.  
  A simple predicate to filter out inserted nodes which have the class "dummy" would look like: `insertedNode.nodeType == 1 && insertedNode.classList && insertedNode.classList.contains("dummy")`.
* `(globalGhostAttributeKeysFromNode || []).push(node => /*ARRAY OF STRINGS*/);`: For any node, the array of strings respresents attribute names that should always be considered as ghost.

### Dealing with page reloads

Editor re-writes the whole page each time a update is back-propagated. It is however possible to save some ghost attributes and some ghost nodes. Here is the list of things Editor saves and restores:

* Any node with an `id` and `ghost-visible` DOM attribute will have its `ghost-visible` DOM attribute value restored.
* Any node with an `id` and `save-attributes` DOM attribute will have all the javascript attributes, that are encoded in the value of `save-attributes` separated with space, restored.
* Any node with a `save-ghost` attribute set to `true`, that is a child of `head` or whose parent has an `id`, will be reinserted back as a child to the `head` or the parent, if it does not yet exists.

### Uploading media files.

You can drop images on a webpage, they are automatically uploaded at the current relative location, and an img html element is inserted. See how this works:

  ![Demo of image drop](/drop-image.gif?raw=true)

### Edit links

  ![Demo of link editing](/create-link.gif?raw=true)

In edit mode, if you click on a link, a pop-up appears, allowing you to
* Navigate to the link
* Modify the link's address
* Delete the link

You can add links by selecting some text and pressing CTRL+K, and then use the method above to edit the newly inserted link.

### Listing files

If a folder does not contain an `index.html` or a `README.md`, Editor will display a list of files.
To force to list the files in a folder even though there is an `index.html` or a `README.md`, just append &amp;ls=true in the URL query parameters.

From any view listing files, you can delete files by deleting the corresponding bullet point.
Similarly, to rename a file, go to devtools and rename the text of the link.

### Add edition capabilities to your webpage

When the `edit=true` search query param is set, the following style is injected to the page:

    .editor-menu { display: initial !important; }

This means that whatever had the class `editor-menu` will be displayed in this edit mode. You can use it to define your own scripts that self-modify the page. A default toolbar is to come soon in Editor, keep in touch.

### Command-line arguments

Editor can be run with some commands to change its default behavior.

* `--edit=false` deactivates the edit mode (activated by default). It can always be re-added on any URL by adding `?edit=true`
* `--autosave=false` deactivates the autosave functionality (activated by default). It can always be re-added on any URL by adding `?autosave=true`
* `--question=false` automatically chooses the first answer in case of ambiguity (interactive questioning by default). It can always be re-added on any URL by adding `?question=true`
* `--admin=true` activates the admin mode (mostly to change `server.elm` -- deactivated by default). It can always be re-added on any URL by adding `?admin=false`
* `--path=dir` launch Editor at the given directory (can be relative or absolute)
* '--port=8080' sets the listening port to 8080 (default: 3000).

### Use Editor to open \*.html, \*.md and \*.elm files

Editor can also be used to open files on the command line. This can be useful to quickly edit one html or markdown file. Sample syntax:

    editor test/links.html

This will launch a temporary version of Editor and open an URL to this file in the default browser.
When the tab or window is closed, the temporary server is killed.

On Windows, if you want to open a file from the explorer window, right-click on an HTML file, Open with, Choose another app[^note], More apps, Look for another app on this PC, navigate to `C:\Users\[your username]\AppData\Roaming\npm` and select `editor.cmd`.

[^note]: At this point, check "Always use this app to open `*.html/*.md files"` to ensure Editor always appears in the list of apps that can open html files.
You can always revert to your favorite Desktop application to open these files later.

### NPM require package

You can invoke Editor as an NPM package in node.js. To do so, after installing `http-server-editor`, place in your code:

    require("http-server-editor")({option:true});
    
You can use the syntax `option:true` to pass along any option described in the previous section 'Command line arguments'

## Limitations, future work and caution

* **Ambiguities**: There are more ambiguity than there should be. We are working on removing meaningless ambiguities. Stay in touch!

* **HTML formatting caution**: On Windows, while loading dynamic `.elm` webpages, if you use `fs.read`, make sure to convert the resulting string with `String.newlines.toUnix`. This is a reversible function that ensures that the newlines are \n and not \r\n. Else, the reverse interpreter will replace all windows-like newlines \r\n by Unix-like newlines \n anyway but this might take a looong time.

* **Need for authentication**: Since there is no authentication yet, everybody that has access to the server can easily modify all the files present. Please do not use this server for production until there is proper authentication. If you want to contribute to authentication, a pull request is welcome.

* **Need for concurrent editing**: In case there are two conflicting edits, they will not be merged, only the second will take place. There is a work in progress for merging edit diffs.

* **Need for better diffs**: The set of edits to nodes is limited to modifications, insertions and deletions. There is no wrapping/unwrapping or other forms of clones. We are working on a new way to express a greater set of edits.

* **Need for templates**: Editor could allow you to create a page from given templates. We'll work on that. We already have several templates in [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch). Among the templates, we want slides, docs, recipe editor, worksheet, contact forms, academic webpage, etc.

* **Need for a toolbar**: It's very easy to add menus or contextual menus to do actions on the page, so we would not need to rely on devtools. The link edition is an example. PR are welcome to have a better menu bar to edit images, tables, etc.

## License

This technology is offered without any warranty (for the moment).
Please refer [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch)'s license to use this technology commercially.
For academic projects and webpages this should be fine.

[^elm-pages]: Similar to PHP, pages written in an Elm-like language are executed and served as HTML. For more info on how to write such pages, visit the [main project page](https://github.com/ravichugh/sketch-n-sketch)
