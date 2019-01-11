# Editor: The First Reversible HTTP/HTTPS Server

![Screenshot of Editor](/screenshot-2.png?raw=true)

Editor is an HTTP/HTTPS server that not only displays HTML (.html), Markdown (.md) and Elm (.elm[^elm-pages]) pages,
but also propagates back modifications made to the pages to the source files themselves.

To edit the page, just use the mouse and keyboard as you would do for a word document.
Alternatively and anytime, you can use the DOM inspector of the browser of your choice to modify the source page.
Editor also offers some convenience tools to upload images and create/modify links.

## Install and launch

Make sure [node.js](https://nodejs.org/) is installed. Open a terminal or command line interface, and run the following command:

    npm install -g http-server-editor

Now, to launch a reversible HTTP server in any folder, run:

    editor

The longer version of this command is `http-server-editor`.  
Then, point your browser to http://localhost:3000

## Features

* Toolbar
  - One-click close/open Edit mode
  - Display and modify the source code of the page being viewed.
  - Autosave or Save button (default)
  - Manual ambiguity resolution
    - Preview of changes to the visual webpage
    - Summary of changes to the file system
    - Save/cancel current solution
  - Automatic ambiguity resolution button ('questions')
* Page viewing and edition
  - Static files \*.html, \*.md supported
  - Dynamic files \*.elm or \*.leo supported with file reading on disk and a full-feature functional programming language.
  - Entire page editable via `contenteditable` -- Change any text, add elements to lists, delete images, etc.
  - Contextual menu to edit links
  - Image drop at the caret location
  - DOM modifiable using developer's tools (F12 in most browsers)
  - File listing to rename or delete files (`?ls=true` on URL)
  - Javascript API for custom page edition buttons
  - Customize css for rendering markdown
  - Keyboard shortcuts (links cTRL+K, save CTRL+S)
* Security
  - Editor launches in HTTPS if the key and certificates are present (see below)
  - path-based access permissions configurable (`htaccess.elm`)
  - Authentication is currently experimental
* Compatibility with plug-ins
  - Editor is compatible with Google Analytics, Google Sign-in, Ace Editor and many functions of Grammarly.
  - Javascript API to specify which modifications to the DOM should not be back-propagated.
* Integration
  - Command-line launch
    - Configurable options
    - Serve a folder
    - Open a file on disk
  - Windows integration
    - Open a file on disk
  - NPM package

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

Now launch Editor, and point your browser to http://localhost:3000/pizzas.elm?user=RandomDude  
Congratulations. In 16 lines of code, you created a Content Management System where all the CRUD operations (create-read-update-delete) can be done in the browser:
* Selecting/Modifying someone's pizza choice
* Add/Modify/delete pizzas
* Modify any text you see
* Display a summary of choices

### Advanced example

Look into [`test/pizzas.elm`](https://github.com/MikaelMayer/Editor/blob/master/test/pizzas.elm) and [`CONTRIBUTING.md`](https://github.com/MikaelMayer/Editor/blob/master/CONTRIBUTING.md) for more advanced examples covering the same website with file reading, evaluation, translation to different languages, deletion of choices, modification of pizza by name, and more.

## Supported features

### Command-line arguments

Editor can be run with some commands to change its default behavior.

* `--edit=false` deactivates the edit mode (activated by default). It can always be re-added on any URL by adding `?edit=true`
* `--autosave=false` deactivates the autosave functionality (activated by default). It can always be re-added on any URL by adding `?autosave=true`
* `--question=false` automatically chooses the first answer in case of ambiguity (interactive questioning by default). It can always be re-added on any URL by adding `?question=true`
* `--admin=true` activates the admin mode (mostly to change `server.elm` -- deactivated by default). It can always be re-added on any URL by adding `?admin=false`
* `--path=dir` launch Editor at the given directory (can be relative or absolute)
* `--port=8080` sets the listening port to 8080 (default: 3000).
* `--google-client-id=YOURCLIENTID.apps.googleusercontent.com` sets the google authentification client ID (see authentication below)
* `--openbrowser=true` launches a browser to open the URL where Editor is listening
* `--key=file-key.pem` sets the file containing the key for the https protocol (default: `localhost-key.pem`)
* `--cert=file.pem` sets the file containing the certificate for the https protocol (default: `localhost.pem`)

### Editing pages

#### Uploading media files.

When Editor displays a webpage in edit mode, place the caret anywhere and drop an image from your file system.
Images are automatically uploaded at the current relative location, and an &lt;img&gt; is inserted. See how this works:

  ![Demo of image drop](/drop-image.gif?raw=true)

#### Edit links

![Demo of link editing](/create-link.gif?raw=true)

In edit mode, if you click on a link, a pop-up appears, allowing you to
* Navigate to the link
* Modify the link's address
* Delete the link

For now, you can add links by selecting some text and pressing CTRL+K, and then use the method above to edit the newly inserted link.

#### Listing, Renaming, Deleting files

When the path on which Editor is opened is a folder, Editor will look for an `index.elm`, and `index.html` or a `README.md` to display by default.
Else it will display a list of files (unless `htaccess.elm` prevents it).
In any case, to force Editor to list the files in a folder, append &amp;ls=true in the URL query parameters.

From any view listing files, you can delete files by deleting the corresponding bullet point.
Similarly, to rename a file, browser the file list in edit mode and rename a bullet point.

![Listing files](/ls.png?raw=true]

### Advanced page editing and styling

#### Custom markdown styling

Editor can directly render markdown files. Markdown styling can be customized by creating a `markdown.css` file at the root where Editor is launched.
Alternatively, one can modify the inline &lt;style&gt; tag at the beginning of the document using the DOM inspector. This action will create the `markdown.css` file directly.

#### Dealing with scripts or plug-ins that modify the page

Some scripts or plugins (such as Google Analytics, Ace editor, Grammarly...) insert nodes or add special attributes. These nodes and attributes should not be back-propagated.  
Editor offers several mechanisms to prevent this unwanted back-propagation.

If you are the author of dynamically added elements or attributes to the page, Editor provides you a way to mark them as ghosts so that they will not be back-propagated. To do so:

* The attribute `isghost="true"` on an element ensures that the whole element is ignored when back-propagation occurs.
  Alternatively, setting `element.isghost=true` in javascript results in the same effect without modifying the DOM.  
  Never put isghost="true" on an element on the source side level, it would be automatically erased on the first back-propagation.
* The attribute `list-ghost-attributes="attr1 attr2 ... attrn"` on an element ensures that any inserted attribute with one of the name `attr1` ... `attrn` will not be back-propagated.
  Never put one of the `attr1` ... `attrn` attributes on the source side level, else it would be automatically erased on the first back-propagation.
* The attribute `children-are-ghost="true"` on an element ensures that any inserted or modified child to this element is not back-propagated.
  Never add children at the source level to an element which has this attribute, else they would be automatically erased on the first back-propagation.

If you are not yourself adding dynamic elements or attributes, Editor also observes insertions and deletions and lets you mark inserted elements as ghosts.
In a script at the beginning of the body:

* `(setGhostOnInserted || []).push(insertedNode => /*PREDICATE ON insertedNode*/);`: For any inserted node, if this predicate returns `true`, Editor will mark and consider the `insertedNode` as ghost.  
  A simple predicate to filter out inserted nodes which have the class "dummy" would look like: `insertedNode.nodeType == 1 && insertedNode.classList && insertedNode.classList.contains("dummy")`.
* `(globalGhostAttributeKeysFromNode || []).push(node => /*ARRAY OF STRINGS*/);`: For any node, the array of strings respresents attribute names that should always be considered as ghost.

#### Saving ghost attributes and properties on page rewrite after edits.

Editor re-writes the whole page each time a update is back-propagated. It is however possible to save some ghost attributes and some ghost nodes. Here is the list of things Editor saves and restores:

* Any node with an `id` and a `ghost-visible` DOM attribute will have its `ghost-visible` DOM attribute value restored.
* Any node with an `id` and a `save-properties` DOM attribute will have all the javascript properties, that are encoded in the value of `save-properties` separated with space, restored.
* Any node with an `id` and a `save-ghost-attributes` DOM attribute will have all its attributes, whose name are encoded in the value of `save-ghost-attributes` separated with spaces, restored. Attributes in `save-ghost-attributes` are automatically considered as ghost attributes, so no `list-ghost-attributes` is necessary;
* Any node with a `save-ghost` attribute set to `true`, that is a child of `head` or whose parent has an `id`, will be reinserted back as a child to the `head` or the parent, if it does not yet exists.

#### Add edition capabilities to your webpage

When Editor is in Edit mode, the following style is injected to the page:

    .editor-menu { display: initial !important; }

This means that whatever had the class `editor-menu` will be displayed in this edit mode.
For example, `<button style="display:none" class="editor-menu">Click me</button>` is a button that will only appear when the page is opened with Editor in edit mode.
You can use this mechanism to define your own scripts that self-modify the page.

Here is a couple of function helpers that Editor provides globally to simplify your button's callbacks:

    // Recursively deletes all text nodes of the HtmlNode
    emptyTextContent(node: HtmlNode)
    
    // Returns the innermost node of the given tag that contains the caret.
    getEnclosingCaret(tagName: String): HtmlNode 
    
    // Inserts `node` before the `beforeNode` which should be a child of `parent`
    // If `beforeNode` is undefined or not provided, `node` is appended to the children of `parent`
    insertBefore(parent: HtmlNode, node: HtmlNode [, beforeNode: HtmlNode])

    // Duplicates `node`.
    //   If the node is a <tr>, <th>, <td> or <li> followed by a text node (whitespace) and a node of the same tag,
    //   then duplicates the whitespace as well.
    // All options have default values.
    // options: {
    //   // If `after` is true, then the cloned node is inserted *after* the original node instead of before (default);
    //   after: Bool, 
    //   // `onBeforeInsert` transforms the cloned node just before it is inserted.
    //   onBeforeInsert: HtmlNode -> HtmlNode
    // }
    duplicate(node: HtmlNode [, options])
    
    // Removes a node. If the node is a <tr>, <th>, <td> or <li>, removes the whitespace before as well.
    remove(node: HtmlNode)

### Security

#### Configure HTTPS

The recommended way to create a localhost HTTPS certificate is to use [mkcert](https://github.com/FiloSottile/mkcert) which is very easy to install.

    mkcert --install
    mkcert localhost

To have Editor launch an HTTPS server instead of HTTP, just place `localhost-key.pem` and `localhost.pem` where you launch Editor.
Alternatively, you can specify the path to these files via command-line (see `--key` and `--cert` above).
To launch a production server, make sure to deny access to the certificate and the key (see `htaccess.elm` above); 

#### Path-based access permisions

If a file `htaccess.elm` is at the root of the folder, it will be executed with an environment containing the variables `path` and `method` ("GET" or "POST") and should produce a boolean (`True`/`False`)indicating if the operation is allowed.

For example, a simple `htaccess.elm` that prevents relative paths and access to the local key and certificate looks at follows:

    not (Regex.matchIn """\.\.(?:/|\\)|(?:/|\\)\.\.|^\.\.$""" path) &&
    not (Regex.matchIn """.*\.pem""" path)


#### Authentication (experimental)

It is now possible to authenticate on dynamic webpages using Google's Sign-in. The following is a minimal example illustrating how to authenticate and use the result:

    <html><head><meta name="google-signin-client_id" content=@googleClientId></head>
    <body>@googlesigninbutton
    <h1><img src=@(listDict.get "picture" user |> Maybe.withDefault "")>
    Hello @(listDict.get "given_name" user |> Maybe.withDefault "Anonymous")!</h1>
    </body></html>

That's it! Under the hood, `googleClientId` is by default my own app's client ID that works only for `localhost:3000`. If you want to modify it to use your own, register an app and credentials [there](https://console.developers.google.com/apis/credentials), obtain a cliend ID, and add to the command-line that runs Editor `--google-client-id=...` as specified in the *Command-line arguments* section above.

#### Administrator rights (soon obsolete, to be replaced by authentication)

It's possible to active the admin rights by setting `&admin=true` or `?admin=true` in the URL. With these rights, 

* If you point the browser to a non-existing HTML or Elm file and modify it, it will display a default template and automatically create the file as soon as you modify the template.
* If you modify the menus, it will create a modified `server.elm` at the root the folder instead of using the built-in one. Careful: when you upgrade Editor, you should remove this file as there might be incompatibilities.

### Integrate Editor

#### Use Editor to open \*.html, \*.md and \*.elm files

Editor can also be used to open files on the command line. This can be useful to quickly edit one html or markdown file. Sample syntax:

    editor test/links.html

This will launch a temporary version of Editor and open an URL to this file in the default browser.
When the tab or window is closed, the temporary server is killed.

On Windows, if you want to open a file from the explorer window, right-click on an HTML file, Open with, Choose another app[^note], More apps, Look for another app on this PC, navigate to `C:\Users\[your username]\AppData\Roaming\npm` and select `editor.cmd`.

[^note]: At this point, check "Always use this app to open `*.html/*.md files"` to ensure Editor always appears in the list of apps that can open html files.
You can always revert to your favorite Desktop application to open these files later.

#### NPM require package

You can invoke Editor as an NPM package in node.js. To do so, after installing `http-server-editor`, place in your code:

    require("http-server-editor")({edit:true});
    
You can use the syntax `option:true` to pass along any option described in the previous section 'Command line arguments'.
As example, [Hyde](https://github.com/MikaelMayer/hyde-build-tool) is a website builder is using this way to launch Editor.

## Limitations, future work and caution

* **Ambiguities**: There are more ambiguity than there should be. We are working on removing meaningless ambiguities. Stay in touch!

* **HTML formatting caution**: On Windows, while loading dynamic `.elm` webpages, if you use `fs.read`, make sure to convert the resulting string with `String.newlines.toUnix`. This is a reversible function that ensures that the newlines are \n and not \r\n. Else, the reverse interpreter will replace all windows-like newlines \r\n by Unix-like newlines \n anyway but this might take a looong time.

* **Need for concurrent editing**: In case there are two conflicting edits, they will not be merged, only the second will take place. There is a work in progress for merging edit diffs.

* **Need for better diffs**: The set of edits to nodes is limited to modifications, insertions and deletions. There is no wrapping/unwrapping or other forms of clones. We are working on a new way to express a greater set of edits.

* **Need for templates**: Editor could allow you to create a page from given templates. We'll work on that. We already have several templates in [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch). Among the templates, we want slides, docs, recipe editor, worksheet, contact forms, academic webpage, etc.

* **Need for a toolbar**: It would be very easy to add menus or contextual menus to do actions on the page, so we would not need to rely on devtools.
The link edition is such an example. PR are welcome to have a better menu bar to edit images, tables, etc.

## License

This technology is offered without any warranty (for the moment).
Please refer [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch)'s license to use this technology commercially.
For academic projects and webpages this should be fine.

[^elm-pages]: Similar to PHP, pages written in an Elm-like language are executed and served as HTML. For more info on how to write such pages, visit the [main project page](https://github.com/ravichugh/sketch-n-sketch)
