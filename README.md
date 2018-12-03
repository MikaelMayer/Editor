# Editor: The First Reversible HTTP Server

Editor is an HTtP server that not only displays HTML (.html), Markdown (.md) and Elm (.elm [^elm-pages]) pages, but also propagates back modifications made to the pages themselves to the source files.

To visually edit a page, simply add `?edit=true` to its URL, and the content will be editable by the mouse and keyboard. Alternatively and anytime, you can use the DOM inspector of the browser of your choice to modify the source page.

## Quick launch

Install node.js and run the following command:

    npm install http-server-editor

Now, to launch a reversible HTTP server in any folder, run any of the following synonym commands:

    http-server-editor
    editor

then point your browser to http://127.0.0.1:3000

## Supported features

* If you point the browser to a non-existing HTML file and modify it, it will automatically create it.

* If a file `htaccess.elm` is at the root of the folder, it will be executed with an environment containing the variables `path` and `method` ("GET" or "POST") and should produce a boolean indicating if the operation is allowed: True meaning yes, False meaning no.
* If a file `server.elm` is at the root the folder, it will always execute this file with an environment containing the variables `path` containing the requested path, a record `vars` containing the query variables, and should output the page.

## Development

If you want to start the webserver after cloning the github repo, enter the commands:

    npm install
    node bin/server.js

then point your browser to http://127.0.0.1:3000

You can test the server by pointing your browser to http://127.0.0.1:3000/test/pizzas.elm?user=Anonymous&hl=fr&edit=true  
Here you can witness the possibilities of a 60-line-of-code webpage in Editor. You can...
* Include and interpret data tiles (`nodejs.fileread "data/pizzas.txt"`).
* Modify the current username directly from the webpage (see how it's updated in the URL)
* Select a pizza choice and so that it is stored and added it to the corresponding summary below.
* Change the summary style (text, color, font...) directly using the DOM explorer.
* Change the translation language
* Edit the current translated sentences
* Add a new translation language (go to the DOM inspector, and duplicate `<option>English</option>`, and then rename it to German)
* Add a new pizza (same but duplicate a pizza option)
* Rename a pizza wherever it appears
* Add new translated sentences from the webpage (wrap "Final choices" so that it becomes "{:Final choices:}". After a roundtrip update, switch to French and translate it ("Choix finaux").
* Remove one's pizza choice by selecting the first "Choose your pizza" option.

# Limitations, future work and caution

## HTML formatting caution

- Make sure your source files have unix-style end of lines (\n and not \r\n). Else this might cause a lot of trouble.

## Need for authentication

Since there is no authentication yet, everybody that has access to the server can in theory modify all the files present.
Please do not use this server publicly until there is proper authentication.
If you want to contribute to authentication, a pull request is welcome.

## Need for concurrent editing

In case there are two conflicting edits, they will not be merged, only the second will take place. There is a work in progress for merging edit diffs.

## Need for better diffs

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

## Need for templates

We did not push templates yet but we will soon, by importing them from [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch). Among the templates, we want
* Slides
* Bidirectional converters (e.g. Markdown, LaTeX)
* Self-modifying webpages.
* Recipe editor
* Worksheets
* Forms

## Need for a toolbar

We need a general-purpose HTML edition toolbar to edit all the web pages without relying only on the devtools.
Furthermore, we need the toolbar to expose udpate ambiguities and let the user choose from them.

## Need for a better WYSIWYG webpage editor.

Why not try [Slate?](https://www.slatejs.org) if it works for the entire HTML. Apparently it does.
https://github.com/ianstormtaylor/slate/blob/master/docs/reference/slate-html-serializer/index.md


# License

This technology is offered without any warranty (for the moment).
Please refer [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch)'s license to use this technology commercially.
For academic projects this should be fine.

[^elm-pages]: Similar to PHP, pages written in an Elm-like language are executed and served as HTML. For more info on how to write such pages, visit the [main project page](https://github.com/ravichugh/sketch-n-sketch)
