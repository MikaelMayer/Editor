publish:
	npm update sketch-n-sketch
	node bundle.js
	git diff --quiet && git diff --staged --quiet || git commit -am "Updated libraries and binaries"
	npm version patch
	npm publish
	git push origin master
	npm install -g http-server-editor
