fast-devel:
	yesod-fast-devel src/DevelMain.hs

reserve:
	stack exec -- reserve src/Main.hs -- serve

livereload:
	browser-sync start --no-open --files="devel-main-since" --proxy "localhost:3000" --port 4000
