fast-devel:
	yesod-fast-devel src/DevelMain.hs

livereload:
	browser-sync start --no-open --files="devel-main-since" --proxy "localhost:3000" --port 4000
