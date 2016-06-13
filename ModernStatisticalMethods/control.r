path="d:\\library\\courses\\birkbeck msc as\\modern statistical methods\\coursework\\";
file="cw.Rnw";
setwd(path)
fileTex=Sweave(file,syntax="SweaveSyntaxNoweb");
dev.off();
#system(paste("pdflatex",fileTex));

