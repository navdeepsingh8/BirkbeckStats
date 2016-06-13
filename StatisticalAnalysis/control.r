path="d:\\library\\courses\\birkbeck msc as\\statistical analysis b\\cw\\";
file="cw2emacs.rnw";

setwd(path);
fileTex=Sweave(file,syntax="SweaveSyntaxNoweb");
dev.off();
system(paste("pdflatex",fileTex));

