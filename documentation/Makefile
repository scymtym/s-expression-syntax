SOURCES=s-expression-syntax.texi

INCLUDES=-I docstrings -I examples -I figures

RESULTS=s-expression-syntax.pdf s-expression-syntax.info s-expression-syntax s-expression-syntax.html

.PHONY: all clean distclean

all: $(RESULTS)

s-expression-syntax.info: $(SOURCES)
	makeinfo $(INCLUDES) --output s-expression-syntax.info s-expression-syntax.texi

s-expression-syntax.pdf: $(SOURCES)
	texi2pdf $(INCLUDES) s-expression-syntax.texi

s-expression-syntax: $(SOURCES)
	makeinfo $(INCLUDES) --html --css-include=style-multi.css s-expression-syntax.texi

s-expression-syntax.html: $(SOURCES)
	makeinfo $(INCLUDES) --html --no-split --css-include=style-common.css --css-include=style-single.css s-expression-syntax.texi

clean:
	rm -rf s-expression-syntax.cp s-expression-syntax.cps s-expression-syntax.fn s-expression-syntax.fns s-expression-syntax.toc \
		s-expression-syntax.tp s-expression-syntax.tps s-expression-syntax.vr s-expression-syntax.vrs s-expression-syntax.aux s-expression-syntax.log

distclean: clean
	rm -rf $(RESULTS)
