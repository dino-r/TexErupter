.PHONY: clean

erupted_example.html: example.html 
	../../src/texerupter -p tikz,pgfplots --img="class=\"block\"" --eq-inline="class=\"eqinline\"" $<

clean:
	rm -f *.svg *.pdf *.aux *.log *.tex erupted_example.html
