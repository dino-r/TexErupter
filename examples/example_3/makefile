.PHONY: clean

erupted_example.html: example.html 
	../../src/texerupter --img="class=\"block\"" $<

clean:
	rm -f *.svg *.pdf *.aux *.log *.tex erupted_example.html
