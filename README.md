# TexErupter

TexErupter is a tool written in Haskell that enables you to write LaTeX code for equations directly inside an HTML file. 

TexErupter is useful for static content, there's no JavaScript or other foobar. The resulting files are pure HTML with SVG images. If you need to type an occasional equation in your HTML, you can use TexErupter to handle the externalization of LaTeX code and the generation of SVG files.

TexErupter defines the pseudo-tags \<tex\> and \<$\> that are processed by TexErupter. All content within the \<tex\>- and \<$\>-environments should be valid LaTeX code. TexErupter processes these environments and externalizes them with an appropriate standalone documentclass in LaTeX. The resulting multipage PDF is converted to one SVG File per equation. The \<tex\>- and \<$\>-environments are replaced by appropriate \<img\>-tags that reference the SVG of the respective equation.

Compiling
---------
Dependencies:
 - Haskell Platform (https://www.haskell.org/platform/)
 - pdflatex (included in Tex Live or MacTex, https://www.tug.org)
 - pdf2svg (http://www.cityinthesky.co.uk/opensource/pdf2svg)

The GHC compiler and some modules contained in the Haskell Platform are necessary for compiling TexErupter. A makefile is included in the 'src'-directory. Simply go ahead and type 'make' to obtain the executable 'texerupter'.

Pdflatex and pdf2svg are run-time dependencies. Pdflatex is used to compile the LaTeX-code to PDF and pdf2svg is used to subsequently obtain SVG images from the PDFs.


Usage
--------
The easiest way to trying out texerupter is to simply create a file, say test.html, with the content:

      <$> e^{i \pi} = - 1 </$>

and to run

      $ texerupter test.html

This will generate a file erupted\_test.html and a file test.html.1.svg. Open erupted\_test.html with your webbrowser and view the result. 

The \<$\>-tags are used as inline equations just as the $-environment in LaTeX documents. One issue that arises with inline equations that are rendered as SVG images, is that there's no baseline to adjust the height of the expressions. The solution to this is that an invisible bracket is rendered into each of the inline equations to approximately make the center of an expression the center of the image. However, it is necessary to make the resulting \<img\>-tag vertically aligned by its middle. This can be achieved with a simple CSS statement (see Examples). 

The \<tex\>-environment is used to handle full LaTeX environments. For an actual equation, you can put

      <tex>
         \[ \vec{A} = 5 \vec{e}_x + \vec{e}_y \]
      </tex>

into a file and let TexErupter do its thing. Note that the equation environment needs to be included within the \<tex\>-tags, since no additional insertions will happen if you use the \<tex\>-tags. The contents are simply copied verbatim into a LaTeX-document.

There are a few options that you can pass to TexErupter for help, debuggung and to customize it for your needs. Using TexErupter with the -h option or without any arguments will show the help


      USAGE: texerupter [-hv] [-o FILE] [-f FONTSIZE] [-p PACKAGES] [--img=STRING] [--eq-inline=STRING] FILE 

        -h           --help               Show this help and exit
        -v           --verbose            Verbose and keep LaTeX files
        -o FILE      --output=FILE        Output FILE
        -f FONTSIZE  --fontsize=FONTSIZE  change the font size in LaTeX files
        -p PACKAGES  --package=PACKAGES   Comma separated list of packages to be included in the LaTeX
                     --img=STRING         Attributes for <img>-tags generated from the <tex>-tags
                     --eq-inline=STRING   Attributes for <img>-tags generated from the <$>-tags


The -v option is for verbose output. TexErupter will print some information on what it does and it will show the pdflatex compilation process. It will also halt during the pdflatex compilation if there are any errors reported by pdflatex. By default, TexErupter cleans up all unnecessary intermediate files like the generated tex-file and the subsequent pdflatex aux-, log- and pdf-file. If you specify the -v option, theses files will be kept for debugging purposes.

The -o option will give you the possibility to specify your own filename for the output.

The -f option gives you control over the font size of the rendered LaTeX-document. The value should be about the same as the font size you picked for your HTML. FONTSIZE can be something like 14, 14pt, 14px or 14em. Whatever the package 'anyfontsize' accepts as valid input.

The -p option gives you access into the composition of the external LaTeX-document. You can use a comma separated list (no spaces!) to specify the package names you want to include via \\usepackage{..} in the LaTeX document. For example: 

      $ texerupter -p tikz,pgfplots file.html

if you want to draw with TikZ or do some fancy plotting inside the \<tex\>-environment.

The --img=STRING option is used for adding attributes to the \<img\>-tags generated from \<tex\>-environments. STRING must be properly escaped, e.g. a valid --img option would be
   
      $ texerupter --img="class=\"whateverfloatsyourboat\"" file.html

The --eq-inline=STRING is essentially the same as --img, only that it inserts a string into the \<img\>-tag generated from \<$\>-environments.


Examples
--------
There's an examples-directory containing a few examples. Each example contains a README which holds some additional information as to what happens. The makefiles contained in each example-directory contain the necessary command to build the example. Once TexErupter has been built from its source, you can build all the examples by going to their directories and typing 'make'. This also ensures, that your system is setup correctly.

There are two examples simply for testing the \<$\>- and \<tex\>-tags, then there's an example that shows how inline equations and block equations can be mixed and properly aligned with the rest of your text. The fourth example shows how plotting routines can be invoked to generate rather beautiful SVG-images from a few lines of code with the powerful TikZ/PGFPlots packages.


TODO
-------
   - Add Support for processing of files outside the current working directory
   - Fix the issue that align environments in LaTeX keep having a margin on the left side inside a standalone documentclass.
