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

The GHC compiler and some modules contained in the Haskell Platform are necessary for compiling TexErupter. A makefile is included in the src-directory. Simply go ahead and type 'make' to obtain the executable 'texerupter'.

Pdflatex and pdf2svg are run-time dependencies. Pdflatex is used to compile the LaTeX-code to PDF and pdf2svg is used to subsequently obtain SVG images from the PDFs.


Usage
--------
The easiest way to trying out texerupter is to simply create a file, say test.html, with the content:

      <$> e^{i \pi} = - 1 </$>

and to run

      $ texerupter test.html

This will generate a file erupted\_test.html and a file test.html.1.svg. Open erupted\_test.html with your webbrowser and view the result. 

The \<$\>-tags are used as inline equations just as the $-environment in LaTeX documents. One issue that arises with inline equations that are rendered as SVG images, is that there's no baseline to adjust the height of the expressions. The solution to this is that an invisible bracket is rendered into each of the inline equations to adjust approximately make the center of an expression the center of the image. However, it is necessary to make the resulting \<img\> tag vertically aligned by its middle. This can be achieved with a simple CSS statement (see Examples). 

The \<tex\>-environment is used to handle full LaTeX environments. For an actual equation, you can put

      <tex>
         \[ \vec{A} = 5 \vec{e}_x + \vec{e}_y \]
      </tex>

into a file and let TexErupter do its thing. Note that the equation environment needs to be included within the \<tex\>-tags, since no additional insertions will happen if you use the \<tex\>-tags. The contents are simply copied verbatim into a LaTeX-document.

There are a few options that you can pass to TexErupter for help, debuggung and to customize it for your needs. Using TexErupter with the -h option or without any arguments will show the help

      USAGE: texerupter [-h] [-v] [-o FILE] [-f FONTSIZE] [-p PACKAGE1,PACKAGE2,...] FILE 

        -h           --help               Show this help and exit
        -v           --verbose            Verbose and keep LaTeX files
        -o FILE      --output=FILE        Output FILE
        -f FONTSIZE  --fontsize=FONTSIZE  change the font size in LaTeX files
        -p PACKAGES  --package=PACKAGES   Comma separated list of packages to be included in the LaTeX header

The -v option is for verbose output. TexErupter will print some information on what it does and it will show the pdflatex compilation process. It will also halt during the pdflatex compilation if there are any errors reported by pdflatex.

The -o option will give you the possibility to specify your own filename for the output.

The -f option gives you control over the font size of the rendered LaTeX-document. The value should be about the same as the font size you picked for your HTML. FONTSIZE can be something like 14, 14pt, 14px or 14em. Whatever the package 'anyfontsize' accepts as valid input.

The -p option gives you access into the composition of the external LaTeX-document. You can use a comma separated list (no spaces!) to specify the package names you want to include via \\usepackage{..} in the LaTeX document. For example: 

   texerupter -p tikz,pgfplots file.html

if you want to draw with TikZ or do some fancy plotting inside the \<tex\>-environment.




TODO
-------
   - Command line argument to pass class attribute to \<img\>-tag
