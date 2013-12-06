#!/bin/bash
set -e
set -x
README="./README.rst"
CONVERTER="rst2latex"
# Generate html from restructured text (assuming docutils are installed)
${CONVERTER} ${README} tmp.tex

# Replace class="title" attribute from H1 tag as it was clashing with something
# in Drupal
#sed -i 's/<h1 class="title">/<h1>/g' tmp.tex

# Remove all lines before <body> tag and after </body> tag.
./cleanTex.awk tmp.tex > readme.tex

rm tmp.tex

