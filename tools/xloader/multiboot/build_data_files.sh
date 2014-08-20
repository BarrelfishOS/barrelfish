#!/bin/bash

if [ $# != 2 ]; then
    echo Expected $0 menu.lst output_prefix
    exit 1
fi

ALIGNMENT=4096

MENU_LST=$1
OUTPUT_PREFIX=$2

ALIGNMENT=4096

# Prefix prepended to each output file within the directory
# $OUTPUT_PREFIX (for safety, this means we can clean the directory
# by removing everything with this prefix)
FILE_PREFIX=mb

# Set up output direcotry
if [ -e $OUTPUT_PREFIX  ] && [ ! -d $OUTPUT_PREFIX ]; then
    echo "  !Error: $OUTPUT_PREFIX exists, but is not a directory"
    exit 1
fi

if [ -d $OUTPUT_PREFIX ]; then
    echo "  Cleaning old directory $OUTPUT_PREFIX" 
    rm -f $OUTPUT_PREFIX/$FILE_PREFIX*
fi

if [ ! -d $OUTPUT_PREFIX/ ]; then
    echo "  Making output directory $OUTPUT_PREFIX"
    mkdir $OUTPUT_PREFIX
fi

# Get list of binaries to translate


MBIMG=$OUTPUT_PREFIX/${FILE_PREFIX}img
MBHEADER=$OUTPUT_PREFIX/${FILE_PREFIX}$MENU_LST


OFFSET=0


echo "  Start parsing multiboot"
while read line           
do           
  # remove leading spaces
  MBMOD=$(echo $line | sed 's/^ *//')
  TYPE=$(echo $MBMOD | cut -f1 -d ' ')
  
  if [[ "$TYPE" == "#" ]]; then
    continue
  fi
  

  if [[ "$TYPE" == "mmap" ]]; then
    echo $line >> $MBHEADER
    continue
  fi

  if [[ "$TYPE" == "module" ]] || [[ "$TYPE" == "kernel" ]]; then
    BIN=$(echo $MBMOD | cut -f2 -d ' ')
    CMD=$(echo $MBMOD | cut -f3-99 -d ' ')
  
    FILESIZE=$(stat -c%s ".$BIN")
    PADDING=$(echo "($ALIGNMENT-($FILESIZE%$ALIGNMENT))" | bc)  
    echo '   +'$BIN ': Size='$FILESIZE', Padding Size='$PADDING' , Offset='$OFFSET
    cat "."$BIN >>  $MBIMG
    for ((i=0; i < $PADDING; i=i+1)) do
       echo -en "\0" >> $MBIMG
    done
    echo $TYPE" "$BIN" "$OFFSET" "$FILESIZE" "$CMD >> $MBHEADER
    OFFSET=$(echo "$OFFSET+$PADDING" | bc)
  fi
done <$MENU_LST	
