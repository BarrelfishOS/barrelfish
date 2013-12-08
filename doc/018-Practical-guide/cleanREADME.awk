#!/usr/bin/gawk -f

BEGIN {
    pr = 0
}

/LIKELY FAQs/ {
    pr = 0
}

/SUPPORTED HARDWARE/ {
   pr = 1
}

{
    if (pr ==1) {
	print
    }
}


