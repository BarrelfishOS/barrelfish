#!/usr/bin/gawk -f

BEGIN {
    pr = 0
}

/KNOWN ISSUES/ {
    pr = 0
}

/{SUPPORTED} HARDWARE/ {
    pr = 1
}

{
    if (pr ==1) {
	print
    }
}


