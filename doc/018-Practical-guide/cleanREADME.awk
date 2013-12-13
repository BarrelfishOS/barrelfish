#!/usr/bin/gawk -f

BEGIN {
    pr = 0
}

/Known Issues/ {
    pr = 0
}

/Barrelfish Overview/ {
   pr = 1
}

{
    if (pr ==1) {
	print
    }
}


