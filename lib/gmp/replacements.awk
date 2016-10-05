{
    if (continuation) {
        if ($0 ~ /\\$/) {
            # nop 
        } else {
            next_continuation = 0
        }
    }

    if (!continuation) {
        if ($0 ~ /^CFLAGS=/) {
            # handle CFLAGS
            print "CFLAGS=\"$$CFLAGS\""
        } else if ($0 ~ /^LTCFLAGS='/) {
            # handle CFLAGS
            print "LTCFLAGS=\"$$CFLAGS\""
        } else if ($0 ~ /^S\[\"CFLAGS\"\]=/) {
            print "S[\"CFLAGS\"]=\"$$CFLAGS\""
            if ($0 ~ /\\$/) {
                next_continuation = 1
            }
        } else if ($0 ~ /^ac_pwd=/) {
            print "ac_pwd='$$PWD'"
        } else if ($0 ~/^CC='/) {
            match($0, /^CC='(.*)'$/, cc_tmp)
            cc = cc_tmp[1]
            print "CC=\"$$CC\""
        } else if ($0 ~ /^D\[\"HAVE_OBSTACK_VPRINTF\"/) {
            print "#", $0
        } else if (cc) {
            sub(cc, "$$CC", $0)
            print
        } else {
            print
        }
    }
    continuation = next_continuation
}
