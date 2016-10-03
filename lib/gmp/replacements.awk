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
            print "CFLAGS=\"$$GMP_CFLAGS\""
        } else if ($0 ~ /^LTCFLAGS='/) {
            # handle CFLAGS
            print "LTCFLAGS=\"$$GMP_CFLAGS\""
        } else if ($0 ~ /^S\[\"CFLAGS\"\]=/) {
            print "S[\"CFLAGS\"]=\"$$GMP_CFLAGS\""
            if ($0 ~ /\\$/) {
                next_continuation = 1
            }
        } else if ($0 ~ /^ac_pwd=/) {
            print "ac_pwd='$$GMP_PWD'"
        } else if ($0 ~/^CC='/) {
            match($0, /^CC='(.*)'$/, cc_tmp)
            cc = cc_tmp[1]
            print "CC=\"$$GMP_CC\""
        } else if (cc) {
            sub(cc, "$$GMP_CC", $0)
            print
        } else {
            print
        }
    }
    continuation = next_continuation
}
