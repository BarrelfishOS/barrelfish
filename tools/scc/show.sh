#!/bin/bash
#
# Shows "VGA" output of a Barrelfish core

# VGA output
case x$1 in
x0) sccDump -d 0x00 0xb8000 7000 -f errorlog.raw > /dev/null;;
x1) sccDump -d 0x00 0x140b8000 7000 -f errorlog.raw >/dev/null;;
x2) sccDump -d 0x00 0x280b8000 7000 -f errorlog.raw >/dev/null;;
x3) sccDump -d 0x00 0x3c0b8000 7000 -f errorlog.raw >/dev/null;;
x4) sccDump -d 0x00 0x500b8000 7000 -f errorlog.raw >/dev/null;;
x5) sccDump -d 0x00 0x640b8000 7000 -f errorlog.raw >/dev/null;;

x6) sccDump -d 0x05 0xb8000 7000 -f errorlog.raw >/dev/null;;
x7) sccDump -d 0x05 0x140b8000 7000 -f errorlog.raw >/dev/null;;
x8) sccDump -d 0x05 0x280b8000 7000 -f errorlog.raw >/dev/null;;
x9) sccDump -d 0x05 0x3c0b8000 7000 -f errorlog.raw >/dev/null;;
x10) sccDump -d 0x05 0x500b8000 7000 -f errorlog.raw >/dev/null;;
x11) sccDump -d 0x05 0x640b8000 7000 -f errorlog.raw >/dev/null;;

x12) sccDump -d 0x00 0x780b8000 7000 -f errorlog.raw >/dev/null;;
x13) sccDump -d 0x00 0x8c0b8000 7000 -f errorlog.raw >/dev/null;;
x14) sccDump -d 0x00 0xa00b8000 7000 -f errorlog.raw >/dev/null;;
x15) sccDump -d 0x00 0xb40b8000 7000 -f errorlog.raw >/dev/null;;
x16) sccDump -d 0x00 0xc80b8000 7000 -f errorlog.raw >/dev/null;;
x17) sccDump -d 0x00 0xdc0b8000 7000 -f errorlog.raw >/dev/null;;

x18) sccDump -d 0x05 0x780b8000 7000 -f errorlog.raw >/dev/null;;
x19) sccDump -d 0x05 0x8c0b8000 7000 -f errorlog.raw >/dev/null;;
x20) sccDump -d 0x05 0xa00b8000 7000 -f errorlog.raw >/dev/null;;
x21) sccDump -d 0x05 0xb40b8000 7000 -f errorlog.raw >/dev/null;;
x22) sccDump -d 0x05 0xc80b8000 7000 -f errorlog.raw >/dev/null;;
x23) sccDump -d 0x05 0xdc0b8000 7000 -f errorlog.raw >/dev/null;;

x24) sccDump -d 0x20 0xb8000 7000 -f errorlog.raw > /dev/null;;
x25) sccDump -d 0x20 0x140b8000 7000 -f errorlog.raw >/dev/null;;
x26) sccDump -d 0x20 0x280b8000 7000 -f errorlog.raw >/dev/null;;
x27) sccDump -d 0x20 0x3c0b8000 7000 -f errorlog.raw >/dev/null;;
x28) sccDump -d 0x20 0x500b8000 7000 -f errorlog.raw >/dev/null;;
x29) sccDump -d 0x20 0x640b8000 7000 -f errorlog.raw >/dev/null;;

x30) sccDump -d 0x25 0xb8000 7000 -f errorlog.raw > /dev/null;;
x31) sccDump -d 0x25 0x140b8000 7000 -f errorlog.raw >/dev/null;;
x32) sccDump -d 0x25 0x280b8000 7000 -f errorlog.raw >/dev/null;;
x33) sccDump -d 0x25 0x3c0b8000 7000 -f errorlog.raw >/dev/null;;
x34) sccDump -d 0x25 0x500b8000 7000 -f errorlog.raw >/dev/null;;
x35) sccDump -d 0x25 0x640b8000 7000 -f errorlog.raw >/dev/null;;

x36) sccDump -d 0x20 0x780b8000 7000 -f errorlog.raw >/dev/null;;
x37) sccDump -d 0x20 0x8c0b8000 7000 -f errorlog.raw >/dev/null;;
x38) sccDump -d 0x20 0xa00b8000 7000 -f errorlog.raw >/dev/null;;
x39) sccDump -d 0x20 0xb40b8000 7000 -f errorlog.raw >/dev/null;;
x40) sccDump -d 0x20 0xc80b8000 7000 -f errorlog.raw >/dev/null;;
x41) sccDump -d 0x20 0xdc0b8000 7000 -f errorlog.raw >/dev/null;;

x42) sccDump -d 0x25 0x780b8000 7000 -f errorlog.raw >/dev/null;;
x43) sccDump -d 0x25 0x8c0b8000 7000 -f errorlog.raw >/dev/null;;
x44) sccDump -d 0x25 0xa00b8000 7000 -f errorlog.raw >/dev/null;;
x45) sccDump -d 0x25 0xb40b8000 7000 -f errorlog.raw >/dev/null;;
x46) sccDump -d 0x25 0xc80b8000 7000 -f errorlog.raw >/dev/null;;
x47) sccDump -d 0x25 0xdc0b8000 7000 -f errorlog.raw >/dev/null;;

x*)
	echo unknown console ID
	exit 1
esac

cat errorlog.raw
exit 0
