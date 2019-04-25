#!/usr/bin/expect -f
# DEBUG# exp_internal 1

set timeout -1

spawn ros run -- --script /woo/start.lisp

#2019-04-25 05:39:38 [INFO ] <RADIANCE>: Startup done.
#*
expect "*<RADIANCE>: Startup done.*\r\n* "
send -- "(quit)\r"
