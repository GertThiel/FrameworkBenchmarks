#!/usr/bin/expect -f
# DEBUG# exp_internal 1

set timeout -1

spawn ros run -- --script radiance-bootstrap.lisp

# > Welcome to the Radiance bootstrapper.
# > Where should Radiance be installed to?
# [~/radiance/]
expect "*/radiance/] "
send -- "/woo\r"

# > Which dists would you like to use?
# [shirakumo quicklisp]
expect "*shirakumo quicklisp] "
send -- "\r"

# > What hostnames is your machine reachable with?
# [example.com localhost]
expect "*example.com localhost] "
send -- "lvh.me localhost\r"

# > Which port should Radiance run on?
# [8080]
expect "*8080] "
send -- "8080\r"

# > Installation complete.
#
# > Module directory:      /woo/modules/
# > Environment directory: /woo/config/
# > Central configuration: /woo/config/default/radiance-core/radiance-core.conf.lisp
# > Custom setup file:     /woo/setup.lisp
# > Radiance launcher:     /woo/start.lisp
expect "*\r\n > Radiance launcher:     /woo/start.lisp"
