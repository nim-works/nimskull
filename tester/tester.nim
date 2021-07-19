#
#
#            Nim Tester
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This is a combination library and program.
##
## Library, when included by a test:
## * the API for which will turn the module into a CLI:
##  - default: when run will describe the test module
##  - can run the tests themselves
##  - can be run as part of a larger pipeline
##  - can be included as a module as part of a larger test program
## * provides access to the tester API for tests
##
## Program, when executing the binary produced:
## * can build, discover, describe, and run tests in different ways
