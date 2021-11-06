discard """
description: '''
These are all reserved words and should raise an error
'''
action: reject
"""

# XXX: this test needs to be broken up into individual files per test or
#      endeavour to write a new test framework, again. Right now it only tests
#      the first error (`addr`) and ignores the rest.

var addr = "test" #[tt Error
    ^ identifier expected, but got 'keyword addr']#

var and = "test" #[tt Error
    ^ identifier expected, but got 'keyword and']#

var as = "test" #[tt Error
    ^ identifier expected, but got 'keyword as']#

var asm = "test" #[tt Error
    ^ identifier expected, but got 'keyword asm']#

var bind = "test" #[tt Error
    ^ identifier expected, but got 'keyword bind']#

var block = "test" #[tt Error
    ^ identifier expected, but got 'keyword block']#

var break = "test" #[tt Error
    ^ identifier expected, but got 'keyword break']#

var case = "test" #[tt Error
    ^ identifier expected, but got 'keyword case']#

var cast = "test" #[tt Error
    ^ identifier expected, but got 'keyword cast']#

var concept = "test" #[tt Error
    ^ identifier expected, but got 'keyword concept']#

var const = "test" #[tt Error
    ^ identifier expected, but got 'keyword const']#

var continue = "test" #[tt Error
    ^ identifier expected, but got 'keyword continue']#

var converter = "test" #[tt Error
    ^ identifier expected, but got 'keyword converter']#

var defer = "test" #[tt Error
    ^ identifier expected, but got 'keyword defer']#

var discard = "test" #[tt Error
    ^ identifier expected, but got 'keyword discard']#

var distinct = "test" #[tt Error
    ^ identifier expected, but got 'keyword distinct']#

var div = "test" #[tt Error
    ^ identifier expected, but got 'keyword div']#

var do = "test" #[tt Error
    ^ identifier expected, but got 'keyword do']#

var elif = "test" #[tt Error
    ^ identifier expected, but got 'keyword elif']#

var else = "test" #[tt Error
    ^ identifier expected, but got 'keyword else']#

var end = "test" #[tt Error
    ^ identifier expected, but got 'keyword end']#

var enum = "test" #[tt Error
    ^ identifier expected, but got 'keyword enum']#

var except = "test" #[tt Error
    ^ identifier expected, but got 'keyword except']#

var export = "test" #[tt Error
    ^ identifier expected, but got 'keyword export']#

var finally = "test" #[tt Error
    ^ identifier expected, but got 'keyword finally']#

var for = "test" #[tt Error
    ^ identifier expected, but got 'keyword for']#

var from = "test" #[tt Error
    ^ identifier expected, but got 'keyword from']#

var func = "test" #[tt Error
    ^ identifier expected, but got 'keyword func']#

var if = "test" #[tt Error
    ^ identifier expected, but got 'keyword if']#

var import = "test" #[tt Error
    ^ identifier expected, but got 'keyword import']#

var in = "test" #[tt Error
    ^ identifier expected, but got 'keyword in']#

var include = "test" #[tt Error
    ^ identifier expected, but got 'keyword include']#

var interface = "test" #[tt Error
    ^ identifier expected, but got 'keyword interface']#

var is = "test" #[tt Error
    ^ identifier expected, but got 'keyword is']#

var isnot = "test" #[tt Error
    ^ identifier expected, but got 'keyword isnot']#

var iterator = "test" #[tt Error
    ^ identifier expected, but got 'keyword iterator']#

var let = "test" #[tt Error
    ^ identifier expected, but got 'keyword let']#

var macro = "test" #[tt Error
    ^ identifier expected, but got 'keyword macro']#

var method = "test" #[tt Error
    ^ identifier expected, but got 'keyword method']#

var mixin = "test" #[tt Error
    ^ identifier expected, but got 'keyword mixin']#

var mod = "test" #[tt Error
    ^ identifier expected, but got 'keyword mod']#

var nil = "test" #[tt Error
    ^ identifier expected, but got 'keyword nil']#

var not = "test" #[tt Error
    ^ identifier expected, but got 'keyword not']#

var notin = "test" #[tt Error
    ^ identifier expected, but got 'keyword notin']#

var object = "test" #[tt Error
    ^ identifier expected, but got 'keyword object']#

var of = "test" #[tt Error
    ^ identifier expected, but got 'keyword of']#

var or = "test" #[tt Error
    ^ identifier expected, but got 'keyword or']#

var out = "test" #[tt Error
    ^ identifier expected, but got 'keyword out']#

var proc = "test" #[tt Error
    ^ identifier expected, but got 'keyword proc']#

var ptr = "test" #[tt Error
    ^ identifier expected, but got 'keyword ptr']#

var raise = "test" #[tt Error
    ^ identifier expected, but got 'keyword raise']#

var ref = "test" #[tt Error
    ^ identifier expected, but got 'keyword ref']#

var return = "test" #[tt Error
    ^ identifier expected, but got 'keyword return']#

var shl = "test" #[tt Error
    ^ identifier expected, but got 'keyword shl']#

var shr = "test" #[tt Error
    ^ identifier expected, but got 'keyword shr']#

var static = "test" #[tt Error
    ^ identifier expected, but got 'keyword static']#

var template = "test" #[tt Error
    ^ identifier expected, but got 'keyword template']#

var try = "test" #[tt Error
    ^ identifier expected, but got 'keyword try']#

var tuple = "test" #[tt Error
    ^ identifier expected, but got 'keyword tuple']#

var type = "test" #[tt Error
    ^ identifier expected, but got 'keyword type']#

var using = "test" #[tt Error
    ^ identifier expected, but got 'keyword using']#

var var = "test" #[tt Error
    ^ identifier expected, but got 'keyword var']#

var when = "test" #[tt Error
    ^ identifier expected, but got 'keyword when']#

var while = "test" #[tt Error
    ^ identifier expected, but got 'keyword while']#

var xor = "test" #[tt Error
    ^ identifier expected, but got 'keyword xor']#

var yield = "test" #[tt Error
    ^ identifier expected, but got 'keyword yield']#
