functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "tiger.grm"*)
(*The grammar is written using online variant of tiger language and the link is

https://www.lrde.epita.fr/~tiger/tiger.html *)
(* This is the preamble where you can have arbitrary sml code. For us
it is empty *)


(*#line 19.1 "tiger.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\025\000\003\000\024\000\016\000\023\000\020\000\022\000\
\\022\000\021\000\024\000\020\000\026\000\019\000\027\000\018\000\
\\029\000\017\000\037\000\015\000\042\000\014\000\043\000\013\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\018\000\092\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\023\000\088\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\023\000\168\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\025\000\137\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\035\000\094\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\035\000\112\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\011\000\202\000\012\000\202\000\015\000\202\000\017\000\202\000\
\\018\000\202\000\021\000\202\000\023\000\202\000\025\000\202\000\
\\028\000\202\000\030\000\202\000\031\000\202\000\032\000\202\000\
\\035\000\202\000\039\000\202\000\041\000\202\000\044\000\202\000\
\\045\000\202\000\046\000\202\000\048\000\202\000\050\000\202\000\
\\051\000\202\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\011\000\203\000\012\000\203\000\015\000\203\000\017\000\203\000\
\\018\000\203\000\021\000\203\000\023\000\203\000\025\000\203\000\
\\028\000\203\000\030\000\203\000\031\000\203\000\032\000\203\000\
\\035\000\203\000\039\000\203\000\041\000\203\000\044\000\203\000\
\\045\000\203\000\046\000\203\000\048\000\203\000\050\000\203\000\
\\051\000\203\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\011\000\204\000\012\000\204\000\015\000\204\000\017\000\204\000\
\\018\000\204\000\021\000\204\000\023\000\204\000\025\000\204\000\
\\028\000\204\000\030\000\204\000\031\000\204\000\032\000\204\000\
\\035\000\204\000\039\000\204\000\041\000\204\000\044\000\204\000\
\\045\000\204\000\046\000\204\000\048\000\204\000\050\000\204\000\
\\051\000\204\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\011\000\205\000\012\000\205\000\015\000\205\000\017\000\205\000\
\\018\000\205\000\021\000\205\000\023\000\205\000\025\000\205\000\
\\028\000\205\000\030\000\205\000\031\000\205\000\032\000\205\000\
\\035\000\205\000\039\000\205\000\041\000\205\000\044\000\205\000\
\\045\000\205\000\046\000\205\000\048\000\205\000\050\000\205\000\
\\051\000\205\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\011\000\206\000\012\000\206\000\015\000\206\000\017\000\206\000\
\\018\000\206\000\021\000\206\000\023\000\206\000\025\000\206\000\
\\028\000\206\000\030\000\206\000\031\000\206\000\032\000\206\000\
\\035\000\206\000\039\000\206\000\041\000\206\000\044\000\206\000\
\\045\000\206\000\046\000\206\000\048\000\206\000\050\000\206\000\
\\051\000\206\000\000\000\
\\001\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\011\000\207\000\012\000\207\000\015\000\207\000\017\000\207\000\
\\018\000\207\000\021\000\207\000\023\000\207\000\025\000\207\000\
\\028\000\207\000\030\000\207\000\031\000\207\000\032\000\207\000\
\\035\000\207\000\039\000\207\000\041\000\207\000\044\000\207\000\
\\045\000\207\000\046\000\207\000\048\000\207\000\050\000\207\000\
\\051\000\207\000\000\000\
\\001\000\007\000\076\000\000\000\
\\001\000\007\000\111\000\000\000\
\\001\000\007\000\148\000\047\000\147\000\000\000\
\\001\000\007\000\171\000\000\000\
\\001\000\007\000\173\000\000\000\
\\001\000\007\000\176\000\047\000\175\000\000\000\
\\001\000\007\000\185\000\000\000\
\\001\000\015\000\000\000\000\000\
\\001\000\019\000\080\000\047\000\079\000\000\000\
\\001\000\019\000\087\000\000\000\
\\001\000\019\000\132\000\000\000\
\\001\000\020\000\077\000\000\000\
\\001\000\020\000\078\000\000\000\
\\001\000\020\000\142\000\000\000\
\\001\000\021\000\089\000\000\000\
\\001\000\021\000\113\000\000\000\
\\001\000\021\000\129\000\000\000\
\\001\000\021\000\131\000\000\000\
\\001\000\021\000\140\000\000\000\
\\001\000\021\000\169\000\000\000\
\\001\000\027\000\042\000\000\000\
\\001\000\027\000\043\000\000\000\
\\001\000\027\000\044\000\000\000\
\\001\000\027\000\045\000\000\000\
\\001\000\027\000\047\000\000\000\
\\001\000\027\000\048\000\000\000\
\\001\000\027\000\053\000\000\000\
\\001\000\027\000\059\000\000\000\
\\001\000\027\000\095\000\000\000\
\\001\000\027\000\103\000\040\000\102\000\049\000\101\000\051\000\100\000\000\000\
\\001\000\027\000\107\000\000\000\
\\001\000\027\000\124\000\000\000\
\\001\000\027\000\143\000\000\000\
\\001\000\027\000\145\000\000\000\
\\001\000\027\000\149\000\000\000\
\\001\000\027\000\162\000\000\000\
\\001\000\027\000\166\000\000\000\
\\001\000\027\000\167\000\000\000\
\\001\000\027\000\172\000\000\000\
\\001\000\027\000\181\000\000\000\
\\001\000\027\000\183\000\000\000\
\\001\000\030\000\081\000\000\000\
\\001\000\031\000\133\000\000\000\
\\001\000\038\000\127\000\000\000\
\\001\000\040\000\075\000\052\000\074\000\000\000\
\\001\000\040\000\121\000\000\000\
\\001\000\040\000\126\000\052\000\125\000\000\000\
\\001\000\040\000\160\000\000\000\
\\001\000\041\000\110\000\000\000\
\\001\000\041\000\122\000\000\000\
\\001\000\041\000\146\000\000\000\
\\001\000\041\000\158\000\000\000\
\\001\000\041\000\161\000\000\000\
\\001\000\041\000\177\000\000\000\
\\001\000\043\000\046\000\000\000\
\\001\000\047\000\130\000\000\000\
\\001\000\047\000\179\000\000\000\
\\189\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\190\000\000\000\
\\191\000\001\000\025\000\003\000\024\000\016\000\023\000\020\000\022\000\
\\022\000\021\000\024\000\020\000\026\000\019\000\027\000\018\000\
\\029\000\017\000\037\000\015\000\042\000\014\000\043\000\013\000\000\000\
\\192\000\000\000\
\\193\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\028\000\091\000\000\000\
\\194\000\000\000\
\\195\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\196\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\197\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\198\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\017\000\139\000\000\000\
\\199\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\208\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\013\000\030\000\000\000\
\\209\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\013\000\030\000\000\000\
\\210\000\000\000\
\\211\000\000\000\
\\212\000\004\000\039\000\005\000\038\000\000\000\
\\213\000\004\000\039\000\005\000\038\000\000\000\
\\214\000\000\000\
\\215\000\000\000\
\\216\000\000\000\
\\217\000\019\000\028\000\034\000\027\000\036\000\026\000\000\000\
\\218\000\000\000\
\\219\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\000\000\
\\224\000\000\000\
\\225\000\001\000\025\000\003\000\024\000\016\000\023\000\020\000\022\000\
\\022\000\021\000\024\000\020\000\026\000\019\000\027\000\018\000\
\\029\000\017\000\037\000\015\000\042\000\014\000\043\000\013\000\000\000\
\\226\000\000\000\
\\227\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\039\000\115\000\000\000\
\\228\000\000\000\
\\229\000\027\000\083\000\000\000\
\\230\000\000\000\
\\231\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\039\000\153\000\000\000\
\\232\000\000\000\
\\233\000\001\000\025\000\003\000\024\000\016\000\023\000\020\000\022\000\
\\022\000\021\000\024\000\020\000\026\000\019\000\027\000\018\000\
\\029\000\017\000\032\000\016\000\037\000\015\000\042\000\014\000\
\\043\000\013\000\044\000\012\000\045\000\011\000\046\000\010\000\
\\048\000\009\000\051\000\008\000\000\000\
\\233\000\032\000\016\000\044\000\012\000\045\000\011\000\046\000\010\000\
\\048\000\009\000\051\000\008\000\000\000\
\\234\000\000\000\
\\235\000\000\000\
\\236\000\000\000\
\\237\000\047\000\150\000\000\000\
\\238\000\000\000\
\\239\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\240\000\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\241\000\000\000\
\\242\000\000\000\
\\243\000\000\000\
\\244\000\000\000\
\\245\000\000\000\
\\246\000\000\000\
\\247\000\000\000\
\\248\000\027\000\105\000\000\000\
\\249\000\000\000\
\\250\000\039\000\165\000\000\000\
\\251\000\000\000\
\\252\000\020\000\052\000\034\000\051\000\040\000\050\000\000\000\
\\253\000\020\000\093\000\000\000\
\\254\000\000\000\
\\255\000\038\000\135\000\000\000\
\\000\001\032\000\016\000\050\000\099\000\000\000\
\\001\001\000\000\
\\002\001\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\003\001\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\004\001\000\000\
\\005\001\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\\006\001\002\000\041\000\003\000\040\000\004\000\039\000\005\000\038\000\
\\006\000\037\000\007\000\036\000\008\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\000\000\
\"
val actionRowNumbers =
"\108\000\111\000\092\000\109\000\
\\071\000\070\000\033\000\034\000\
\\035\000\036\000\067\000\099\000\
\\037\000\093\000\038\000\109\000\
\\128\000\081\000\039\000\000\000\
\\072\000\000\000\000\000\089\000\
\\040\000\000\000\000\000\110\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\057\000\013\000\024\000\025\000\
\\112\000\096\000\021\000\054\000\
\\104\000\000\000\100\000\022\000\
\\002\000\027\000\074\000\001\000\
\\091\000\129\000\005\000\076\000\
\\008\000\084\000\083\000\012\000\
\\011\000\009\000\007\000\010\000\
\\086\000\085\000\088\000\087\000\
\\041\000\132\000\042\000\124\000\
\\124\000\043\000\000\000\072\000\
\\061\000\014\000\006\000\028\000\
\\102\000\000\000\000\000\090\000\
\\073\000\000\000\000\000\100\000\
\\130\000\058\000\136\000\062\000\
\\132\000\044\000\059\000\056\000\
\\124\000\117\000\029\000\068\000\
\\030\000\023\000\137\000\055\000\
\\095\000\000\000\131\000\097\000\
\\101\000\000\000\004\000\077\000\
\\074\000\079\000\031\000\132\000\
\\122\000\133\000\026\000\045\000\
\\132\000\046\000\063\000\015\000\
\\047\000\113\000\000\000\082\000\
\\106\000\000\000\102\000\000\000\
\\075\000\000\000\098\000\064\000\
\\124\000\060\000\065\000\119\000\
\\118\000\048\000\000\000\126\000\
\\049\000\138\000\105\000\050\000\
\\094\000\103\000\003\000\080\000\
\\123\000\032\000\132\000\121\000\
\\016\000\115\000\125\000\051\000\
\\114\000\017\000\000\000\018\000\
\\066\000\000\000\069\000\000\000\
\\078\000\052\000\000\000\120\000\
\\116\000\053\000\106\000\019\000\
\\134\000\126\000\107\000\000\000\
\\127\000\135\000\020\000"
val gotoT =
"\
\\001\000\005\000\003\000\186\000\004\000\004\000\005\000\003\000\
\\006\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\027\000\005\000\003\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\047\000\005\000\003\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\052\000\006\000\002\000\000\000\
\\001\000\054\000\002\000\053\000\006\000\002\000\000\000\
\\001\000\055\000\006\000\002\000\000\000\
\\001\000\056\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\001\000\058\000\006\000\002\000\000\000\
\\001\000\059\000\006\000\002\000\000\000\
\\000\000\
\\001\000\060\000\006\000\002\000\000\000\
\\001\000\061\000\006\000\002\000\000\000\
\\001\000\062\000\006\000\002\000\000\000\
\\001\000\063\000\006\000\002\000\000\000\
\\001\000\064\000\006\000\002\000\000\000\
\\001\000\065\000\006\000\002\000\000\000\
\\001\000\066\000\006\000\002\000\000\000\
\\001\000\067\000\006\000\002\000\000\000\
\\001\000\068\000\006\000\002\000\000\000\
\\001\000\069\000\006\000\002\000\000\000\
\\001\000\070\000\006\000\002\000\000\000\
\\001\000\071\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\080\000\000\000\
\\001\000\082\000\006\000\002\000\000\000\
\\001\000\084\000\006\000\002\000\010\000\083\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\088\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\096\000\016\000\095\000\017\000\094\000\000\000\
\\000\000\
\\014\000\102\000\000\000\
\\014\000\104\000\000\000\
\\000\000\
\\001\000\106\000\006\000\002\000\000\000\
\\001\000\054\000\002\000\107\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\112\000\000\000\
\\001\000\114\000\006\000\002\000\000\000\
\\001\000\115\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\001\000\116\000\006\000\002\000\000\000\
\\001\000\117\000\006\000\002\000\000\000\
\\001\000\084\000\006\000\002\000\010\000\118\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\096\000\016\000\121\000\017\000\094\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\126\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\132\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\134\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\012\000\136\000\000\000\
\\000\000\
\\000\000\
\\015\000\096\000\016\000\139\000\017\000\094\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\096\000\016\000\142\000\017\000\094\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\149\000\006\000\002\000\000\000\
\\000\000\
\\009\000\150\000\000\000\
\\001\000\152\000\006\000\002\000\000\000\
\\011\000\153\000\000\000\
\\001\000\154\000\006\000\002\000\000\000\
\\000\000\
\\001\000\155\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\014\000\157\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\161\000\006\000\002\000\000\000\
\\013\000\162\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\096\000\016\000\168\000\017\000\094\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\172\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\001\000\176\000\006\000\002\000\000\000\
\\000\000\
\\001\000\178\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\001\000\180\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\182\000\000\000\
\\000\000\
\\000\000\
\\013\000\184\000\000\000\
\\000\000\
\\001\000\185\000\006\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 187
val numrules = 74
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | QUOTE of  (string) | ID of  (string) | CONST of  (int) | VARDEC of  (Ast.Vdec) | CLASSFIELDS of  (Ast.Classfield list) | CLASSFIELD of  (Ast.Classfield) | TYFIELDS of  ( ( string*string )  list) | TYFIELDSTAIL of  ( ( string*string )  list) | EXPTAIL of  (Ast.Expr list) | ARGTAIL of  (Ast.Expr list) | ARGUMENTS of  (Ast.Expr list) | RECBODYTAIL of  ( ( string * Ast.Expr )  list) | RECBODY of  ( ( string * Ast.Expr )  list) | Record of  (Ast.Expr) | LVALUE of  (Ast.Expr) | DEC of  (Ast.Dec) | DECS of  (Ast.Dec list) | PROGRAM of  (Ast.Program) | EXPS of  (Ast.Expr list) | EXP of  (Ast.Expr)
end
type svalue = MlyValue.svalue
type result = Ast.Program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 14) => true | _ => false
val showTerminal =
fn (T 0) => "CONST"
  | (T 1) => "PLUS"
  | (T 2) => "MINUS"
  | (T 3) => "MUL"
  | (T 4) => "DIV"
  | (T 5) => "LT"
  | (T 6) => "EQ"
  | (T 7) => "LTEQ"
  | (T 8) => "GTEQ"
  | (T 9) => "GT"
  | (T 10) => "AND"
  | (T 11) => "OR"
  | (T 12) => "NTEQ"
  | (T 13) => "NEWLINE"
  | (T 14) => "EOF"
  | (T 15) => "IF"
  | (T 16) => "ELSE"
  | (T 17) => "THEN"
  | (T 18) => "ASSIGN"
  | (T 19) => "LPAREN"
  | (T 20) => "RPAREN"
  | (T 21) => "WHILE"
  | (T 22) => "DO"
  | (T 23) => "FOR"
  | (T 24) => "TO"
  | (T 25) => "BREAK"
  | (T 26) => "ID"
  | (T 27) => "SEMICOLON"
  | (T 28) => "LET"
  | (T 29) => "IN"
  | (T 30) => "END"
  | (T 31) => "VAR"
  | (T 32) => "UMINUS"
  | (T 33) => "LBRACK"
  | (T 34) => "RBRACK"
  | (T 35) => "DOT"
  | (T 36) => "NIL"
  | (T 37) => "OF"
  | (T 38) => "COMMA"
  | (T 39) => "LBRACE"
  | (T 40) => "RBRACE"
  | (T 41) => "NEW"
  | (T 42) => "QUOTE"
  | (T 43) => "IMPORT"
  | (T 44) => "PRIMITIVE"
  | (T 45) => "FUNCTION"
  | (T 46) => "COLON"
  | (T 47) => "TYPE"
  | (T 48) => "ARRAY"
  | (T 49) => "METHOD"
  | (T 50) => "CLASS"
  | (T 51) => "EXTENDS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 51) $$ (T 50) $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 107.30 "tiger.grm"*)Ast.Foo EXP (*#line 657.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.DECS DECS, DECS1left, DECS1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 108.15 "tiger.grm"*)Ast.Bar DECS(*#line 661.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, DECS1left, DECS1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 110.33 "tiger.grm"*) []                  (*#line 665.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXPTAIL EXPTAIL, _, EXPTAIL1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 111.18 "tiger.grm"*)EXP :: EXPTAIL	   (*#line 669.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXPTAIL1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.EXPTAIL ((*#line 112.22 "tiger.grm"*)[](*#line 673.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXPTAIL EXPTAIL, _, EXPTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, SEMICOLON1left, _)) :: rest671)) => let val  result = MlyValue.EXPTAIL ((*#line 113.33 "tiger.grm"*)EXP::EXPTAIL(*#line 677.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, SEMICOLON1left, EXPTAIL1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 115.43 "tiger.grm"*)Ast.Assign (LVALUE, EXP)(*#line 681.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, EXP1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 116.23 "tiger.grm"*)Ast.While EXP1 EXP2(*#line 685.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 117.47 "tiger.grm"*)Ast.For ID EXP1 EXP2 EXP3(*#line 689.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, FOR1left, EXP3right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 118.25 "tiger.grm"*)Ast.Openif EXP1 EXP2 (*#line 693.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 119.30 "tiger.grm"*)Ast.Closedif  EXP1 EXP2 EXP3(*#line 697.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP3right), rest671)
end
|  ( 11, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 120.13 "tiger.grm"*)Ast.BREAK	(*#line 701.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 12, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 121.27 "tiger.grm"*)Ast.LET (DECS, EXPS) (*#line 705.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 122.25 "tiger.grm"*)Ast.equal  EXP1 EXP2(*#line 709.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 123.27 "tiger.grm"*)Ast.notequal  EXP1 EXP2(*#line 713.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 124.27 "tiger.grm"*)Ast.lessequal  EXP1 EXP2(*#line 717.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 125.25 "tiger.grm"*)Ast.less  EXP1 EXP2(*#line 721.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 126.27 "tiger.grm"*)Ast.greatequal  EXP1 EXP2(*#line 725.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 127.25 "tiger.grm"*)Ast.great  EXP1 EXP2(*#line 729.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 128.26 "tiger.grm"*)Ast.logicaland  EXP1 EXP2(*#line 733.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 129.25 "tiger.grm"*)Ast.or  EXP1 EXP2(*#line 737.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 130.26 "tiger.grm"*) Ast.mul   EXP1 EXP2 (*#line 741.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 131.30 "tiger.grm"*)Ast.divide  EXP1 EXP2(*#line 745.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 132.30 "tiger.grm"*) Ast.plus  EXP1 EXP2 (*#line 749.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 133.26 "tiger.grm"*) Ast.minus EXP1 EXP2 (*#line 753.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 134.26 "tiger.grm"*) Ast.Const CONST     (*#line 757.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 135.26 "tiger.grm"*)Ast.Closed EXPS(*#line 761.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( _, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 136.32 "tiger.grm"*)Ast.Neg EXP   (*#line 765.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, MINUS1left, EXP1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, LVALUE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 137.13 "tiger.grm"*)LVALUE(*#line 769.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, LVALUE1right), rest671)
end
|  ( 29, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 138.12 "tiger.grm"*)Ast.NIL(*#line 773.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 139.41 "tiger.grm"*)Ast.Array(ID,EXP1,EXP2) (*#line 777.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, ID1left, EXP2right), rest671)
end
|  ( 31, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.RECBODY RECBODY, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 140.31 "tiger.grm"*)Ast.Record (ID, RECBODY)(*#line 781.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: ( _, ( _, NEW1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 141.14 "tiger.grm"*)Ast.Object ID(*#line 785.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, NEW1left, ID1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ARGUMENTS ARGUMENTS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 142.33 "tiger.grm"*)Ast.FunCall (ID,ARGUMENTS) (*#line 789.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ARGUMENTS ARGUMENTS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 143.46 "tiger.grm"*)Ast.MethodCall (LVALUE,ID,ARGUMENTS) (*#line 793.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, RPAREN1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.QUOTE QUOTE, QUOTE1left, QUOTE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 144.14 "tiger.grm"*)Ast.Quote QUOTE(*#line 797.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, QUOTE1left, QUOTE1right), rest671)
end
|  ( 36, ( rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 145.25 "tiger.grm"*)[](*#line 801.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ARGTAIL ARGTAIL, _, ARGTAIL1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 146.18 "tiger.grm"*)EXP::ARGTAIL(*#line 805.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, EXP1left, ARGTAIL1right), rest671)
end
|  ( 38, ( rest671)) => let val  result = MlyValue.ARGTAIL ((*#line 147.15 "tiger.grm"*)[](*#line 809.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ARGTAIL ARGTAIL, _, ARGTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.ARGTAIL ((*#line 148.23 "tiger.grm"*)EXP::ARGTAIL(*#line 813.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, COMMA1left, ARGTAIL1right), rest671)
end
|  ( 40, ( rest671)) => let val  result = MlyValue.RECBODY ((*#line 149.24 "tiger.grm"*)[](*#line 817.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 41, ( ( _, ( MlyValue.RECBODYTAIL RECBODYTAIL, _, RECBODYTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.RECBODY ((*#line 150.27 "tiger.grm"*)(ID, EXP)::RECBODYTAIL(*#line 821.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, ID1left, RECBODYTAIL1right), rest671)
end
|  ( 42, ( rest671)) => let val  result = MlyValue.RECBODYTAIL ((*#line 151.27 "tiger.grm"*)[](*#line 825.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 43, ( ( _, ( _, _, RECBODYTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.RECBODYTAIL ((*#line 152.32 "tiger.grm"*)(ID, EXP)::[](*#line 829.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, COMMA1left, RECBODYTAIL1right), rest671)
end
|  ( 44, ( rest671)) => let val  result = MlyValue.DECS ((*#line 154.23 "tiger.grm"*)[](*#line 833.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 45, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 155.14 "tiger.grm"*)DEC :: DECS	(*#line 837.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, DEC1left, DECS1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.VARDEC VARDEC, VARDEC1left, VARDEC1right)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 157.20 "tiger.grm"*)Ast.VariableDec VARDEC(*#line 841.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VARDEC1left, VARDEC1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.QUOTE QUOTE, _, QUOTE1right)) :: ( _, ( _, IMPORT1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 158.23 "tiger.grm"*)Ast.Import QUOTE(*#line 845.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, IMPORT1left, QUOTE1right), rest671)
end
|  ( 48, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, PRIMITIVE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 159.42 "tiger.grm"*)Ast.PrimitiveDec (ID,(Ast.Tyfield TYFIELDS) ) (*#line 849.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, PRIMITIVE1left, RPAREN1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, PRIMITIVE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 160.50 "tiger.grm"*)Ast.PrimitiveDecType (ID1,(Ast.Tyfield TYFIELDS),ID2 ) (*#line 853.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, PRIMITIVE1left, ID2right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 161.48 "tiger.grm"*)Ast.FunctionDec(ID,(Ast.Tyfield TYFIELDS),EXP) (*#line 857.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 162.59 "tiger.grm"*)Ast.FunctionDecType(ID1,(Ast.Tyfield TYFIELDS),ID2,EXP) (*#line 861.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 163.22 "tiger.grm"*)Ast.TypeDec (ID1,(Ast.NameTy ID2) )(*#line 865.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, TYPE1left, ID2right), rest671)
end
|  ( 53, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 164.40 "tiger.grm"*)Ast.TypeDec (ID1,(Ast.RecordTy (Ast.Tyfield TYFIELDS) ) )(*#line 869.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, TYPE1left, RBRACE1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 165.29 "tiger.grm"*)Ast.TypeDec (ID1,(Ast.ArrayTy ID2) ) (*#line 873.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, TYPE1left, ID2right), rest671)
end
|  ( 55, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.CLASSFIELDS CLASSFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 166.59 "tiger.grm"*)Ast.TypeDec (ID1,(Ast.ClassDefCanType(ID2, CLASSFIELDS) ) ) (*#line 877.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, TYPE1left, RBRACE1right), rest671)
end
|  ( 56, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.CLASSFIELDS CLASSFIELDS, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 167.47 "tiger.grm"*)Ast.TypeDec (ID,(Ast.ClassDefCan CLASSFIELDS) ) (*#line 881.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, TYPE1left, RBRACE1right), rest671)
end
|  ( 57, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.CLASSFIELDS CLASSFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, CLASS1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 168.40 "tiger.grm"*)Ast.ClassDec (ID,CLASSFIELDS)(*#line 885.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, CLASS1left, RBRACE1right), rest671)
end
|  ( 58, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.CLASSFIELDS CLASSFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, CLASS1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 169.50 "tiger.grm"*)Ast.ClassDecType (ID1,ID2,CLASSFIELDS)(*#line 889.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, CLASS1left, RBRACE1right), rest671)
end
|  ( 59, ( rest671)) => let val  result = MlyValue.TYFIELDS ((*#line 171.25 "tiger.grm"*)[](*#line 893.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 60, ( ( _, ( MlyValue.TYFIELDSTAIL TYFIELDSTAIL, _, TYFIELDSTAIL1right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.TYFIELDS ((*#line 172.29 "tiger.grm"*)(ID1,ID2)::TYFIELDSTAIL(*#line 897.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, ID1left, TYFIELDSTAIL1right), rest671)
end
|  ( 61, ( rest671)) => let val  result = MlyValue.TYFIELDSTAIL ((*#line 173.27 "tiger.grm"*)[](*#line 901.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 62, ( ( _, ( MlyValue.TYFIELDSTAIL TYFIELDSTAIL, _, TYFIELDSTAIL1right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.TYFIELDSTAIL ((*#line 174.34 "tiger.grm"*)(ID1,ID2)::TYFIELDSTAIL(*#line 905.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, COMMA1left, TYFIELDSTAIL1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 176.16 "tiger.grm"*)Ast.Name ID(*#line 909.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 177.19 "tiger.grm"*)Ast.Method ( LVALUE, ID )(*#line 913.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LVALUE1left, ID1right), rest671)
end
|  ( 65, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 178.29 "tiger.grm"*)Ast.Access( LVALUE, EXP)(*#line 917.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LVALUE1left, RBRACK1right), rest671)
end
|  ( 66, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 179.25 "tiger.grm"*)Ast.Access( (Ast.Name ID), EXP)(*#line 921.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 67, ( rest671)) => let val  result = MlyValue.CLASSFIELDS ((*#line 181.27 "tiger.grm"*)[](*#line 925.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 68, ( ( _, ( MlyValue.CLASSFIELDS CLASSFIELDS, _, CLASSFIELDS1right)) :: ( _, ( MlyValue.CLASSFIELD CLASSFIELD, CLASSFIELD1left, _)) :: rest671)) => let val  result = MlyValue.CLASSFIELDS ((*#line 182.28 "tiger.grm"*)CLASSFIELD::CLASSFIELDS(*#line 929.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, CLASSFIELD1left, CLASSFIELDS1right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, METHOD1left, _)) :: rest671)) => let val  result = MlyValue.CLASSFIELD ((*#line 183.55 "tiger.grm"*)Ast.MethodDec (ID,(Ast.Tyfield TYFIELDS),EXP) (*#line 933.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, METHOD1left, EXP1right), rest671)
end
|  ( 70, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, METHOD1left, _)) :: rest671)) => let val  result = MlyValue.CLASSFIELD ((*#line 184.55 "tiger.grm"*)Ast.MethodDecType (ID1,(Ast.Tyfield TYFIELDS),ID2,EXP) (*#line 937.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, METHOD1left, EXP1right), rest671)
end
|  ( 71, ( ( _, ( MlyValue.VARDEC VARDEC, VARDEC1left, VARDEC1right)) :: rest671)) => let val  result = MlyValue.CLASSFIELD ((*#line 185.19 "tiger.grm"*)Ast.ClassAttribute VARDEC(*#line 941.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, VARDEC1left, VARDEC1right), rest671)
end
|  ( 72, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.VARDEC ((*#line 186.34 "tiger.grm"*)Ast.VarDec(ID, EXP) (*#line 945.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, VAR1left, EXP1right), rest671)
end
|  ( 73, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.VARDEC ((*#line 187.35 "tiger.grm"*)Ast.VarDecType(ID1, ID2, EXP) (*#line 949.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, VAR1left, EXP1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.CONST i,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun LTEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun GTEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun NTEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.ID i,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(ParserData.MlyValue.VOID,p1,p2))
fun NEW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(ParserData.MlyValue.VOID,p1,p2))
fun QUOTE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(ParserData.MlyValue.QUOTE i,p1,p2))
fun IMPORT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(ParserData.MlyValue.VOID,p1,p2))
fun PRIMITIVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(ParserData.MlyValue.VOID,p1,p2))
fun METHOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(ParserData.MlyValue.VOID,p1,p2))
fun CLASS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(ParserData.MlyValue.VOID,p1,p2))
fun EXTENDS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(ParserData.MlyValue.VOID,p1,p2))
end
end
