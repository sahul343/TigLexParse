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
(*#line 1.2 "tiger.grm"*)(* This is the preamble where you can have arbitrary sml code. For us
it is empty *)


(*#line 15.1 "tiger.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\023\000\003\000\022\000\016\000\021\000\020\000\020\000\
\\022\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\
\\029\000\015\000\037\000\013\000\042\000\012\000\043\000\011\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\018\000\083\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\021\000\082\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\023\000\081\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\023\000\141\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\025\000\117\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\035\000\085\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\035\000\097\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\011\000\166\000\012\000\166\000\015\000\166\000\017\000\166\000\
\\018\000\166\000\021\000\166\000\023\000\166\000\025\000\166\000\
\\028\000\166\000\030\000\166\000\031\000\166\000\032\000\166\000\
\\035\000\166\000\039\000\166\000\041\000\166\000\044\000\166\000\
\\045\000\166\000\046\000\166\000\047\000\166\000\048\000\166\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\011\000\167\000\012\000\167\000\015\000\167\000\017\000\167\000\
\\018\000\167\000\021\000\167\000\023\000\167\000\025\000\167\000\
\\028\000\167\000\030\000\167\000\031\000\167\000\032\000\167\000\
\\035\000\167\000\039\000\167\000\041\000\167\000\044\000\167\000\
\\045\000\167\000\046\000\167\000\047\000\167\000\048\000\167\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\011\000\168\000\012\000\168\000\015\000\168\000\017\000\168\000\
\\018\000\168\000\021\000\168\000\023\000\168\000\025\000\168\000\
\\028\000\168\000\030\000\168\000\031\000\168\000\032\000\168\000\
\\035\000\168\000\039\000\168\000\041\000\168\000\044\000\168\000\
\\045\000\168\000\046\000\168\000\047\000\168\000\048\000\168\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\011\000\169\000\012\000\169\000\015\000\169\000\017\000\169\000\
\\018\000\169\000\021\000\169\000\023\000\169\000\025\000\169\000\
\\028\000\169\000\030\000\169\000\031\000\169\000\032\000\169\000\
\\035\000\169\000\039\000\169\000\041\000\169\000\044\000\169\000\
\\045\000\169\000\046\000\169\000\047\000\169\000\048\000\169\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\011\000\170\000\012\000\170\000\015\000\170\000\017\000\170\000\
\\018\000\170\000\021\000\170\000\023\000\170\000\025\000\170\000\
\\028\000\170\000\030\000\170\000\031\000\170\000\032\000\170\000\
\\035\000\170\000\039\000\170\000\041\000\170\000\044\000\170\000\
\\045\000\170\000\046\000\170\000\047\000\170\000\048\000\170\000\000\000\
\\001\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\011\000\171\000\012\000\171\000\015\000\171\000\017\000\171\000\
\\018\000\171\000\021\000\171\000\023\000\171\000\025\000\171\000\
\\028\000\171\000\030\000\171\000\031\000\171\000\032\000\171\000\
\\035\000\171\000\039\000\171\000\041\000\171\000\044\000\171\000\
\\045\000\171\000\046\000\171\000\047\000\171\000\048\000\171\000\000\000\
\\001\000\007\000\070\000\000\000\
\\001\000\007\000\096\000\000\000\
\\001\000\007\000\123\000\047\000\122\000\000\000\
\\001\000\007\000\142\000\000\000\
\\001\000\007\000\144\000\000\000\
\\001\000\015\000\000\000\000\000\
\\001\000\019\000\073\000\000\000\
\\001\000\019\000\080\000\000\000\
\\001\000\020\000\071\000\000\000\
\\001\000\020\000\072\000\000\000\
\\001\000\021\000\098\000\000\000\
\\001\000\021\000\107\000\000\000\
\\001\000\021\000\109\000\000\000\
\\001\000\021\000\119\000\000\000\
\\001\000\027\000\040\000\000\000\
\\001\000\027\000\041\000\000\000\
\\001\000\027\000\042\000\000\000\
\\001\000\027\000\044\000\000\000\
\\001\000\027\000\045\000\000\000\
\\001\000\027\000\050\000\000\000\
\\001\000\027\000\055\000\000\000\
\\001\000\027\000\088\000\040\000\087\000\049\000\086\000\000\000\
\\001\000\027\000\120\000\000\000\
\\001\000\027\000\124\000\000\000\
\\001\000\027\000\126\000\000\000\
\\001\000\027\000\134\000\000\000\
\\001\000\027\000\138\000\000\000\
\\001\000\027\000\140\000\000\000\
\\001\000\027\000\143\000\000\000\
\\001\000\027\000\149\000\000\000\
\\001\000\030\000\074\000\000\000\
\\001\000\031\000\111\000\000\000\
\\001\000\038\000\105\000\000\000\
\\001\000\041\000\095\000\000\000\
\\001\000\041\000\121\000\000\000\
\\001\000\043\000\043\000\000\000\
\\001\000\047\000\108\000\000\000\
\\001\000\047\000\147\000\000\000\
\\153\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\000\000\
\\154\000\000\000\
\\155\000\001\000\023\000\003\000\022\000\016\000\021\000\020\000\020\000\
\\022\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\
\\029\000\015\000\037\000\013\000\042\000\012\000\043\000\011\000\000\000\
\\156\000\000\000\
\\157\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\028\000\113\000\000\000\
\\158\000\000\000\
\\159\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\000\000\
\\160\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\000\000\
\\161\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\000\000\
\\162\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\017\000\118\000\000\000\
\\163\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\172\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\013\000\028\000\000\000\
\\173\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\013\000\028\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\004\000\037\000\005\000\036\000\000\000\
\\177\000\004\000\037\000\005\000\036\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\019\000\026\000\034\000\025\000\036\000\024\000\000\000\
\\182\000\000\000\
\\183\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\001\000\023\000\003\000\022\000\016\000\021\000\020\000\020\000\
\\022\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\
\\029\000\015\000\037\000\013\000\042\000\012\000\043\000\011\000\000\000\
\\190\000\000\000\
\\191\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\039\000\100\000\000\000\
\\192\000\000\000\
\\193\000\027\000\076\000\000\000\
\\194\000\000\000\
\\195\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\039\000\129\000\000\000\
\\196\000\000\000\
\\197\000\001\000\023\000\003\000\022\000\016\000\021\000\020\000\020\000\
\\022\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\
\\029\000\015\000\032\000\014\000\037\000\013\000\042\000\012\000\
\\043\000\011\000\044\000\010\000\045\000\009\000\046\000\008\000\
\\048\000\007\000\000\000\
\\197\000\032\000\014\000\044\000\010\000\045\000\009\000\046\000\008\000\
\\048\000\007\000\000\000\
\\198\000\000\000\
\\199\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\
\\047\000\110\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\047\000\125\000\000\000\
\\203\000\000\000\
\\204\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\000\000\
\\205\000\002\000\039\000\003\000\038\000\004\000\037\000\005\000\036\000\
\\006\000\035\000\007\000\034\000\008\000\033\000\009\000\032\000\
\\010\000\031\000\011\000\030\000\012\000\029\000\013\000\028\000\000\000\
\\206\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\027\000\090\000\000\000\
\\210\000\000\000\
\\211\000\039\000\137\000\000\000\
\\212\000\000\000\
\\213\000\020\000\049\000\034\000\048\000\040\000\047\000\000\000\
\\214\000\020\000\084\000\000\000\
\\215\000\000\000\
\\216\000\038\000\115\000\000\000\
\"
val actionRowNumbers =
"\090\000\074\000\091\000\053\000\
\\052\000\028\000\029\000\030\000\
\\049\000\081\000\031\000\075\000\
\\032\000\091\000\107\000\063\000\
\\033\000\000\000\000\000\000\000\
\\000\000\071\000\034\000\000\000\
\\000\000\092\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\014\000\022\000\
\\023\000\095\000\078\000\020\000\
\\044\000\086\000\000\000\082\000\
\\021\000\003\000\002\000\001\000\
\\073\000\108\000\006\000\058\000\
\\009\000\066\000\065\000\013\000\
\\012\000\010\000\008\000\011\000\
\\068\000\067\000\070\000\069\000\
\\035\000\103\000\103\000\000\000\
\\054\000\047\000\015\000\007\000\
\\024\000\084\000\000\000\000\000\
\\072\000\000\000\082\000\109\000\
\\046\000\103\000\100\000\025\000\
\\050\000\026\000\093\000\045\000\
\\056\000\077\000\000\000\110\000\
\\079\000\083\000\000\000\005\000\
\\059\000\061\000\027\000\036\000\
\\048\000\016\000\037\000\096\000\
\\038\000\064\000\055\000\000\000\
\\088\000\000\000\084\000\000\000\
\\000\000\080\000\102\000\101\000\
\\039\000\000\000\105\000\040\000\
\\094\000\056\000\087\000\041\000\
\\076\000\085\000\004\000\062\000\
\\017\000\098\000\104\000\042\000\
\\097\000\057\000\018\000\000\000\
\\000\000\051\000\000\000\060\000\
\\099\000\043\000\088\000\105\000\
\\089\000\106\000\019\000"
val gotoT =
"\
\\001\000\004\000\003\000\150\000\004\000\003\000\005\000\002\000\
\\006\000\001\000\000\000\
\\000\000\
\\004\000\025\000\005\000\002\000\000\000\
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
\\004\000\044\000\005\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\049\000\006\000\001\000\000\000\
\\001\000\050\000\006\000\001\000\000\000\
\\001\000\051\000\006\000\001\000\000\000\
\\001\000\052\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\054\000\006\000\001\000\000\000\
\\001\000\055\000\006\000\001\000\000\000\
\\000\000\
\\001\000\056\000\006\000\001\000\000\000\
\\001\000\057\000\006\000\001\000\000\000\
\\001\000\058\000\006\000\001\000\000\000\
\\001\000\059\000\006\000\001\000\000\000\
\\001\000\060\000\006\000\001\000\000\000\
\\001\000\061\000\006\000\001\000\000\000\
\\001\000\062\000\006\000\001\000\000\000\
\\001\000\063\000\006\000\001\000\000\000\
\\001\000\064\000\006\000\001\000\000\000\
\\001\000\065\000\006\000\001\000\000\000\
\\001\000\066\000\006\000\001\000\000\000\
\\001\000\067\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\073\000\000\000\
\\001\000\075\000\006\000\001\000\000\000\
\\001\000\077\000\006\000\001\000\010\000\076\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\014\000\087\000\000\000\
\\014\000\089\000\000\000\
\\001\000\090\000\006\000\001\000\000\000\
\\001\000\092\000\002\000\091\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\097\000\000\000\
\\001\000\099\000\006\000\001\000\000\000\
\\001\000\100\000\006\000\001\000\000\000\
\\000\000\
\\001\000\101\000\006\000\001\000\000\000\
\\001\000\077\000\006\000\001\000\010\000\102\000\000\000\
\\000\000\
\\000\000\
\\014\000\104\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\110\000\000\000\
\\000\000\
\\001\000\112\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\114\000\006\000\001\000\000\000\
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
\\001\000\125\000\006\000\001\000\000\000\
\\009\000\126\000\000\000\
\\001\000\128\000\006\000\001\000\000\000\
\\011\000\129\000\000\000\
\\001\000\130\000\006\000\001\000\000\000\
\\001\000\131\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\133\000\006\000\001\000\000\000\
\\013\000\134\000\000\000\
\\000\000\
\\000\000\
\\012\000\137\000\000\000\
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
\\001\000\143\000\006\000\001\000\000\000\
\\001\000\144\000\006\000\001\000\000\000\
\\000\000\
\\001\000\146\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\148\000\000\000\
\\013\000\149\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 151
val numrules = 64
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
datatype svalue = VOID | ntVOID of unit | QUOTE of  (string) | IDENTIFIER of  (string) | CONST of  (int) | TYFIELDS of  ( ( string*string )  list) | TYFIELDSTAIL of  ( ( string*string )  list) | EXPTAIL of  (Ast.Expr list) | ARGTAIL of  (Ast.Expr list) | ARGUMENTS of  (Ast.Expr list) | RECBODYTAIL of  ( ( string * Ast.Expr )  list) | RECBODY of  ( ( string * Ast.Expr )  list) | Record of  (Ast.Expr) | LVALUE of  (Ast.Expr) | DEC of  (Ast.Dec) | DECS of  (Ast.Dec list) | PROGRAM of  (Ast.Program) | EXPS of  (Ast.Expr list) | EXP of  (Ast.Expr)
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
  | (T 26) => "IDENTIFIER"
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
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 97.30 "tiger.grm"*)Ast.Foo EXP (*#line 569.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.DECS DECS, DECS1left, DECS1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 98.15 "tiger.grm"*)Ast.Bar DECS(*#line 573.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, DECS1left, DECS1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 100.33 "tiger.grm"*) []                  (*#line 577.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXPTAIL EXPTAIL, _, EXPTAIL1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 101.18 "tiger.grm"*)EXP :: EXPTAIL	   (*#line 581.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXPTAIL1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.EXPTAIL ((*#line 102.22 "tiger.grm"*)[](*#line 585.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXPTAIL EXPTAIL, _, EXPTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, SEMICOLON1left, _)) :: rest671)) => let val  result = MlyValue.EXPTAIL ((*#line 103.33 "tiger.grm"*)EXP::EXPTAIL(*#line 589.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, SEMICOLON1left, EXPTAIL1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 105.43 "tiger.grm"*)Ast.Assign (LVALUE, EXP)(*#line 593.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, EXP1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 106.23 "tiger.grm"*)Ast.While EXP1 EXP2(*#line 597.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 107.55 "tiger.grm"*)Ast.For IDENTIFIER EXP1 EXP2 EXP3(*#line 601.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, FOR1left, EXP3right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 108.25 "tiger.grm"*)Ast.Openif EXP1 EXP2 (*#line 605.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 109.30 "tiger.grm"*)Ast.Closedif  EXP1 EXP2 EXP3(*#line 609.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP3right), rest671)
end
|  ( 11, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 110.13 "tiger.grm"*)Ast.BREAK	(*#line 613.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 12, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 111.27 "tiger.grm"*)Ast.LET (DECS, EXPS) (*#line 617.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 112.25 "tiger.grm"*)Ast.equal  EXP1 EXP2(*#line 621.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 113.27 "tiger.grm"*)Ast.notequal  EXP1 EXP2(*#line 625.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 114.27 "tiger.grm"*)Ast.lessequal  EXP1 EXP2(*#line 629.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 115.25 "tiger.grm"*)Ast.less  EXP1 EXP2(*#line 633.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 116.27 "tiger.grm"*)Ast.greatequal  EXP1 EXP2(*#line 637.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 117.25 "tiger.grm"*)Ast.great  EXP1 EXP2(*#line 641.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 118.26 "tiger.grm"*)Ast.logicaland  EXP1 EXP2(*#line 645.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 119.25 "tiger.grm"*)Ast.or  EXP1 EXP2(*#line 649.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 120.26 "tiger.grm"*) Ast.mul   EXP1 EXP2 (*#line 653.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 121.30 "tiger.grm"*)Ast.divide  EXP1 EXP2(*#line 657.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 122.30 "tiger.grm"*) Ast.plus  EXP1 EXP2 (*#line 661.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 123.26 "tiger.grm"*) Ast.minus EXP1 EXP2 (*#line 665.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 124.26 "tiger.grm"*) Ast.Const CONST     (*#line 669.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 125.25 "tiger.grm"*)EXP(*#line 673.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( _, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 126.32 "tiger.grm"*)Ast.Neg EXP   (*#line 677.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, MINUS1left, EXP1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, LVALUE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 127.13 "tiger.grm"*)LVALUE(*#line 681.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, LVALUE1right), rest671)
end
|  ( 29, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 128.12 "tiger.grm"*)Ast.NIL(*#line 685.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 129.49 "tiger.grm"*)Ast.Array(IDENTIFIER,EXP1,EXP2) (*#line 689.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, EXP2right), rest671)
end
|  ( 31, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.RECBODY RECBODY, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 130.39 "tiger.grm"*)Ast.Record (IDENTIFIER, RECBODY)(*#line 693.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, RBRACE1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, IDENTIFIER1right)) :: ( _, ( _, NEW1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 131.22 "tiger.grm"*)Ast.Object IDENTIFIER(*#line 697.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, NEW1left, IDENTIFIER1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ARGUMENTS ARGUMENTS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 132.41 "tiger.grm"*)Ast.FunCall (IDENTIFIER,ARGUMENTS) (*#line 701.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, RPAREN1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ARGUMENTS ARGUMENTS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 133.54 "tiger.grm"*)Ast.MethodCall (LVALUE,IDENTIFIER,ARGUMENTS) (*#line 705.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, RPAREN1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.QUOTE QUOTE, QUOTE1left, QUOTE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 134.14 "tiger.grm"*)Ast.Quote QUOTE(*#line 709.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, QUOTE1left, QUOTE1right), rest671)
end
|  ( 36, ( rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 135.25 "tiger.grm"*)[](*#line 713.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ARGTAIL ARGTAIL, _, ARGTAIL1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 136.18 "tiger.grm"*)EXP::ARGTAIL(*#line 717.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, EXP1left, ARGTAIL1right), rest671)
end
|  ( 38, ( rest671)) => let val  result = MlyValue.ARGTAIL ((*#line 137.15 "tiger.grm"*)[](*#line 721.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ARGTAIL ARGTAIL, _, ARGTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.ARGTAIL ((*#line 138.23 "tiger.grm"*)EXP::ARGTAIL(*#line 725.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, COMMA1left, ARGTAIL1right), rest671)
end
|  ( 40, ( rest671)) => let val  result = MlyValue.RECBODY ((*#line 139.24 "tiger.grm"*)[](*#line 729.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 41, ( ( _, ( MlyValue.RECBODYTAIL RECBODYTAIL, _, RECBODYTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.RECBODY ((*#line 140.35 "tiger.grm"*)(IDENTIFIER, EXP)::RECBODYTAIL(*#line 733.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, IDENTIFIER1left, RECBODYTAIL1right), rest671)
end
|  ( 42, ( rest671)) => let val  result = MlyValue.RECBODYTAIL ((*#line 141.27 "tiger.grm"*)[](*#line 737.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 43, ( ( _, ( _, _, RECBODYTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.RECBODYTAIL ((*#line 142.40 "tiger.grm"*)(IDENTIFIER, EXP)::[](*#line 741.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, COMMA1left, RECBODYTAIL1right), rest671)
end
|  ( 44, ( rest671)) => let val  result = MlyValue.DECS ((*#line 144.23 "tiger.grm"*)[](*#line 745.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 45, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 145.14 "tiger.grm"*)DEC :: DECS	(*#line 749.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, DEC1left, DECS1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 147.38 "tiger.grm"*)Ast.VarDec(IDENTIFIER, EXP) (*#line 753.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VAR1left, EXP1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER2, _, IDENTIFIER2right)) :: _ :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 148.49 "tiger.grm"*)Ast.VarDecType(IDENTIFIER1, EXP,IDENTIFIER2) (*#line 757.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VAR1left, IDENTIFIER2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.QUOTE QUOTE, _, QUOTE1right)) :: ( _, ( _, IMPORT1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 149.23 "tiger.grm"*)Ast.Import QUOTE(*#line 761.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, IMPORT1left, QUOTE1right), rest671)
end
|  ( 49, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, PRIMITIVE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 150.50 "tiger.grm"*)Ast.PrimitiveDec (IDENTIFIER,(Ast.Tyfield TYFIELDS) ) (*#line 765.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, PRIMITIVE1left, RPAREN1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER2, _, IDENTIFIER2right)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, PRIMITIVE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 151.66 "tiger.grm"*)Ast.PrimitiveDecType (IDENTIFIER1,(Ast.Tyfield TYFIELDS),IDENTIFIER2 ) (*#line 769.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, PRIMITIVE1left, IDENTIFIER2right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 152.56 "tiger.grm"*)Ast.FunctionDec(IDENTIFIER,(Ast.Tyfield TYFIELDS),EXP) (*#line 773.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 153.75 "tiger.grm"*)Ast.FunctionDecType(IDENTIFIER1,(Ast.Tyfield TYFIELDS),IDENTIFIER2,EXP) (*#line 777.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER2, _, IDENTIFIER2right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 154.38 "tiger.grm"*)Ast.TypeDec (IDENTIFIER1,(Ast.NameTy IDENTIFIER2) )(*#line 781.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, TYPE1left, IDENTIFIER2right), rest671)
end
|  ( 54, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 155.48 "tiger.grm"*)Ast.TypeDec (IDENTIFIER1,(Ast.RecordTy (Ast.Tyfield TYFIELDS) ) )(*#line 785.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, TYPE1left, RBRACE1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER2, _, IDENTIFIER2right)) :: _ :: _ :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 156.45 "tiger.grm"*)Ast.TypeDec (IDENTIFIER1,(Ast.ArrayTy IDENTIFIER2) ) (*#line 789.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, TYPE1left, IDENTIFIER2right), rest671)
end
|  ( 56, ( rest671)) => let val  result = MlyValue.TYFIELDS ((*#line 158.25 "tiger.grm"*)[](*#line 793.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 57, ( ( _, ( MlyValue.TYFIELDSTAIL TYFIELDSTAIL, _, TYFIELDSTAIL1right)) :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER2, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.TYFIELDS ((*#line 159.45 "tiger.grm"*)(IDENTIFIER1,IDENTIFIER2)::TYFIELDSTAIL(*#line 797.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, IDENTIFIER1left, TYFIELDSTAIL1right), rest671)
end
|  ( 58, ( rest671)) => let val  result = MlyValue.TYFIELDSTAIL ((*#line 160.27 "tiger.grm"*)[](*#line 801.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 59, ( ( _, ( MlyValue.TYFIELDSTAIL TYFIELDSTAIL, _, TYFIELDSTAIL1right)) :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER2, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.TYFIELDSTAIL ((*#line 161.50 "tiger.grm"*)(IDENTIFIER1,IDENTIFIER2)::TYFIELDSTAIL(*#line 805.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, COMMA1left, TYFIELDSTAIL1right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 163.24 "tiger.grm"*)Ast.Name IDENTIFIER(*#line 809.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, IDENTIFIER1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 164.27 "tiger.grm"*)Ast.Method ( LVALUE, IDENTIFIER )(*#line 813.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LVALUE1left, IDENTIFIER1right), rest671)
end
|  ( 62, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 165.29 "tiger.grm"*)Ast.Access( LVALUE, EXP)(*#line 817.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LVALUE1left, RBRACK1right), rest671)
end
|  ( 63, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 166.33 "tiger.grm"*)Ast.Access( (Ast.Name IDENTIFIER), EXP)(*#line 821.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, IDENTIFIER1left, RBRACK1right), rest671)
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
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.IDENTIFIER i,p1,p2))
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
end
end
