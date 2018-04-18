/* parser generated by jison 0.4.15 */
/*
  Returns a Parser object of the following structure:

  Parser: {
    yy: {}
  }

  Parser.prototype: {
    yy: {},
    trace: function(),
    symbols_: {associative list: name ==> number},
    terminals_: {associative list: number ==> name},
    productions_: [...],
    performAction: function anonymous(yytext, yyleng, yylineno, yy, yystate, $$, _$),
    table: [...],
    defaultActions: {...},
    parseError: function(str, hash),
    parse: function(input),

    lexer: {
        EOF: 1,
        parseError: function(str, hash),
        setInput: function(input),
        input: function(),
        unput: function(str),
        more: function(),
        less: function(n),
        pastInput: function(),
        upcomingInput: function(),
        showPosition: function(),
        test_match: function(regex_match_array, rule_index),
        next: function(),
        lex: function(),
        begin: function(condition),
        popState: function(),
        _currentRules: function(),
        topState: function(),
        pushState: function(condition),

        options: {
            ranges: boolean           (optional: true ==> token location info will include a .range[] member)
            flex: boolean             (optional: true ==> flex-like lexing behaviour where the rules are tested exhaustively to find the longest match)
            backtrack_lexer: boolean  (optional: true ==> lexer regexes are tested in order and for each matching regex the action code is invoked; the lexer terminates the scan when a token is returned by the action code)
        },

        performAction: function(yy, yy_, $avoiding_name_collisions, YY_START),
        rules: [...],
        conditions: {associative list: name ==> set},
    }
  }


  token location info (@$, _$, etc.): {
    first_line: n,
    last_line: n,
    first_column: n,
    last_column: n,
    range: [start_number, end_number]       (where the numbers are indexes into the input string, regular zero-based)
  }


  the parseError function receives a 'hash' object with these members for lexer and parser errors: {
    text:        (matched text)
    token:       (the produced terminal token, if any)
    line:        (yylineno)
  }
  while parser (grammar) errors will also provide these members, i.e. parser errors deliver a superset of attributes: {
    loc:         (yylloc)
    expected:    (string describing the set of expected tokens)
    recoverable: (boolean: TRUE when the parser has a error recovery rule available for this particular error)
  }
*/
var parser = (function(){
var o=function(k,v,o,l){for(o=o||{},l=k.length;l--;o[k[l]]=v);return o},$V0=[1,12],$V1=[1,13],$V2=[1,14],$V3=[1,15],$V4=[1,17],$V5=[1,18],$V6=[1,19],$V7=[1,20],$V8=[1,21],$V9=[1,25],$Va=[1,26],$Vb=[1,23],$Vc=[1,24],$Vd=[1,22],$Ve=[5,15,16,17,18,20,26,27,28,29,30,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,48,49,53],$Vf=[1,40],$Vg=[2,41],$Vh=[1,63],$Vi=[2,50],$Vj=[2,16],$Vk=[1,69],$Vl=[15,16,17,18,26,27,28,29,30,33,34,43,46,53];
var parser = {trace: function trace() { },
yy: {},
symbols_: {"error":2,"program":3,"exp":4,"EOF":5,"var_exp":6,"intlit_exp":7,"fn_exp":8,"app_exp":9,"prim1_app_exp":10,"prim2_app_exp":11,"list_exp":12,"if_exp":13,"let_exp":14,"VAR":15,"INT":16,"FN":17,"LPAREN":18,"formals":19,"RPAREN":20,"THATRETURNS":21,"moreformals":22,"COMMA":23,"args":24,"prim1_op":25,"SUMLIST":26,"NOT":27,"HD":28,"TL":29,"ISNULL":30,"prim2_op":31,"CONS":32,"ADD1":33,"NEGATIVE":34,"PLUS":35,"TIMES":36,"MINUS":37,"MOD":38,"DIVIDE":39,"BOOLEQ":40,"GT":41,"LT":42,"IF":43,"THEN":44,"ELSE":45,"LET":46,"bindings":47,"IN":48,"END":49,"EQ":50,"prim_args":51,"more_prim_args":52,"LBRACKET":53,"int_list":54,"RBRACKET":55,"more_ints":56,"$accept":0,"$end":1},
terminals_: {2:"error",5:"EOF",15:"VAR",16:"INT",17:"FN",18:"LPAREN",20:"RPAREN",21:"THATRETURNS",23:"COMMA",26:"SUMLIST",27:"NOT",28:"HD",29:"TL",30:"ISNULL",32:"CONS",33:"ADD1",34:"NEGATIVE",35:"PLUS",36:"TIMES",37:"MINUS",38:"MOD",39:"DIVIDE",40:"BOOLEQ",41:"GT",42:"LT",43:"IF",44:"THEN",45:"ELSE",46:"LET",48:"IN",49:"END",50:"EQ",53:"LBRACKET",55:"RBRACKET"},
productions_: [0,[3,2],[4,1],[4,1],[4,1],[4,1],[4,1],[4,1],[4,1],[4,1],[4,1],[6,1],[7,1],[8,6],[19,0],[19,2],[22,0],[22,3],[9,4],[10,4],[10,4],[10,4],[10,4],[10,4],[10,4],[11,5],[11,5],[25,1],[25,1],[31,1],[31,1],[31,1],[31,1],[31,1],[31,1],[31,1],[31,1],[13,6],[14,5],[47,3],[47,4],[24,0],[24,2],[51,0],[51,2],[52,0],[52,3],[12,3],[54,0],[54,2],[56,0],[56,3]],
performAction: function anonymous(yytext, yyleng, yylineno, yy, yystate /* action[1] */, $$ /* vstack */, _$ /* lstack */) {
/* this == yyval */

var $0 = $$.length - 1;
switch (yystate) {
case 1:
 return SLang.absyn.createProgram($$[$0-1]); 
break;
case 2: case 3: case 4: case 5: case 6: case 7: case 8: case 9: case 10: case 27: case 28: case 29: case 30: case 31: case 32: case 33: case 34: case 35: case 36:
 this.$ = $$[$0]; 
break;
case 11:
 this.$ = SLang.absyn.createVarExp( $$[$0] ); 
break;
case 12:
 this.$ =SLang.absyn.createIntExp( $$[$0] ); 
break;
case 13:
 this.$ = SLang.absyn.createFnExp($$[$0-3],$$[$0]); 
break;
case 14: case 41: case 43:
 this.$ = [ ]; 
break;
case 15: case 42:
 var result;
          if ($$[$0] === [ ])
	     result = [ $$[$0-1] ];
          else {
             $$[$0].unshift($$[$0-1]);
             result = $$[$0];
          }
          this.$ = result;
        
break;
case 16: case 45:
 this.$ = [ ] 
break;
case 17:
 $$[$0].unshift($$[$0-1]);
         this.$ = $$[$0]; 
break;
case 18:
  $$[$0-1].unshift("args");
          this.$ = SLang.absyn.createAppExp($$[$0-2],$$[$0-1]); 
break;
case 19:
 this.$ = SLang.absyn.createPrim1AppExp($$[$0-3],$$[$0-1]); 
break;
case 20:
 this.$ = SLang.absyn.createPrim1AppExp("+", $$[$0-1]); 
break;
case 21:
 this.$ = SLang.absyn.createPrim1AppExp("not", $$[$0-1]); 
break;
case 22:
 this.$ = SLang.absyn.createPrim1AppExp("hd",$$[$0-1]); 
break;
case 23:
 this.$ = SLang.absyn.createPrim1AppExp("tl",$$[$0-1]); 
break;
case 24:
 this.$ = SLang.absyn.createPrim1AppExp("isNull",$$[$0-1]); 
break;
case 25:
 this.$ = SLang.absyn.createPrim2AppExp($$[$0-2],$$[$0-3],$$[$0-1]); 
break;
case 26:
 this.$ = SLang.absyn.createPrim2AppExp("::",$$[$0-3],$$[$0-1]); 
break;
case 37:
 this.$ = SLang.absyn.createIfExp("if",$$[$0-4],$$[$0-2],$$[$0]); 
break;
case 38:
  console.log("bindings " + $$[$0-3]);
			console.log($$[$0-3][0]);
			var f = SLang.absyn.createFnExp($$[$0-3][0],$$[$0-1]);
			SLang.absyn.createAppExp(f, $$[$0-3][1]); 
break;
case 39:
 	var a = [$$[$0-2]];
		var b = [$$[$0]];
		this.$ = [a, b]; 
break;
case 40:
 	var a = $$[$0][0].push($$[$0-3]);
		var b = $$[$0][1].push($$[$0-1]);
		console.log([a, b]);
		this.$ = [a, b];	 
break;
case 44: case 46:
 $$[$0].unshift($$[$0-1]); this.$ = $$[$0]; 
break;
case 47:
 this.$ = SLang.absyn.createListExp($$[$0-1]); 
break;
case 48: case 50:
 this.$ = []; 
break;
case 49:
 $$[$0].unshift(parseInt($$[$0-1])); this.$ = $$[$0]; 
break;
case 51:
 $$[$0].unshift(parseInt($$[$0-1])); this.$ = $$[$0];
break;
}
},
table: [{3:1,4:2,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},{1:[3]},{5:[1,27]},o($Ve,[2,2]),o($Ve,[2,3]),o($Ve,[2,4]),o($Ve,[2,5]),o($Ve,[2,6]),o($Ve,[2,7]),o($Ve,[2,8]),o($Ve,[2,9]),o($Ve,[2,10]),o($Ve,[2,11]),o($Ve,[2,12]),{18:[1,28]},{4:29,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},{18:[1,30]},{18:[1,31]},{18:[1,32]},{18:[1,33]},{18:[1,34]},{18:[1,35]},{16:[1,37],54:36,55:[2,48]},{4:38,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},{15:$Vf,47:39},{18:[2,27]},{18:[2,28]},{1:[2,1]},{15:[1,42],19:41,20:[2,14]},{4:46,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,20:$Vg,24:43,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,31:44,32:[1,45],33:$V9,34:$Va,35:[1,47],36:[1,48],37:[1,49],38:[1,50],39:[1,51],40:[1,52],41:[1,53],42:[1,54],43:$Vb,46:$Vc,53:$Vd},{4:55,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},{12:56,53:$Vd},{4:57,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},{12:58,53:$Vd},{12:59,53:$Vd},{12:60,53:$Vd},{55:[1,61]},{23:$Vh,55:$Vi,56:62},{44:[1,64]},{48:[1,65]},{50:[1,66]},{20:[1,67]},{20:$Vj,22:68,23:$Vk},{20:[1,70]},{4:71,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},{12:72,53:$Vd},{4:46,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,20:$Vg,24:73,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},o($Vl,[2,29]),o($Vl,[2,30]),o($Vl,[2,31]),o($Vl,[2,32]),o($Vl,[2,33]),o($Vl,[2,34]),o($Vl,[2,35]),o($Vl,[2,36]),{20:[1,74]},{20:[1,75]},{20:[1,76]},{20:[1,77]},{20:[1,78]},{20:[1,79]},o($Ve,[2,47]),{55:[2,49]},{16:[1,80]},{4:81,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},{4:82,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},{4:83,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},{21:[1,84]},{20:[2,15]},{15:[1,85]},o($Ve,[2,18]),{20:[1,86]},{20:[1,87]},{20:[2,42]},o($Ve,[2,19]),o($Ve,[2,20]),o($Ve,[2,21]),o($Ve,[2,22]),o($Ve,[2,23]),o($Ve,[2,24]),{23:$Vh,55:$Vi,56:88},{45:[1,89]},{49:[1,90]},{15:$Vf,47:91,48:[2,39]},{4:92,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},{20:$Vj,22:93,23:$Vk},o($Ve,[2,25]),o($Ve,[2,26]),{55:[2,51]},{4:94,6:3,7:4,8:5,9:6,10:7,11:8,12:9,13:10,14:11,15:$V0,16:$V1,17:$V2,18:$V3,25:16,26:$V4,27:$V5,28:$V6,29:$V7,30:$V8,33:$V9,34:$Va,43:$Vb,46:$Vc,53:$Vd},o($Ve,[2,38]),{48:[2,40]},o($Ve,[2,13]),{20:[2,17]},o($Ve,[2,37])],
defaultActions: {25:[2,27],26:[2,28],27:[2,1],62:[2,49],68:[2,15],73:[2,42],88:[2,51],91:[2,40],93:[2,17]},
parseError: function parseError(str, hash) {
    if (hash.recoverable) {
        this.trace(str);
    } else {
        throw new Error(str);
    }
},
parse: function parse(input) {
    var self = this, stack = [0], tstack = [], vstack = [null], lstack = [], table = this.table, yytext = '', yylineno = 0, yyleng = 0, recovering = 0, TERROR = 2, EOF = 1;
    var args = lstack.slice.call(arguments, 1);
    var lexer = Object.create(this.lexer);
    var sharedState = { yy: {} };
    for (var k in this.yy) {
        if (Object.prototype.hasOwnProperty.call(this.yy, k)) {
            sharedState.yy[k] = this.yy[k];
        }
    }
    lexer.setInput(input, sharedState.yy);
    sharedState.yy.lexer = lexer;
    sharedState.yy.parser = this;
    if (typeof lexer.yylloc == 'undefined') {
        lexer.yylloc = {};
    }
    var yyloc = lexer.yylloc;
    lstack.push(yyloc);
    var ranges = lexer.options && lexer.options.ranges;
    if (typeof sharedState.yy.parseError === 'function') {
        this.parseError = sharedState.yy.parseError;
    } else {
        this.parseError = Object.getPrototypeOf(this).parseError;
    }
    function popStack(n) {
        stack.length = stack.length - 2 * n;
        vstack.length = vstack.length - n;
        lstack.length = lstack.length - n;
    }
    _token_stack:
        function lex() {
            var token;
            token = lexer.lex() || EOF;
            if (typeof token !== 'number') {
                token = self.symbols_[token] || token;
            }
            return token;
        }
    var symbol, preErrorSymbol, state, action, a, r, yyval = {}, p, len, newState, expected;
    while (true) {
        state = stack[stack.length - 1];
        if (this.defaultActions[state]) {
            action = this.defaultActions[state];
        } else {
            if (symbol === null || typeof symbol == 'undefined') {
                symbol = lex();
            }
            action = table[state] && table[state][symbol];
        }
                    if (typeof action === 'undefined' || !action.length || !action[0]) {
                var errStr = '';
                expected = [];
                for (p in table[state]) {
                    if (this.terminals_[p] && p > TERROR) {
                        expected.push('\'' + this.terminals_[p] + '\'');
                    }
                }
                if (lexer.showPosition) {
                    errStr = 'Parse error on line ' + (yylineno + 1) + ':\n' + lexer.showPosition() + '\nExpecting ' + expected.join(', ') + ', got \'' + (this.terminals_[symbol] || symbol) + '\'';
                } else {
                    errStr = 'Parse error on line ' + (yylineno + 1) + ': Unexpected ' + (symbol == EOF ? 'end of input' : '\'' + (this.terminals_[symbol] || symbol) + '\'');
                }
                this.parseError(errStr, {
                    text: lexer.match,
                    token: this.terminals_[symbol] || symbol,
                    line: lexer.yylineno,
                    loc: yyloc,
                    expected: expected
                });
            }
        if (action[0] instanceof Array && action.length > 1) {
            throw new Error('Parse Error: multiple actions possible at state: ' + state + ', token: ' + symbol);
        }
        switch (action[0]) {
        case 1:
            stack.push(symbol);
            vstack.push(lexer.yytext);
            lstack.push(lexer.yylloc);
            stack.push(action[1]);
            symbol = null;
            if (!preErrorSymbol) {
                yyleng = lexer.yyleng;
                yytext = lexer.yytext;
                yylineno = lexer.yylineno;
                yyloc = lexer.yylloc;
                if (recovering > 0) {
                    recovering--;
                }
            } else {
                symbol = preErrorSymbol;
                preErrorSymbol = null;
            }
            break;
        case 2:
            len = this.productions_[action[1]][1];
            yyval.$ = vstack[vstack.length - len];
            yyval._$ = {
                first_line: lstack[lstack.length - (len || 1)].first_line,
                last_line: lstack[lstack.length - 1].last_line,
                first_column: lstack[lstack.length - (len || 1)].first_column,
                last_column: lstack[lstack.length - 1].last_column
            };
            if (ranges) {
                yyval._$.range = [
                    lstack[lstack.length - (len || 1)].range[0],
                    lstack[lstack.length - 1].range[1]
                ];
            }
            r = this.performAction.apply(yyval, [
                yytext,
                yyleng,
                yylineno,
                sharedState.yy,
                action[1],
                vstack,
                lstack
            ].concat(args));
            if (typeof r !== 'undefined') {
                return r;
            }
            if (len) {
                stack = stack.slice(0, -1 * len * 2);
                vstack = vstack.slice(0, -1 * len);
                lstack = lstack.slice(0, -1 * len);
            }
            stack.push(this.productions_[action[1]][0]);
            vstack.push(yyval.$);
            lstack.push(yyval._$);
            newState = table[stack[stack.length - 2]][stack[stack.length - 1]];
            stack.push(newState);
            break;
        case 3:
            return true;
        }
    }
    return true;
}};

/* generated by jison-lex 0.3.4 */
var lexer = (function(){
var lexer = ({

EOF:1,

parseError:function parseError(str, hash) {
        if (this.yy.parser) {
            this.yy.parser.parseError(str, hash);
        } else {
            throw new Error(str);
        }
    },

// resets the lexer, sets new input
setInput:function (input, yy) {
        this.yy = yy || this.yy || {};
        this._input = input;
        this._more = this._backtrack = this.done = false;
        this.yylineno = this.yyleng = 0;
        this.yytext = this.matched = this.match = '';
        this.conditionStack = ['INITIAL'];
        this.yylloc = {
            first_line: 1,
            first_column: 0,
            last_line: 1,
            last_column: 0
        };
        if (this.options.ranges) {
            this.yylloc.range = [0,0];
        }
        this.offset = 0;
        return this;
    },

// consumes and returns one char from the input
input:function () {
        var ch = this._input[0];
        this.yytext += ch;
        this.yyleng++;
        this.offset++;
        this.match += ch;
        this.matched += ch;
        var lines = ch.match(/(?:\r\n?|\n).*/g);
        if (lines) {
            this.yylineno++;
            this.yylloc.last_line++;
        } else {
            this.yylloc.last_column++;
        }
        if (this.options.ranges) {
            this.yylloc.range[1]++;
        }

        this._input = this._input.slice(1);
        return ch;
    },

// unshifts one char (or a string) into the input
unput:function (ch) {
        var len = ch.length;
        var lines = ch.split(/(?:\r\n?|\n)/g);

        this._input = ch + this._input;
        this.yytext = this.yytext.substr(0, this.yytext.length - len);
        //this.yyleng -= len;
        this.offset -= len;
        var oldLines = this.match.split(/(?:\r\n?|\n)/g);
        this.match = this.match.substr(0, this.match.length - 1);
        this.matched = this.matched.substr(0, this.matched.length - 1);

        if (lines.length - 1) {
            this.yylineno -= lines.length - 1;
        }
        var r = this.yylloc.range;

        this.yylloc = {
            first_line: this.yylloc.first_line,
            last_line: this.yylineno + 1,
            first_column: this.yylloc.first_column,
            last_column: lines ?
                (lines.length === oldLines.length ? this.yylloc.first_column : 0)
                 + oldLines[oldLines.length - lines.length].length - lines[0].length :
              this.yylloc.first_column - len
        };

        if (this.options.ranges) {
            this.yylloc.range = [r[0], r[0] + this.yyleng - len];
        }
        this.yyleng = this.yytext.length;
        return this;
    },

// When called from action, caches matched text and appends it on next action
more:function () {
        this._more = true;
        return this;
    },

// When called from action, signals the lexer that this rule fails to match the input, so the next matching rule (regex) should be tested instead.
reject:function () {
        if (this.options.backtrack_lexer) {
            this._backtrack = true;
        } else {
            return this.parseError('Lexical error on line ' + (this.yylineno + 1) + '. You can only invoke reject() in the lexer when the lexer is of the backtracking persuasion (options.backtrack_lexer = true).\n' + this.showPosition(), {
                text: "",
                token: null,
                line: this.yylineno
            });

        }
        return this;
    },

// retain first n characters of the match
less:function (n) {
        this.unput(this.match.slice(n));
    },

// displays already matched input, i.e. for error messages
pastInput:function () {
        var past = this.matched.substr(0, this.matched.length - this.match.length);
        return (past.length > 20 ? '...':'') + past.substr(-20).replace(/\n/g, "");
    },

// displays upcoming input, i.e. for error messages
upcomingInput:function () {
        var next = this.match;
        if (next.length < 20) {
            next += this._input.substr(0, 20-next.length);
        }
        return (next.substr(0,20) + (next.length > 20 ? '...' : '')).replace(/\n/g, "");
    },

// displays the character position where the lexing error occurred, i.e. for error messages
showPosition:function () {
        var pre = this.pastInput();
        var c = new Array(pre.length + 1).join("-");
        return pre + this.upcomingInput() + "\n" + c + "^";
    },

// test the lexed token: return FALSE when not a match, otherwise return token
test_match:function (match, indexed_rule) {
        var token,
            lines,
            backup;

        if (this.options.backtrack_lexer) {
            // save context
            backup = {
                yylineno: this.yylineno,
                yylloc: {
                    first_line: this.yylloc.first_line,
                    last_line: this.last_line,
                    first_column: this.yylloc.first_column,
                    last_column: this.yylloc.last_column
                },
                yytext: this.yytext,
                match: this.match,
                matches: this.matches,
                matched: this.matched,
                yyleng: this.yyleng,
                offset: this.offset,
                _more: this._more,
                _input: this._input,
                yy: this.yy,
                conditionStack: this.conditionStack.slice(0),
                done: this.done
            };
            if (this.options.ranges) {
                backup.yylloc.range = this.yylloc.range.slice(0);
            }
        }

        lines = match[0].match(/(?:\r\n?|\n).*/g);
        if (lines) {
            this.yylineno += lines.length;
        }
        this.yylloc = {
            first_line: this.yylloc.last_line,
            last_line: this.yylineno + 1,
            first_column: this.yylloc.last_column,
            last_column: lines ?
                         lines[lines.length - 1].length - lines[lines.length - 1].match(/\r?\n?/)[0].length :
                         this.yylloc.last_column + match[0].length
        };
        this.yytext += match[0];
        this.match += match[0];
        this.matches = match;
        this.yyleng = this.yytext.length;
        if (this.options.ranges) {
            this.yylloc.range = [this.offset, this.offset += this.yyleng];
        }
        this._more = false;
        this._backtrack = false;
        this._input = this._input.slice(match[0].length);
        this.matched += match[0];
        token = this.performAction.call(this, this.yy, this, indexed_rule, this.conditionStack[this.conditionStack.length - 1]);
        if (this.done && this._input) {
            this.done = false;
        }
        if (token) {
            return token;
        } else if (this._backtrack) {
            // recover context
            for (var k in backup) {
                this[k] = backup[k];
            }
            return false; // rule action called reject() implying the next rule should be tested instead.
        }
        return false;
    },

// return next match in input
next:function () {
        if (this.done) {
            return this.EOF;
        }
        if (!this._input) {
            this.done = true;
        }

        var token,
            match,
            tempMatch,
            index;
        if (!this._more) {
            this.yytext = '';
            this.match = '';
        }
        var rules = this._currentRules();
        for (var i = 0; i < rules.length; i++) {
            tempMatch = this._input.match(this.rules[rules[i]]);
            if (tempMatch && (!match || tempMatch[0].length > match[0].length)) {
                match = tempMatch;
                index = i;
                if (this.options.backtrack_lexer) {
                    token = this.test_match(tempMatch, rules[i]);
                    if (token !== false) {
                        return token;
                    } else if (this._backtrack) {
                        match = false;
                        continue; // rule action called reject() implying a rule MISmatch.
                    } else {
                        // else: this is a lexer rule which consumes input without producing a token (e.g. whitespace)
                        return false;
                    }
                } else if (!this.options.flex) {
                    break;
                }
            }
        }
        if (match) {
            token = this.test_match(match, rules[index]);
            if (token !== false) {
                return token;
            }
            // else: this is a lexer rule which consumes input without producing a token (e.g. whitespace)
            return false;
        }
        if (this._input === "") {
            return this.EOF;
        } else {
            return this.parseError('Lexical error on line ' + (this.yylineno + 1) + '. Unrecognized text.\n' + this.showPosition(), {
                text: "",
                token: null,
                line: this.yylineno
            });
        }
    },

// return next match that has a token
lex:function lex() {
        var r = this.next();
        if (r) {
            return r;
        } else {
            return this.lex();
        }
    },

// activates a new lexer condition state (pushes the new lexer condition state onto the condition stack)
begin:function begin(condition) {
        this.conditionStack.push(condition);
    },

// pop the previously active lexer condition state off the condition stack
popState:function popState() {
        var n = this.conditionStack.length - 1;
        if (n > 0) {
            return this.conditionStack.pop();
        } else {
            return this.conditionStack[0];
        }
    },

// produce the lexer rule set which is active for the currently active lexer condition state
_currentRules:function _currentRules() {
        if (this.conditionStack.length && this.conditionStack[this.conditionStack.length - 1]) {
            return this.conditions[this.conditionStack[this.conditionStack.length - 1]].rules;
        } else {
            return this.conditions["INITIAL"].rules;
        }
    },

// return the currently active lexer condition state; when an index argument is provided it produces the N-th previous condition state, if available
topState:function topState(n) {
        n = this.conditionStack.length - 1 - Math.abs(n || 0);
        if (n >= 0) {
            return this.conditionStack[n];
        } else {
            return "INITIAL";
        }
    },

// alias for begin(condition)
pushState:function pushState(condition) {
        this.begin(condition);
    },

// return the number of states currently on the stack
stateStackSize:function stateStackSize() {
        return this.conditionStack.length;
    },
options: {},
performAction: function anonymous(yy,yy_,$avoiding_name_collisions,YY_START) {
var YYSTATE=YY_START;
switch($avoiding_name_collisions) {
case 0: /* skip whitespace */ 
break;
case 1: return 17; 
break;
case 2: return 18; 
break;
case 3: return 20; 
break;
case 4: return 35; 
break;
case 5: return 36; 
break;
case 6: return 37; 
break;
case 7: return 33; 
break;
case 8: return 'FROM'; 
break;
case 9: return 23; 
break;
case 10: return 34; 
break;
case 11: return 21; 
break;
case 12: return 40; 
break;
case 13: return 50; 
break;
case 14: return 53; 
break;
case 15: return 55; 
break;
case 16: return 39; 
break;
case 17: return 38; 
break;
case 18: return 32; 
break;
case 19: return 41; 
break;
case 20: return 42; 
break;
case 21: return 46; 
break;
case 22: return 48; 
break;
case 23: return 49; 
break;
case 24: return 28; 
break;
case 25: return 29; 
break;
case 26: return 26; 
break;
case 27: return 27; 
break;
case 28: return 30; 
break;
case 29: return 43; 
break;
case 30: return 44; 
break;
case 31: return 45; 
break;
case 32: return 5; 
break;
case 33: return 15; 
break;
case 34: return 16; 
break;
case 35: return 'INVALID'; 
break;
}
},
rules: [/^(?:\s+)/,/^(?:fn\b)/,/^(?:\()/,/^(?:\))/,/^(?:\+)/,/^(?:\*)/,/^(?:-)/,/^(?:add1\b)/,/^(?:from\b)/,/^(?:,)/,/^(?:~)/,/^(?:=>)/,/^(?:===)/,/^(?:=)/,/^(?:\[)/,/^(?:\])/,/^(?:\/)/,/^(?:%)/,/^(?:::)/,/^(?:>)/,/^(?:<)/,/^(?:let\b)/,/^(?:in\b)/,/^(?:end\b)/,/^(?:hd\b)/,/^(?:tl\b)/,/^(?:sumlist\b)/,/^(?:not\b)/,/^(?:isNull\b)/,/^(?:if\b)/,/^(?:then\b)/,/^(?:else\b)/,/^(?:$)/,/^(?:([a-zA-Z])(([a-zA-Z])|([0-9])|_)*)/,/^(?:([0-9])+)/,/^(?:.)/],
conditions: {"INITIAL":{"rules":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35],"inclusive":true}}
});
return lexer;
})();
parser.lexer = lexer;
function Parser () {
  this.yy = {};
}
Parser.prototype = parser;parser.Parser = Parser;
return new Parser;
})();


if (typeof require !== 'undefined' && typeof exports !== 'undefined') {
exports.parser = parser;
exports.Parser = parser.Parser;
exports.parse = function () { return parser.parse.apply(parser, arguments); };
exports.main = function commonjsMain(args) {
    if (!args[1]) {
        console.log('Usage: '+args[0]+' FILE');
        process.exit(1);
    }
    var source = require('fs').readFileSync(require('path').normalize(args[1]), "utf8");
    return exports.parser.parse(source);
};
if (typeof module !== 'undefined' && require.main === module) {
  exports.main(process.argv.slice(1));
}
}