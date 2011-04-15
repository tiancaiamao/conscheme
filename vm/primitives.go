// This file is part of conscheme
// Automatically generated by compiler/primitives.scm
package conscheme
import "fmt"
import "os"
var primitives map[string]Obj = make(map[string]Obj)
func init() {
	primitives["$write"] = wrap(Procedure{name:"$write",required:2,apply:apprim})
	primitives["$display"] = wrap(Procedure{name:"$display",required:2,apply:apprim})
	primitives["lookahead-u8"] = wrap(Procedure{name:"lookahead-u8",required:1,apply:apprim})
	primitives["put-u8"] = wrap(Procedure{name:"put-u8",required:2,apply:apprim})
	primitives["get-u8"] = wrap(Procedure{name:"get-u8",required:1,apply:apprim})
	primitives["$write-char"] = wrap(Procedure{name:"$write-char",required:2,apply:apprim})
	primitives["$peek-char"] = wrap(Procedure{name:"$peek-char",required:1,apply:apprim})
	primitives["$read-char"] = wrap(Procedure{name:"$read-char",required:1,apply:apprim})
	primitives["close-input-port"] = wrap(Procedure{name:"close-input-port",required:1,apply:apprim})
	primitives["open-input-file"] = wrap(Procedure{name:"open-input-file",required:1,apply:apprim})
	primitives["current-output-port"] = wrap(Procedure{name:"current-output-port",required:0,apply:apprim})
	primitives["current-input-port"] = wrap(Procedure{name:"current-input-port",required:0,apply:apprim})
	primitives["output-port?"] = wrap(Procedure{name:"output-port?",required:1,apply:apprim})
	primitives["input-port?"] = wrap(Procedure{name:"input-port?",required:1,apply:apprim})
	primitives["port?"] = wrap(Procedure{name:"port?",required:1,apply:apprim})
	primitives["bytevector?"] = wrap(Procedure{name:"bytevector?",required:1,apply:apprim})
	primitives["$cell-set!"] = wrap(Procedure{name:"$cell-set!",required:2,apply:apprim})
	primitives["$cell-ref"] = wrap(Procedure{name:"$cell-ref",required:1,apply:apprim})
	primitives["$make-cell"] = wrap(Procedure{name:"$make-cell",required:1,apply:apprim})
	primitives["$eval"] = wrap(Procedure{name:"$eval",required:1,apply:apprim})
	primitives["command-line"] = wrap(Procedure{name:"command-line",required:0,apply:apprim})
	primitives["exit"] = wrap(Procedure{name:"exit",required:1,apply:apprim})
	primitives["eq?"] = wrap(Procedure{name:"eq?",required:2,apply:apprim})
	primitives["eof-object"] = wrap(Procedure{name:"eof-object",required:0,apply:apprim})
	primitives["unspecified"] = wrap(Procedure{name:"unspecified",required:0,apply:apprim})
	primitives["procedure?"] = wrap(Procedure{name:"procedure?",required:1,apply:apprim})
	primitives["apply"] = wrap(Procedure{name:"apply",required:1,apply:apprim})
	primitives["make-string"] = wrap(Procedure{name:"make-string",required:1,apply:apprim})
	primitives["string-set!"] = wrap(Procedure{name:"string-set!",required:3,apply:apprim})
	primitives["string-ref"] = wrap(Procedure{name:"string-ref",required:2,apply:apprim})
	primitives["string-length"] = wrap(Procedure{name:"string-length",required:1,apply:apprim})
	primitives["string?"] = wrap(Procedure{name:"string?",required:1,apply:apprim})
	primitives["greatest-fixnum"] = wrap(Procedure{name:"greatest-fixnum",required:0,apply:apprim})
	primitives["least-fixnum"] = wrap(Procedure{name:"least-fixnum",required:0,apply:apprim})
	primitives["$cmp"] = wrap(Procedure{name:"$cmp",required:2,apply:apprim})
	primitives["$-"] = wrap(Procedure{name:"$-",required:2,apply:apprim})
	primitives["$/"] = wrap(Procedure{name:"$/",required:2,apply:apprim})
	primitives["$+"] = wrap(Procedure{name:"$+",required:2,apply:apprim})
	primitives["$number->string"] = wrap(Procedure{name:"$number->string",required:2,apply:apprim})
	primitives["number?"] = wrap(Procedure{name:"number?",required:1,apply:apprim})
	primitives["vector-set!"] = wrap(Procedure{name:"vector-set!",required:3,apply:apprim})
	primitives["vector-ref"] = wrap(Procedure{name:"vector-ref",required:2,apply:apprim})
	primitives["vector-length"] = wrap(Procedure{name:"vector-length",required:1,apply:apprim})
	primitives["make-vector"] = wrap(Procedure{name:"make-vector",required:2,apply:apprim})
	primitives["vector?"] = wrap(Procedure{name:"vector?",required:1,apply:apprim})
	primitives["char-downcase"] = wrap(Procedure{name:"char-downcase",required:1,apply:apprim})
	primitives["char-upcase"] = wrap(Procedure{name:"char-upcase",required:1,apply:apprim})
	primitives["char-whitespace?"] = wrap(Procedure{name:"char-whitespace?",required:1,apply:apprim})
	primitives["integer->char"] = wrap(Procedure{name:"integer->char",required:1,apply:apprim})
	primitives["char->integer"] = wrap(Procedure{name:"char->integer",required:1,apply:apprim})
	primitives["char?"] = wrap(Procedure{name:"char?",required:1,apply:apprim})
	primitives["string->symbol"] = wrap(Procedure{name:"string->symbol",required:1,apply:apprim})
	primitives["symbol->string"] = wrap(Procedure{name:"symbol->string",required:1,apply:apprim})
	primitives["symbol?"] = wrap(Procedure{name:"symbol?",required:1,apply:apprim})
	primitives["set-cdr!"] = wrap(Procedure{name:"set-cdr!",required:2,apply:apprim})
	primitives["set-car!"] = wrap(Procedure{name:"set-car!",required:2,apply:apprim})
	primitives["length"] = wrap(Procedure{name:"length",required:1,apply:apprim})
	primitives["floyd"] = wrap(Procedure{name:"floyd",required:1,apply:apprim})
	primitives["cdr"] = wrap(Procedure{name:"cdr",required:1,apply:apprim})
	primitives["car"] = wrap(Procedure{name:"car",required:1,apply:apprim})
	primitives["cons"] = wrap(Procedure{name:"cons",required:2,apply:apprim})
	primitives["pair?"] = wrap(Procedure{name:"pair?",required:1,apply:apprim})
	primitives["not"] = wrap(Procedure{name:"not",required:1,apply:apprim})
	primitives["boolean?"] = wrap(Procedure{name:"boolean?",required:1,apply:apprim})
}

func evprim(primop string, args []Obj) Obj {
	switch primop {
	case "$write":
		return write(args[0], args[1])
	case "$display":
		return display(args[0], args[1])
	case "lookahead-u8":
		return lookahead_u8(args[0])
	case "put-u8":
		return put_u8(args[0], args[1])
	case "get-u8":
		return get_u8(args[0])
	case "$write-char":
		return _write_char(args[0], args[1])
	case "$peek-char":
		return _peek_char(args[0])
	case "$read-char":
		return _read_char(args[0])
	case "close-input-port":
		return close_input_port(args[0])
	case "open-input-file":
		return open_input_file(args[0])
	case "current-output-port":
		return current_output_port()
	case "current-input-port":
		return current_input_port()
	case "output-port?":
		return output_port_p(args[0])
	case "input-port?":
		return input_port_p(args[0])
	case "port?":
		return port_p(args[0])
	case "bytevector?":
		return bytevector_p(args[0])
	case "$cell-set!":
		v := args[0]
		vv := (*v).(*[1]Obj)
		vv[0] = args[1]
		return Void
	case "$cell-ref":
		v := args[0]
		vv := (*v).(*[1]Obj)
		return vv[0]
	case "$make-cell":
		var v [1]Obj
		v[0] = args[0]
		var vv interface{} = &v
		return Obj(&vv)
	case "$eval":
		return Eval(args[0])
	case "command-line":
		return Command_line()
	case "exit":
		os.Exit(number_to_int(args[0]))
	case "eq?":
		if args[0] == args[1] {
			return True
		} else {
			return False
		}
	case "eof-object":
		return Eof
	case "unspecified":
		return Void
	case "procedure?":
		return procedure_p(args[0])
	case "apply":
		return apply(args)
	case "make-string":
		switch len(args) {
		default: return Make_string(args[0],args[1])
		case 1: return Make_string(args[0],Make_char(32))
		}
	case "string-set!":
		return String_set_ex(args[0], args[1], args[2])
	case "string-ref":
		return String_ref(args[0], args[1])
	case "string-length":
		return String_length(args[0])
	case "string?":
		return string_p(args[0])
	case "greatest-fixnum":
		return Make_fixnum(fixnum_max)
	case "least-fixnum":
		return Make_fixnum(fixnum_min)
	case "$cmp":
		return number_cmp(args[0], args[1])
	case "$-":
		return number_subtract(args[0], args[1])
	case "$/":
		return number_divide(args[0], args[1])
	case "$+":
		return number_add(args[0], args[1])
	case "$number->string":
		return _number_to_string(args[0], args[1])
	case "number?":
		return number_p(args[0])
	case "vector-set!":
		return Vector_set_ex(args[0], args[1], args[2])
	case "vector-ref":
		return Vector_ref(args[0], args[1])
	case "vector-length":
		return vector_length(args[0])
	case "make-vector":
		return Make_vector(args[0], args[1])
	case "vector?":
		return vector_p(args[0])
	case "char-downcase":
		return char_downcase(args[0])
	case "char-upcase":
		return char_upcase(args[0])
	case "char-whitespace?":
		return char_whitespace_p(args[0])
	case "integer->char":
		return integer_to_char(args[0])
	case "char->integer":
		return char_to_integer(args[0])
	case "char?":
		return char_p(args[0])
	case "string->symbol":
		return String_to_symbol(args[0])
	case "symbol->string":
		return Symbol_to_string(args[0])
	case "symbol?":
		return symbol_p(args[0])
	case "set-cdr!":
		return set_cdr_ex(args[0], args[1])
	case "set-car!":
		return set_car_ex(args[0], args[1])
	case "length":
		return Length(args[0])
	case "floyd":
		return Floyd(args[0])
	case "cdr":
		return cdr(args[0])
	case "car":
		return car(args[0])
	case "cons":
		return Cons(args[0], args[1])
	case "pair?":
		return pair_p(args[0])
	case "not":
		return not(args[0])
	case "boolean?":
		return boolean_p(args[0])
	default:
		fmt.Fprintf(os.Stderr, "Please regenerate primitives.go\n")
		panic(fmt.Sprintf("Unimplemented primitive: %s",primop))
	}
	panic(fmt.Sprintf("Fell off the edge in evprim(): %s",primop))
}
