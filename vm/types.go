// Copyright (C) 2011, 2017 Göran Weinholt <goran@weinholt.se>
// Copyright (C) 2011 Per Odlund <per.odlund@gmail.com>

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

// Basic definitions of Scheme object types and a few primitives

// All Scheme objects have type Obj.

// Objects have these dynamic interface{} types:
// []Obj        vector
// []rune       string
// *[2]Obj      pair
// *big.Int     bignum
// *big.Rat     ratnum
// ScmSym       symbol
// bool         boolean
// int          fixnum
// ScmChar      char

package vm

import (
	"fmt"
	"io"
	"os"
	"sync"
)

// The type of all Scheme objects.
type Obj interface{}

type ScmChar struct {
	codepoint rune
}

type ScmSym struct {
	str string
}

// Constants.
var (
	False = Obj(false)
	True  = Obj(true)
	Eol   = Obj([0]Obj{}) // empty list
	Eof   = Obj(io.EOF)   // end of file object
	Void  = Obj(nil)      // the unspecified value
)

// Chars

func char_p(x Obj) Obj {
	switch (x).(type) {
	default:
		return False
	case ScmChar:
		return True
	}
}

func Make_char(x rune) Obj {
	return ScmChar{x}
}

func char_to_int(x Obj) rune {
	return (x).(ScmChar).codepoint
}

func char_to_integer(x Obj) Obj {
	return int((x).(ScmChar).codepoint)
}

func integer_to_char(c Obj) Obj {
	fx := number_to_int(c)
	if (fx >= 0 && fx <= 0xd7ff) || (fx >= 0xe000 && fx <= 0x10ffff) {
		return Make_char(rune(fx))
	}
	panic("codepoint is outside the unicode range")
}

// Booleans

func boolean_p(x Obj) Obj {
	switch (x).(type) {
	case bool:
		return True
	}
	return False
}

func Make_boolean(x bool) Obj {
	if x {
		return True
	}
	return False
}

func not(x Obj) Obj {
	if x == False {
		return True
	}
	return False
}

// Pairs

func pair_p(x Obj) Obj {
	switch (x).(type) {
	case *[2]Obj:
		return True
	}
	return False
}

func Cons(x, y Obj) Obj {
	var v [2]Obj
	v[0] = x
	v[1] = y

	var vi interface{} = &v
	return Obj(vi)
}

func car(x Obj) Obj {
	v := (x).(*[2]Obj)
	return v[0]
}

func cdr(x Obj) Obj {
	v := (x).(*[2]Obj)
	return v[1]
}

func set_car_ex(x, value Obj) Obj {
	v := (x).(*[2]Obj)
	v[0] = value
	return Void
}

func set_cdr_ex(x, value Obj) Obj {
	v := (x).(*[2]Obj)
	v[1] = value
	return Void
}

// func list(v ...Obj) Obj {
// 	ret := Eol
// 	for i := len(v) - 1; i >= 0; i-- {
// 		ret = Cons(v[i], ret)
// 	}
// 	return ret
// }

func Length(x Obj) Obj {
	var i int
	for i = 0; x != Eol; i++ {
		v := (x).(*[2]Obj)
		x = v[1]
	}
	return make_number(i)
}

func Floyd(x Obj) Obj {
	t := x
	h := x
	for {
		if t == Eol || h == Eol {
			return True
		}
		if pair_p(t) == False || pair_p(h) == False {
			return False
		}
		h = cdr(h)
		if h == Eol {
			return True
		}

		if pair_p(h) == False {
			return False
		}
		t = cdr(t)
		h = cdr(h)
		if h == t {
			return False
		}
	}
}

// Vectors

func vector_p(x Obj) Obj {
	switch (x).(type) {
	case []Obj:
		return True
	}
	return False
}

func _vector(v ...Obj) Obj {
	var vi interface{} = v
	return Obj(vi)
}

func Make_vector(length, init Obj) Obj {
	if fixnum_p(length) == False {
		panic("bad type")
	}
	l := fixnum_to_int(length)
	v := make([]Obj, fixnum_to_int(length))

	for i := 0; i < l; i++ {
		v[i] = init
	}

	return v
}

func vector_length(x Obj) Obj {
	v := (x).([]Obj)
	return make_number(len(v))
}

func Vector_ref(x, idx Obj) Obj {
	v := (x).([]Obj)
	return v[fixnum_to_int(idx)]
}

func Vector_set_ex(x, idx, value Obj) Obj {
	v := (x).([]Obj)
	v[fixnum_to_int(idx)] = value
	return Void
}

// Strings

func string_p(x Obj) Obj {
	switch (x).(type) {
	case []rune:
		return True
	}
	return False
}

func String_string(s string) Obj {
	return ([]rune)(s)
}

func Make_string(length, init Obj) Obj {
	if fixnum_p(length) == False || char_p(init) == False {
		panic("bad type")
	}
	l := fixnum_to_int(length)
	v := make([]rune, fixnum_to_int(length))
	init_c := char_to_int(init)

	for i := 0; i < l; i++ {
		v[i] = init_c
	}

	return v
}

func String_length(x Obj) Obj {
	v := (x).([]rune)
	return make_number(len(v))
}

func String_ref(x, idx Obj) Obj {
	v := (x).([]rune)
	return Make_char(v[fixnum_to_int(idx)])
}

func String_set_ex(x, idx, ch Obj) Obj {
	v := (x).([]rune)
	v[fixnum_to_int(idx)] = char_to_int(ch)
	return Void
}

// Symbols

// It would be better to use a weak hashset here, if one was available
var symtab map[string]Obj = make(map[string]Obj)
var symlock sync.RWMutex

func intern(x string) Obj {
	return String_to_symbol(String_string(x))
}

func symbol_p(x Obj) Obj {
	switch (x).(type) {
	case *ScmSym:
		return True
	}
	return False
}

func getsym(str string) (Obj, bool) {
	symlock.RLock()
	defer symlock.RUnlock()
	sym, is_interned := symtab[str]
	return sym, is_interned
}

func String_to_symbol(x Obj) Obj {
	str := string(x.([]rune))
	sym, is_interned := getsym(str)
	if is_interned {
		return sym
	}

	// Intern the new symbol
	symlock.Lock()
	defer symlock.Unlock()
	newsym := &ScmSym{string(x.([]rune))}
	symtab[str] = newsym

	return newsym
}

func scm2str(x Obj) string {
	return x.(*ScmSym).str
}

func Symbol_to_string(x Obj) Obj {
	if symbol_p(x) == False {
		panic("bad type")
	}

	return ([]rune)(x.(*ScmSym).str)
}

// Object printer (for debugging)

func Obj_display(x Obj, p io.Writer, write Obj) {
	switch {
	case number_p(x) != False:
		str := string((_number_to_string(x, Make_fixnum(10))).([]rune))
		fmt.Fprintf(p, "%v", str)
	case char_p(x) != False:
		if write != False {
			fmt.Fprintf(p, "#\\")
			// XXX: doesn't handle #\newline, etc
		}
		fmt.Fprintf(p, "%c", char_to_int(x))
	case boolean_p(x) != False:
		if x == False {
			fmt.Fprintf(p, "#f")
		} else {
			fmt.Fprintf(p, "#t")
		}
	case vector_p(x) != False:
		fmt.Fprintf(p, "#(")
		length := fixnum_to_int(vector_length(x))
		for i := 0; i < length-1; i++ {
			Obj_display(Vector_ref(x, Make_fixnum(i)), p, write)
			fmt.Fprintf(p, " ")
		}
		if length > 0 {
			Obj_display(Vector_ref(x, Make_fixnum(length-1)), p, write)
		}
		fmt.Fprintf(p, ")")
	case string_p(x) != False:
		// XXX: doesn't handle \n, etc
		if write != False {
			fmt.Fprintf(p, "\"")
		}
		length := fixnum_to_int(String_length(x))
		s := (x).([]rune)
		for i := 0; i < length; i++ {
			fmt.Fprintf(p, "%c", s[i])
		}
		if write != False {
			fmt.Fprintf(p, "\"")
		}
	case symbol_p(x) != False:
		// XXX: doesn't handle escapes
		Obj_display(Symbol_to_string(x), p, False)
	case pair_p(x) != False:
		fmt.Fprintf(p, "(")
		for i := x; i != Eol; {
			Obj_display(car(i), p, write)
			i = cdr(i)
			switch {
			case i == Eol:
			case pair_p(i) != False:
				fmt.Fprintf(p, " ")
			default:
				fmt.Fprintf(p, " . ")
				Obj_display(i, p, write)
				i = Eol
			}
		}
		fmt.Fprintf(p, ")")
	case x == Eol:
		fmt.Fprintf(p, "()")
	case x == Eof:
		fmt.Fprintf(p, "#<eof>")
	case x == Void:
		fmt.Fprintf(p, "#<void>")
	case procedure_p(x) != False:
		proc := (x).(*Procedure)
		fmt.Fprintf(p, "#<procedure %s>", proc.name)
	case port_p(x) != False:
		display_port(p, x)
	case bytevector_p(x) != False:
		fmt.Fprintf(p, "#vu8(")
		bv := (x).([]byte)
		for i := 0; i < len(bv); i++ {
			if i > 0 {
				fmt.Fprintf(p, " ")
			}
			fmt.Fprintf(p, "%d", bv[i])
		}
		fmt.Fprintf(p, ")")
	case thread_p(x) != False:
		fmt.Fprintf(p, "#<thread ")
		t := (x).(*Thread)
		Obj_display(t.name, p, write)
		fmt.Fprintf(p, ">")
	// Unknown types
	default:
		fmt.Fprintf(p, "#<obj %x>", x)
	}
}

func Display(x Obj) Obj {
	Obj_display(x, os.Stdout, False)
	return Void
}

func Write(x Obj) Obj {
	Obj_display(x, os.Stdout, True)
	return Void
}
