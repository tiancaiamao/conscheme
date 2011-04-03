// Copyright (C) 2011 Göran Weinholt <goran@weinholt.se>
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

// All Scheme objects have type Obj. The two low bits in pointers are
// used to get limited type tagging. Normally one would want the tag
// '00' for fixnums, because it simplies arithmetic, but we have to
// use '00' for heap-allocated objects, or else the GC will deallocate
// all our objects.

// bits         purpose
// x....x00     heap-allocated object
// x....x01     fixnum
//       10     (unused)
//     x011     boolean
// x..x0111     character
//   001111     empty list
//   011111     end of file object
//   101111     void

// Heap-allocated objects have these dynamic interface{} types:
// []Obj        vector
// []int        string
// *[2]Obj      pair
// *big.Int     bignum
// string       symbol

package conscheme

import (
	"big"
	"fmt"
	"io"
	"os"
	"sync"
	"unsafe"
)

// Tags
const heap_tag = 0
const heap_mask = 0x3

const fixnum_tag = 1
const fixnum_mask = 0x3
const fixnum_shift = 2

const bool_shift = 3
const bool_tag = 0x3
const bool_mask = 0x7

const char_tag = 7
const char_mask = 0xf
const char_shift = 4

// The type of all Scheme objects.
type Obj *interface{}

// Constants.
const (
	False = Obj(unsafe.Pointer(uintptr((0 << bool_shift) | bool_tag)))
	True = Obj(unsafe.Pointer(uintptr((1 << bool_shift) | bool_tag)))
	Eol = Obj(unsafe.Pointer(uintptr(0x0f))) // empty list
	Eof = Obj(unsafe.Pointer(uintptr(0x1f))) // end of file object
	Void = Obj(unsafe.Pointer(uintptr(0x2f)))	// the unspecified value
)

const fixnum_max = int(^uint(0) >> (1 + fixnum_shift))
const fixnum_min = -fixnum_max - 1

// Fixnums

func fixnum_p(x Obj) Obj {
	return Make_boolean((uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag)
}

func Make_fixnum(x int) Obj {
	// XXX: assumes x fits in a fixnum
	return Obj(unsafe.Pointer(uintptr((x << fixnum_shift) | fixnum_tag)))
}

func fixnum_to_int(x Obj) int {
	return int(uintptr(unsafe.Pointer(x))) >> fixnum_shift
}

func fixnum_add(fx1,fx2 Obj) Obj {
	i1 := uintptr(unsafe.Pointer(fx1))
	i2 := uintptr(unsafe.Pointer(fx2))
	if (i1 & fixnum_mask) != fixnum_tag || (i2 & fixnum_mask) != fixnum_tag {
		panic("bad type")
	}
	r := i1 + i2
	// TODO: how should we do this?
	// if r < fixnum_min || r > fixnum_max {
	// 	panic("result not representable")
	// }

	return Obj(unsafe.Pointer(uintptr(r - fixnum_tag)))
}

// Chars

func char_p(x Obj) Obj {
	return Make_boolean((uintptr(unsafe.Pointer(x)) & char_mask) == char_tag)
}

func Make_char(x int) Obj {
	return Obj(unsafe.Pointer(uintptr((x << char_shift) | char_tag)))
}

func char_to_int(x Obj) int {
	return int(uintptr(unsafe.Pointer(x))) >> char_shift
}

// Booleans

func boolean_p(x Obj) Obj {
	return Make_boolean((uintptr(unsafe.Pointer(x)) & bool_mask) == bool_tag)
}

func Make_boolean(x bool) Obj {
	if x { return True }
	return False
}

func not(x Obj) Obj {
	if x == False {	return True }
	return False
}

// Pairs

func pair_p(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { return False }

	switch v := (*x).(type) {
	case *[2]Obj:
		return True
	}
	return False
}

func Cons(x,y Obj) Obj {
	var v [2]Obj
	v[0] = x
	v[1] = y

	var vi interface{} = &v
	return Obj(&vi)
}

func car(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { panic("bad type") }
	v := (*x).(*[2]Obj)
	return v[0]
}

func cdr(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { panic("bad type") }
	v := (*x).(*[2]Obj)
	return v[1]
}

func set_car_ex(x,value Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { panic("bad type") }
	v := (*x).(*[2]Obj)
	v[0] = value
	return Void
}

func set_cdr_ex(x,value Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { panic("bad type") }
	v := (*x).(*[2]Obj)
	v[1] = value
	return Void
}

func list(v ...Obj) Obj {
	ret := Eol
	for i := len(v) - 1; i >= 0; i-- {
		ret = Cons(v[i], ret)
	}
	return ret
}

func Length(x Obj) Obj {
	var l int
	for l = 0; x != Eol; l++ {
		if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag {
			panic("not a list")
		}
		v := (*x).(*[2]Obj)
		x = v[1]
	}
	return make_number(l)
}


// Vectors

func vector_p(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { return False }

	switch v := (*x).(type) {
	case []Obj:
		return True
	}
	return False
}

func vector(v ...Obj) Obj {
	var vi interface{} = v
	return Obj(&vi)
}

func Make_vector(length,init Obj) Obj {
	if fixnum_p(length) == False {
		panic("bad type")
	}
	l := fixnum_to_int(length)
	v := make([]Obj, fixnum_to_int(length))

	for i := 0; i < l; i++ {
		v[i] = init
	}

	var vi interface{} = v

	return Obj(&vi)
}

func vector_length(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { panic("bad type") }
	v := (*x).([]Obj)
	return make_number(len(v))
}


func Vector_ref(x,idx Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag ||
		(uintptr(unsafe.Pointer(idx)) & fixnum_mask) != fixnum_tag {
		panic("bad type")
	}
	v := (*x).([]Obj)
	return v[fixnum_to_int(idx)]
}

func Vector_set_ex(x,idx,value Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag ||
		(uintptr(unsafe.Pointer(idx)) & fixnum_mask) != fixnum_tag {
		panic("bad type")
	}
	v := (*x).([]Obj)
	v[fixnum_to_int(idx)] = value
	return Void
}

// Strings

func string_p(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { return False }

	switch v := (*x).(type) {
	case []int:
		return True
	}
	return False
}

func String_string(s string) Obj {
	rune := ([]int)(s)
	var vv interface{} = rune
	return Obj(&vv)
}

func Make_string(length,init Obj) Obj {
	if fixnum_p(length) == False || char_p(init) == False {
		panic("bad type")
	}
	l := fixnum_to_int(length)
	v := make([]int, fixnum_to_int(length))
	init_c := char_to_int(init)

	for i := 0; i < l; i++ {
		v[i] = init_c
	}

	var vi interface{} = v

	return Obj(&vi)
}

func String_length(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { panic("bad type") }
	v := (*x).([]int)
	return make_number(len(v))
}

func String_ref(x, idx Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag ||
		(uintptr(unsafe.Pointer(idx)) & fixnum_mask) != fixnum_tag {
		panic("bad type")
	}
	v := (*x).([]int)
	return Make_char(v[fixnum_to_int(idx)])
}


// Bignums

func make_bignum(x int64) Obj {
	var vv interface{} = big.NewInt(x)
	return Obj(&vv)
}

func bignum_p(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { return False }

	switch v := (*x).(type) {
	case *big.Int:
		return True
	}
	return False
}

// Numbers

func make_number(x int) Obj {
	v := Make_fixnum(x)
	if fixnum_to_int(v) != x {
		return make_bignum(int64(x))
	}
	return v
}

func number_to_int(x Obj) int {
	if (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag {
		return fixnum_to_int(x)
	}

	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag {
		panic("bad type")
	}

	switch v := (*x).(type) {
	case *big.Int:
		// XXX:
		return int(v.Int64())
	}
	panic("bad type")
}

func number_p(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag {
		return True
	}
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag {
		return False
	}
	switch v := (*x).(type) {
	case *big.Int:
		return True
	}
	return False
}

func number_equal(x,y Obj) Obj {
	xfx := (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag
	yfx := (uintptr(unsafe.Pointer(y)) & fixnum_mask) == fixnum_tag
	if xfx && yfx {	return Make_boolean(x == y) }

	if (!xfx && (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag) ||
		(!yfx && (uintptr(unsafe.Pointer(y)) & heap_mask) != heap_tag) {
		panic("bad type")
	}

	if xfx { return number_equal(y,x) }

	switch vx := (*x).(type) {
	case *big.Int:
		if yfx {
			vy := big.NewInt(int64(fixnum_to_int(y)))
			return Make_boolean(vx.Cmp(vy) == 0)
		}
		switch vy := (*y).(type) {
		case *big.Int:
			return Make_boolean(vx.Cmp(vy) == 0)
		default:
			panic("bad type")
		}
	}
	panic("bad type")
}

// Symbols

// It would be better to use a weak hashset here, if one was available
var symtab map[string]Obj = make(map[string]Obj)
var symlock sync.Mutex

func intern(x string) Obj {
	return String_to_symbol(String_string(x))
}

func symbol_p(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag { return False }

	switch v := (*x).(type) {
	case string:
		return True
	}
	return False
}

func String_to_symbol(x Obj) Obj {
	if string_p(x) == False {
		panic("bad type")
		return Void
	}
	v := (*x).([]int)
	str := string(v)
	symlock.Lock();	defer symlock.Unlock()
	sym, is_interned := symtab[str]
	if is_interned {
		// string->symbol has already interned this symbol
		return sym
	}
	// Intern the new symbol
	var stri interface{} = str
	sym = Obj(&stri)
	symtab[str] = sym
	return sym
}

func Symbol_to_string(x Obj) Obj {
	if symbol_p(x) == False {
		panic("bad type")
		return Void
	}
	v := (*x).(string)
	str := ([]int)(v)
	var stri interface{} = str
	return Obj(&stri)
}

// Object printer (for debugging)

func Obj_display(x Obj, p io.Writer, write Obj) {
	switch {
	case fixnum_p(x) != False:
		fmt.Fprintf(p, "%d", fixnum_to_int(x))
	case bignum_p(x) != False:
		fmt.Fprintf(p, "%d", *x)
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
		for i := 0; i < length - 1; i++ {
			Obj_display(Vector_ref(x,Make_fixnum(i)), p, write)
			fmt.Fprintf(p, " ")
		}
		if length > 0 {
			Obj_display(Vector_ref(x,Make_fixnum(length-1)), p, write)
		}
		fmt.Fprintf(p, ")")
	case string_p(x) != False:
		// XXX: doesn't handle \n, etc
		if write != False { fmt.Fprintf(p, "\"") }
		length := fixnum_to_int(String_length(x))
		s := (*x).([]int)
		for i := 0; i < length; i++ {
			fmt.Fprintf(p, "%c", s[i])
		}
		if write != False { fmt.Fprintf(p, "\"") }
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
	// Unknown types
	case uintptr(unsafe.Pointer(x)) & heap_mask == heap_tag:
		fmt.Fprintf(p, "#<heapobj %d>", *x)
	default:
		fmt.Fprintf(p, "#<immobj %x>", x)
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
