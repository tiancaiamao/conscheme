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

// Arithmetic for conscheme

// TODO: the arithmetic procedures should return fixnums when fixnums
// can represent the result, e.g. when two bignums are added.

package conscheme

import (
	"big"
	"fmt"
	"math"
	"strings"
	"unsafe"
)

const (
	fixnum_max = int(^uint(0) >> (1 + fixnum_shift))
	fixnum_min = -fixnum_max - 1
)

var fixnum_max_Int, fixnum_min_Int *big.Int

func init() {
	fixnum_max_Int = big.NewInt(int64(fixnum_max))
	fixnum_min_Int = big.NewInt(int64(fixnum_min))
}

// Exact complex numbers. Inexact complex numbers use complex128.
type Compnum struct {
	real, imag big.Rat
}

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

func make_number(x int) Obj {
	v := Make_fixnum(x)
	if fixnum_to_int(v) != x {
		return wrap(big.NewInt(int64(x)))
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
	default: return False
	case *big.Int:
	case *big.Rat:
	case float64:
	case complex128:
	// case *Compnum:
	}
	return True
}

func integer_p(x Obj) Obj {
	if (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag {
		return True
	}
	if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag {
		return False
	}
	switch v := (*x).(type) {
	default: return False
	case *big.Int:
		return True
	case float64:
		return Make_boolean(v == math.Floor(v))
	}
	return False
}

func denominator(num Obj) Obj {
	if (uintptr(unsafe.Pointer(num)) & fixnum_mask) == fixnum_tag {
		return Make_fixnum(1)
	}

	if (uintptr(unsafe.Pointer(num)) & heap_mask) != heap_tag {
		panic("bad type")
	}

	switch n := (*num).(type) {
	case *big.Int:
		return Make_fixnum(1)
	case *big.Rat:
		return wrap(n.Denom())
	case float64:
		panic("TODO: denominator for float64")
	case complex128:
		panic("undefined")
	}
	panic("bad type")
}


// func number_equal(x,y Obj) Obj {
// 	xfx := (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag
// 	yfx := (uintptr(unsafe.Pointer(y)) & fixnum_mask) == fixnum_tag
// 	if xfx && yfx {	return Make_boolean(x == y) }

// 	if (!xfx && (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag) ||
// 		(!yfx && (uintptr(unsafe.Pointer(y)) & heap_mask) != heap_tag) {
// 		panic("bad type")
// 	}

// 	if xfx { return number_equal(y,x) }

// 	switch vx := (*x).(type) {
// 	case *big.Int:
// 		if yfx {
// 			vy := big.NewInt(int64(fixnum_to_int(y)))
// 			return Make_boolean(vx.Cmp(vy) == 0)
// 		}
// 		switch vy := (*y).(type) {
// 		case *big.Int:
// 			return Make_boolean(vx.Cmp(vy) == 0)
// 		case *big.Rat:
// 			return number_equal(y,x)
// 		default:
// 			panic("bad type")
// 		}
// 	case *big.Rat:
// 		// rationals should always have been converted into
// 		// other types if the denominator is one
// 		if yfx { return False }
// 		switch vy := (*y).(type) {
// 		case *big.Int:
// 			return False
// 		case *big.Rat:
// 			return Make_boolean(vx.Cmp(vy) == 0)
// 		default:
// 			panic("bad type")
// 		}
// 	}
// 	panic("bad type")
// }

func number_add(x,y Obj) Obj {
	xfx := (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag
	yfx := (uintptr(unsafe.Pointer(y)) & fixnum_mask) == fixnum_tag
	if xfx && yfx {
		i1 := uintptr(unsafe.Pointer(x))
		i2 := uintptr(unsafe.Pointer(y))
		r := (int(i1) >> fixnum_shift) + (int(i2) >> fixnum_shift)
		if r > fixnum_min && r < fixnum_max {
			return Make_fixnum(r)
		} else {
			return wrap(big.NewInt(int64(r)))
		}
	}

	if (!xfx && (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag) ||
		(!yfx && (uintptr(unsafe.Pointer(y)) & heap_mask) != heap_tag) {
		panic("bad type")
	}

	if xfx { return number_add(y,x) }

	switch vx := (*x).(type) {
	case *big.Int:
		var z *big.Int = big.NewInt(0)
		if yfx {
			vy := big.NewInt(int64(fixnum_to_int(y)))
			return wrap(z.Add(vx,vy))
		}
		switch vy := (*y).(type) {
		case *big.Int:
			return wrap(z.Add(vx,vy))
		default:
			panic("bad type")
		}
	}
	panic("bad type")
}

func number_subtract(x,y Obj) Obj {
	xfx := (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag
	yfx := (uintptr(unsafe.Pointer(y)) & fixnum_mask) == fixnum_tag
	if xfx && yfx {
		i1 := uintptr(unsafe.Pointer(x))
		i2 := uintptr(unsafe.Pointer(y))
		r := (int(i1) >> fixnum_shift) - (int(i2) >> fixnum_shift)
		if r > fixnum_min && r < fixnum_max {
			return Make_fixnum(r)
		} else {
			return wrap(big.NewInt(int64(r)))
		}
	}

	if (!xfx && (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag) ||
		(!yfx && (uintptr(unsafe.Pointer(y)) & heap_mask) != heap_tag) {
		panic("bad type")
	}

	if xfx { return number_subtract(y,x) }

	switch vx := (*x).(type) {
	case *big.Int:
		var z *big.Int = big.NewInt(0)
		if yfx {
			vy := big.NewInt(int64(fixnum_to_int(y)))
			return wrap(z.Sub(vx,vy))
		}
		switch vy := (*y).(type) {
		case *big.Int:
			return wrap(z.Sub(vx,vy))
		default:
			panic("bad type")
		}
	}
	panic("bad type")
}

func number_divide(x,y Obj) Obj {
	xfx := (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag
	yfx := (uintptr(unsafe.Pointer(y)) & fixnum_mask) == fixnum_tag
	if xfx && yfx {
		i1 := int(uintptr(unsafe.Pointer(x))) >> fixnum_shift
		i2 := int(uintptr(unsafe.Pointer(y))) >> fixnum_shift
		// A good optimizer will combine the div and mod into
		// one instruction.
		r, m := i1 / i2, i1 % i2
		if m == 0 && r > fixnum_min && r < fixnum_max {
			return Make_fixnum(r)
		} else {
			return wrap(big.NewRat(int64(i1),int64(i2)))
		}
	}

	if (!xfx && (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag) ||
		(!yfx && (uintptr(unsafe.Pointer(y)) & heap_mask) != heap_tag) {
		panic("bad type")
	}

	if xfx { return number_divide(y,x) }

	switch vx := (*x).(type) {
	case *big.Int:
		var z *big.Int = big.NewInt(0)
		if yfx {
			vy := big.NewInt(int64(fixnum_to_int(y)))
			return wrap(z.Add(vx,vy))
		}
		switch vy := (*y).(type) {
		case *big.Int:
			return wrap(z.Add(vx,vy))
		default:
			panic("bad type")
		}
	}
	panic("bad type")
}

func number_cmp(x,y Obj) Obj {
	xfx := (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag
	yfx := (uintptr(unsafe.Pointer(y)) & fixnum_mask) == fixnum_tag
	if xfx && yfx {
		i1 := int(uintptr(unsafe.Pointer(x))) >> fixnum_shift
		i2 := int(uintptr(unsafe.Pointer(y))) >> fixnum_shift
		switch {
		case i1 > i2: return Make_fixnum(1)
		case i1 < i2: return Make_fixnum(-1)
		default: return Make_fixnum(0)
		}
	}

	if (!xfx && (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag) ||
		(!yfx && (uintptr(unsafe.Pointer(y)) & heap_mask) != heap_tag) {
		panic("bad type")
	}

	if xfx { return number_subtract(y,x) }

	switch vx := (*x).(type) {
	case *big.Int:
		if yfx {
			vy := big.NewInt(int64(fixnum_to_int(y)))
			return Make_fixnum(vx.Cmp(vy))
		}
		switch vy := (*y).(type) {
		case *big.Int:
			return Make_fixnum(vx.Cmp(vy))
		case *big.Rat:
			r := big.NewRat(1, 1).SetInt(vx)
			return Make_fixnum(r.Cmp(vy))
		case complex128:
			panic("comparison on complex numbers is undefined")
		default:
			panic("bad type")
		}
	case *big.Rat:
		if yfx {
			vy := big.NewRat(int64(fixnum_to_int(y)), 1)
			return Make_fixnum(vx.Cmp(vy))
		}
		switch vy := (*y).(type) {
		case *big.Int:
			r := big.NewRat(1, 1).SetInt(vy)
			return Make_fixnum(vx.Cmp(r))
		case *big.Rat:
			return Make_fixnum(vx.Cmp(vy))
		case complex128:
			panic("comparison on complex numbers is undefined")
		default:
			panic("bad type")
		}
	case complex128:
		panic("comparison on complex numbers is undefined")
	}
	panic("bad type")
}

func bitwise_ior(x,y Obj) Obj {
	xfx := (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag
	yfx := (uintptr(unsafe.Pointer(y)) & fixnum_mask) == fixnum_tag
	if xfx && yfx {
		i1 := uintptr(unsafe.Pointer(x))
		i2 := uintptr(unsafe.Pointer(y))
		return Obj(unsafe.Pointer(uintptr(i1 | i2)))
	}

	if (!xfx && (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag) ||
		(!yfx && (uintptr(unsafe.Pointer(y)) & heap_mask) != heap_tag) {
		panic("bad type")
	}

	if xfx { return bitwise_ior(y,x) }

	switch vx := (*x).(type) {
	case *big.Int:
		var z *big.Int = big.NewInt(0)
		if yfx {
			vy := big.NewInt(int64(fixnum_to_int(y)))
			return wrap(z.Or(vx,vy))
		}
		switch vy := (*y).(type) {
		case *big.Int:
			return wrap(z.Or(vx,vy))
		default:
			panic("bad type")
		}
	}
	panic("bad type")
}

func bitwise_and(x,y Obj) Obj {
	xfx := (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag
	yfx := (uintptr(unsafe.Pointer(y)) & fixnum_mask) == fixnum_tag
	if xfx && yfx {
		i1 := uintptr(unsafe.Pointer(x))
		i2 := uintptr(unsafe.Pointer(y))
		return Obj(unsafe.Pointer(uintptr(i1 & i2)))
	}

	if (!xfx && (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag) ||
		(!yfx && (uintptr(unsafe.Pointer(y)) & heap_mask) != heap_tag) {
		panic("bad type")
	}

	if xfx { return bitwise_and(y,x) }

	switch vx := (*x).(type) {
	case *big.Int:
		var z *big.Int = big.NewInt(0)
		if yfx {
			vy := big.NewInt(int64(fixnum_to_int(y)))
			return wrap(z.And(vx,vy))
		}
		switch vy := (*y).(type) {
		case *big.Int:
			return wrap(z.And(vx,vy))
		default:
			panic("bad type")
		}
	}
	panic("bad type")
}

func bitwise_arithmetic_shift_right(x,y Obj) Obj {
	xfx := (uintptr(unsafe.Pointer(x)) & fixnum_mask) == fixnum_tag
	yfx := (uintptr(unsafe.Pointer(y)) & fixnum_mask) == fixnum_tag
	if !yfx { panic("bad shift amount") }
	// TODO: check the amount. shouldn't be negative, and perhaps
	// '>>' does a modulo on the amount.
	amount := uint(uintptr(unsafe.Pointer(y)) >> fixnum_shift)
	if xfx {
		i1 := uintptr(unsafe.Pointer(x)) >> fixnum_shift
		return Obj(unsafe.Pointer(uintptr(((i1 >> amount) << fixnum_shift) | fixnum_tag)))
	} else if (uintptr(unsafe.Pointer(x)) & heap_mask) != heap_tag {
		panic("bad type")
	}

	switch vx := (*x).(type) {
	case *big.Int:
		var z *big.Int = big.NewInt(0)
		return wrap(z.Rsh(vx, amount))
	}
	panic("bad type")
}


func _number_to_string(num Obj, radix Obj) Obj {
	var format string

	switch number_to_int(radix) {
	case 2: format = "%b"
	case 8: format = "%o"
	case 10: format = "%d"
	default: format = "%x"
	}

	if fixnum_p(num) != False {
		return String_string(fmt.Sprintf(format, fixnum_to_int(num)))
	}

	switch v := (*num).(type) {
	case *big.Int:
		return String_string(fmt.Sprintf(format, v))
	case *big.Rat:
		return String_string(fmt.Sprintf(format + "/" + format,
			v.Num(), v.Denom()))
	case float64:
		if format != "%d" {
			panic("inexact numbers can only be printed decimally")
		}
		return String_string(fmt.Sprintf("%g", v))
	case complex128:
		if format != "%d" {
			panic("inexact numbers can only be printed decimally")
		}
		// %+f gives trailing zeroes, but %+g loses decimal
		// points, which is worse
		format = "%+f%+fi"
		return String_string(fmt.Sprintf(format, real(v), imag(v)))
	// case *Compnum:
	}

	panic("number->string needs numbers")
}

// TODO: handle flonums, compnums, ratnums, etc
func _string_to_number(_str Obj, _radix Obj) Obj {
	if is_immediate(_str) { panic("bad type") }
	str := string((*_str).([]int))

	radix := number_to_int(_radix)
	switch {
	case strings.HasPrefix(str, "#b"): radix = 2; str = str[2:]
	case strings.HasPrefix(str, "#o"): radix = 8; str = str[2:]
	case strings.HasPrefix(str, "#d"): radix = 10; str = str[2:]
	case strings.HasPrefix(str, "#x"): radix = 16; str = str[2:]
	}

	var v big.Int
	z, s := v.SetString(str, radix)
	if !s { return False }
	if z.Cmp(fixnum_max_Int) < 1 && z.Cmp(fixnum_min_Int) > -1 {
		return Make_fixnum(int(z.Int64()))
	}
	return wrap(z)
}
