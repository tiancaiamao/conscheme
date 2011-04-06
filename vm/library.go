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

// Standard library for conscheme

package conscheme

import (
	"big"
	"fmt"
	"os"
)

// Numbers

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
	}

	panic("number->string needs numbers")
}

// Misc

func Command_line() Obj {
	ret := Eol
	for i := len(os.Args)-1; i >= 0; i-- {
		arg := ([]int)(os.Args[i])
		var vv interface{} = arg
		ret = Cons(Obj(&vv), ret)
	}
	return ret
}

