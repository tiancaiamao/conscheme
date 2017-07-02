// Copyright (C) 2011, 2017 Göran Weinholt <goran@weinholt.se>

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

/// Leftovers from the tree interpreter

package vm

func apprim(proc *Procedure, args []Obj, ct Obj) Obj {
	// XXX: should also check if there's a maximum number of
	// arguments, like e.g. make-string
	if len(args) < proc.required {
		panic("Too few of arguments to primitive procedure")
	}
	return evprimn(uint32(proc.label), args, ct)
}

func ap(oproc Obj, args []Obj, ct Obj) Obj {
	// oproc should be a Procedure.
	proc := (oproc).(*Procedure)
	if proc.apply == nil {
		return apprim(proc, args, ct)
	} else {
		return proc.apply(proc, args, ct)
	}
}

// XXX: Legacy implementation for the apply bytecode op
func apply(args []Obj, ct Obj) Obj {
	var funargs []Obj
	fun := args[0]
	// The last argument to apply is a list
	last := args[len(args)-1]
	funargs = make([]Obj, len(args)-2+fixnum_to_int(Length(last)))
	copy(funargs, args[1:len(args)-1])
	for i := len(args) - 2; last != Eol; i, last = i+1, cdr(last) {
		funargs[i] = car(last)
	}

	return ap(fun, funargs, ct)
}
