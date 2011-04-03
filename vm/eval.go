// Copyright (C) 2011 Göran Weinholt <goran@weinholt.se>

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

/// Simple tree interpreter for Scheme. We'll use this to iron out a
/// few details before we hopefully go over to a bytecode VM.

// Limitations: wrong semantics for mutable variables, and no TCO. No
// call/cc, etc.

package conscheme

import (
	"fmt"
)

// Symbol constants used by eval
var Begin Obj
var Define Obj
var If Obj
var Lambda Obj
var Quote Obj
var Set_ex Obj
var _Funcall Obj
var _Primcall Obj

func init() {
	Begin = intern("begin")
	Define = intern("define")
	If = intern("if")
	Lambda = intern("lambda")
	Quote = intern("quote")
	Set_ex = intern("set!")
	_Funcall = intern("$funcall")
	_Primcall = intern("$primcall")
}

type Procedure struct {
	name string
	required int
	formals Obj
	apply func (proc Procedure, args []Obj, lexenv map[string]Obj) Obj
	// These parts are specific to eval
	lexenv map[string]Obj
	body Obj
}

// Top-level environment. Should there be one of these per process, or
// should there just be a lock around it? In a bytecode VM we can
// actually skip the hashing and do the "hashtable lookup" at compile
// time, so there would mostly not need to be any locking.
var env map[string]Obj = make(map[string]Obj)

func lookup(name Obj, lexenv map[string]Obj) Obj {
	sname := (*name).(string)
	if lexenv != nil {
		if binding, is_bound := lexenv[sname]; is_bound {
			return binding
		}
	}
	if binding, is_bound := env[sname]; is_bound {
		return binding
	}
	panic(fmt.Sprintf("unbound variable: %s",sname))
}

func lambda_apply(proc Procedure, args []Obj, lexenv map[string]Obj) Obj {
	// XXX: This actually assumes there's a pass that transforms
	// mutable variables into cells
	newenv := make(map[string]Obj)
	for k,v := range lexenv { newenv[k] = v }
	// Extend newenv using formals + args
	if len(args) < proc.required {
		panic("Too few arguments to procedure")
	}
	formals := proc.formals
	var i int
	for i = 0; formals != Eol; i++ {
		switch f := (*formals).(type) {
		case string:
			// cons up the rest of the arguments
			v := args[i:]
			rest := Eol
			for i := len(v)-1; i >= 0; i-- {
				rest = Cons(v[i],rest)
			}
			newenv[f] = rest
			return ev(proc.body, true, newenv)
		case *[2]Obj:
			name := (*f[0]).(string) // car
			newenv[name] = args[i]
			formals = f[1] // cdr
		default:
			// Should never happen
			panic("invalid lambda formals")
		}
	}
	if i != len(args) {
		panic("Too many arguments to procedure")
	}
	return ev(proc.body, true, newenv)
}

func ev(origcode Obj, tailpos bool, lexenv map[string]Obj) Obj {
	code := origcode
	defer func() {
		if err := recover(); err != nil {
			fmt.Printf("Error in Scheme code: %v\n", err)
			Write(origcode)
			fmt.Printf("\n")
			panic("no error recovery yet")
		}
	}()
	// fmt.Printf("eval: ")
	// Write(code)
	// fmt.Printf("\n")
	if symbol_p(code) != False {
		return lookup(code, lexenv)
	}

	switch cmd := car(code); cmd {
	case Begin:
		var ret Obj
		for code = cdr(code); code != Eol; code = cdr(code) {
			// fmt.Printf("begin: ")
			// Write(car(code))
			// fmt.Printf("\n")
			ret = ev(car(code), tailpos && cdr(code) == Eol, lexenv)
		}
		return ret
	case Define:
		code = cdr(code); name := car(code)
		code = cdr(code)
		sname := (*name).(string)
		env[sname] = ev(car(code), true, lexenv)
		return Void
	case If:
		code = cdr(code); test := car(code)
		code = cdr(code); consequent := car(code)
		code = cdr(code); alternative := car(code)
		if ev(test, false, lexenv) == False {
			return ev(alternative, tailpos, lexenv)
		} else {
			return ev(consequent, tailpos, lexenv)
		}
	case Lambda:
		var closure Procedure
		code = cdr(code); closure.formals = car(code)
		code = cdr(code); closure.body = car(code)
		closure.lexenv = lexenv
		closure.name = "unnamed"
		closure.apply = lambda_apply
		closure.required = 0
		for formals := closure.formals; formals != Eol; {
			switch f := (*formals).(type) {
			case string:
				formals = Eol
			case *[2]Obj:
				closure.required++
				formals = f[1] // cdr
			}
		}
		var vv interface{} = closure
		return Obj(&vv)
	case Set_ex:
		code = cdr(code); name := car(code)
		code = cdr(code)
		sname := (*name).(string)
		value := ev(car(code), true, lexenv)
		if lexenv != nil {
			if _, is_bound := lexenv[sname]; is_bound {
				lexenv[sname] = value
				return Void
			}
		}
		env[sname] = value
		return Void
	case Quote:
		return car(cdr(code))

	case _Funcall:
		// Procedure call
		code := cdr(code)
		fun := ev(car(code), false, lexenv)
		code = cdr(code)
		args := make([]Obj, fixnum_to_int(Length(code)))
		for i := 0; code != Eol; i, code = i+1, cdr(code) {
			args[i] = ev(car(code), false, lexenv)
		}
		return ap(fun, args, lexenv)
	case _Primcall:
		code = cdr(code)
		primop := (*car(code)).(string)
		code = cdr(code)
		return evprim(primop, code, lexenv)
	default:
		name := (*cmd).(string)
		panic(fmt.Sprintf("Unimplemented syntax: %s",name))
	}

	panic("One of the eval cases did not return")
}

func ap(oproc Obj, args []Obj, lexenv map[string]Obj) Obj {
	// oproc should be a Procedure.
	proc := (*oproc).(Procedure)
	return proc.apply(proc, args, lexenv)
}

// Runs the simple language emitted by the "compiler"
func Eval(code Obj) Obj {
	return ev(code, true, nil)
}
