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

// Runs conscheme image files

package vm

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"os"
	"runtime"
)

// When GOMAXPROCS is larger than 1, we apparently need to implement
// our own timer tick to get some sort of preemptive threading.
const SCHED_TICK = 100

const (
	// Instruction numbers
	FRAME        = 1
	RETURN       = 2
	PUSH         = 3
	MAKE_VOID    = 4
	MOVE         = 5
	CLOSURE_NAME = 6
	CLOSURE_REF  = 7
	//CLOSURE_SET_EX = 8
	CLOSURE       = 10
	TAILCALL      = 11
	CONSARGS      = 12
	CLOSURE_VAR   = 13
	FUNCALL       = 14
	JUMP          = 0
	CONST_REF     = 9
	BF            = 15
	CLOSURE_LABEL = 16
	PRIMCALL      = 17
	PRIMREF       = 18

	// Instruction fields
	I_SHIFT      = 27
	OP1_N        = 0x7f00000
	OP1_N_SHIFT  = 20
	OP1_R1       = 0xFFC00
	OP1_R1_SHIFT = 10
	OP1_R2       = 0x3ff
	OP2_N        = 0x7FFFC00
	OP2_N_SHIFT  = 10
	OP2_R        = 0x3ff
	OP3_N1       = 0x7FC0000
	OP3_N1_SHIFT = 18
	OP3_N2       = 0x3FC00
	OP3_N2_SHIFT = 10
	OP3_R        = 0x3ff
)

func int17(i uint32) int {
	// Decode a signed 17-bit integer. There must be a better way
	// to do this.
	x := int(i)
	if x <= (1<<16)-1 {
		return x
	}
	return x - (1 << 17)
}

func _validate_header_version(header Obj) {
	version := -1

	// look for the bytecode version header
	for h := header; h != Eol; h = cdr(h) {
		prop := car(h)
		name := car(prop)
		value := cdr(prop)
		if scm2str(name) == "bytecode" {
			version = number_to_int(value)
			break
		}
	}

	if version != 1 {
		panic(fmt.Sprintf("Incompatible bytecode: %d", version))
	}
}

func Conscheme(header, code Obj) Obj {
	_validate_header_version(header)

	bytecode := Vector_ref(code, Make_fixnum(0))
	constants := Vector_ref(code, Make_fixnum(1))

	// fmt.Printf("\nbytecode:")
	// Write(bytecode)
	// fmt.Printf("\nconstants:")
	// Write(constants)
	// fmt.Printf("\n")

	return _bytecode_run(bytecode, constants, primordial)
}

func try_open_image(fn string) (*Deserializer, error) {
	f, e := os.OpenFile(fn, os.O_RDONLY, 0666)
	if e != nil {
		return nil, e
	}
	d, e := NewReader(f)
	if e != nil {
		return nil, e
	}
	return d, nil
}

func _bytecode_load(fn Obj) Obj {
	// Loads bytecode from a named file. Returns a vector of
	// bytecode and constant pool.
	filename := fn.([]rune)
	d, e := try_open_image(string(filename))
	if e != nil {
		return Eol
	}
	header := d.ReadObject()
	_validate_header_version(header)
	code := d.ReadObject()
	return code
}

// Top-level environment. Should there be one of these per process, or
// should there just be a lock around it? One possible optimization:
// this is used for $global-set! and $global-ref, so it is probably
// beneficial to map names to locations at compile-time.
var env map[*ScmSym]Obj = make(map[*ScmSym]Obj)

type Procedure struct {
	name     string
	required int
	apply    func(proc *Procedure, args []Obj, ct Obj) Obj
	label    int
	free     []Obj
	code     *Code
}

func procedure_p(x Obj) Obj {
	switch (x).(type) {
	case *Procedure:
		return True
	}
	return False
}

type Code struct {
	bc     []uint32 // bytecode
	consts []Obj    // constants pool
}

type Frame struct {
	up      *Frame
	rreg    int
	savedpc int
	argnum  int // for CONSARGS
	regs    []Obj
	cc      *Procedure
	code    *Code
}

type Argstack []Obj

func (s *Argstack) Push(v Obj) {
	// XXX: check if the array is out of capacity..?
	*s = append(*s, v)
}

func (s *Argstack) Pop() Obj {
	l := len(*s)
	ret := (*s)[l-1]
	*s = (*s)[:l-1]

	return ret
}

func start_frame(size int, code *Code) *Frame {
	// Creates the frame at the top of the stack
	r := make([]Obj, size)
	for i := 0; i < size; i++ {
		r[i] = Void
	}
	return &Frame{regs: r, code: code}
}

func call_frame(up *Frame, rreg, savedpc, size int) *Frame {
	// Creates a new frame for a function call
	r := make([]Obj, size)
	for i := 0; i < size; i++ {
		r[i] = Void
	}
	return &Frame{up: up, rreg: rreg, savedpc: savedpc, regs: r}
}

func tail_frame(f *Frame, n int) {
	// make room for n args and locals
	if len(f.regs) < n {
		nf := make([]Obj, n)
		copy(nf, f.regs)
		for i := len(f.regs); i < n; i++ {
			nf[i] = Void
		}
		f.regs = nf
	}
}

func _bytecode_run(bytecode, constants, current_thread Obj) Obj {
	// The bytecode is 32-bit integers encoded in little endian format
	_bc := (bytecode).([]byte)
	bc := make([]uint32, len(_bc)/4)
	rbc := bytes.NewBuffer(_bc)

	if err := binary.Read(rbc, binary.LittleEndian, bc); err != nil {
		panic(fmt.Sprintf("Trouble converting to integers: %s", err))
	}

	i := bc[0]
	if (i >> I_SHIFT) != FRAME {
		panic(fmt.Sprintf("First instruction is not FRAME: %d", i))
	}

	argstack := make(Argstack, 16)

	// TODO: this has to link the stack to the caller's stack.
	// This means that primitives need access to the stack. That
	// will also be useful for apply and call/cc.
	return run(current_thread,
		start_frame(int(i&OP1_R2), &Code{bc, (constants).([]Obj)}),
		&argstack)
}

func run(ct Obj, stack *Frame, argstack *Argstack) Obj {
	defer func() {
		if err := recover(); err != nil {
			fmt.Printf("Error in Scheme code: %v\n", err)
			stack_trace(stack)
			panic("no error recovery yet")
		}
	}()
	pc := stack.savedpc
	cycles := 0

	for {
		cycles++
		if cycles > SCHED_TICK {
			cycles = 0
			runtime.Gosched()
		}
		i := stack.code.bc[pc]
		if false {
			name := "*unknown*"
			if stack.cc != nil {
				name = stack.cc.name
			}
			fmt.Printf("\nI=#x%x op=#b%b PC=#x%x procedure=%s\nregs: ",
				i, i>>I_SHIFT, pc, name)
			for i := range stack.regs {
				Write(stack.regs[i])
				fmt.Printf(", ")
			}
			fmt.Printf("\nargstack: %v\n", argstack)
		}

		pc += 1

		switch i >> I_SHIFT {
		// op1 format
		case FRAME:
			// TODO: use this to check that the required
			// number of arguments were passed
			continue
		case RETURN:
			v := stack.regs[i&OP1_R2]
			if stack.up == nil {
				return v
			}
			// stack_trace(stack)
			rreg := stack.rreg
			pc = stack.savedpc
			// fmt.Printf("will return to PC=#x%x in reg %d: ",pc,rreg)
			// Write(v)
			// fmt.Printf("\n")
			stack = stack.up
			stack.regs[rreg] = v
		case PUSH:
			//fmt.Printf("pushing reg %d on argument stack", i & OP1_R2)
			argstack.Push(stack.regs[i&OP1_R2])
		case MOVE:
			src := (i & OP1_R1) >> OP1_R1_SHIFT
			dst := (i & OP1_R2)
			stack.regs[dst] = stack.regs[src]
		case MAKE_VOID:
			dst := (i & OP1_R2)
			stack.regs[dst] = Void
		case CLOSURE:
			f := make([]Obj, (i&OP1_N)>>OP1_N_SHIFT)
			stack.regs[i&OP1_R2] = wrap(&Procedure{apply: aprun, free: f, code: stack.code})
		case CLOSURE_NAME:
			p := (stack.regs[i&OP1_R2]).(*Procedure)
			name := stack.regs[(i&OP1_R1)>>OP1_R1_SHIFT]
			p.name = scm2str(name)
		case CLOSURE_VAR:
			p := (stack.regs[i&OP1_R2]).(*Procedure)
			value := stack.regs[(i&OP1_R1)>>OP1_R1_SHIFT]
			freevar := (i & OP1_N) >> OP1_N_SHIFT
			p.free[freevar] = value
		case CLOSURE_REF:
			p := stack.cc
			freevar := (i & OP1_N) >> OP1_N_SHIFT
			stack.regs[i&OP1_R2] = p.free[freevar]
		case FUNCALL:
			// stack_trace(stack)
			r := int(i & OP1_R2)
			argnum := int((i & OP1_N) >> OP1_N_SHIFT)
			_p := stack.regs[(i&OP1_R1)>>OP1_R1_SHIFT]
			p := (_p).(*Procedure)
			if p.apply == nil {
				// This is a primitive.
				args := make([]Obj, argnum)
				for i := argnum - 1; i >= 0; i-- {
					args[i] = argstack.Pop()
				}
				stack.regs[r] = apprim(p, args, ct)
				continue
			}
			dst_i := p.code.bc[p.label]
			if (dst_i >> I_SHIFT) != FRAME {
				panic(fmt.Sprintf("Procedure %s at #x%x has no FRAME: #x%x",
					p.name, p.label, i))
			}
			frame := call_frame(stack, r, pc, argnum+int((dst_i&OP1_R2)))
			for i := argnum - 1; i >= 0; i-- {
				frame.regs[i] = argstack.Pop()
			}
			frame.cc = p
			frame.argnum = argnum
			frame.code = p.code
			stack = frame
			pc = p.label
			// stack_trace(stack)
			//fmt.Printf("funcall to %d (%s), new frame = %v\n",pc,p.name,stack)
		case TAILCALL:
			argnum := int((i & OP1_N) >> OP1_N_SHIFT)
			_p := stack.regs[i&OP1_R2]
			p := (_p).(*Procedure)
			dst_i := p.code.bc[p.label]
			if (dst_i >> I_SHIFT) != FRAME {
				panic(fmt.Sprintf("Procedure at #x%x has no FRAME: #x%x",
					p.label, i))
			}
			if p.apply == nil {
				panic("tail-call to primitive")
			}
			tail_frame(stack, argnum+int(dst_i&OP1_R2))
			for i := argnum - 1; i >= 0; i-- {
				stack.regs[i] = argstack.Pop()
			}
			stack.cc = p
			stack.argnum = argnum
			stack.code = p.code
			pc = p.label
			//fmt.Printf("tailcall to %d, new frame = %v\n",pc,stack)
		case CONSARGS:
			// Called at the very start of procedures with
			// rest arguments. n is how many variables are
			// in the formals of the procedure.
			n := int((i & OP1_N) >> OP1_N_SHIFT)
			rest := Eol
			for i := stack.argnum - 1; i >= n-1; i-- {
				rest = Cons(stack.regs[i], rest)
				stack.regs[i] = Void
			}
			stack.regs[n-1] = rest
		// op2 format
		case JUMP:
			disp := (i & OP2_N) >> OP2_N_SHIFT
			// fmt.Printf("JUMP DISPLACEMENT: %d = %d\n", disp, int17(disp))
			// convert to signed:
			abs := pc - 1 + int17(disp)
			pc = abs
		case CONST_REF:
			stack.regs[i&OP2_R] = stack.code.consts[(i&OP2_N)>>OP2_N_SHIFT]
		case CLOSURE_LABEL:
			p := (stack.regs[i&OP2_R]).(*Procedure)
			disp := (i & OP2_N) >> OP2_N_SHIFT
			// convert to signed:
			// fmt.Printf("CLOSURE LABEL DISPLACEMENT: %x = %x = %x\n",
			// 	disp, int17(disp), pc-1+int17(disp))
			abs := pc - 1 + int17(disp)
			p.label = abs
		case BF:
			v := stack.regs[i&OP2_R]
			if v != False {
				continue
			}
			disp := (i & OP2_N) >> OP2_N_SHIFT
			// fmt.Printf("BRANCH DISPLACEMENT: %d = %d\n",
			// 	disp, int17(disp))
			// convert to signed:
			abs := pc - 1 + int17(disp)
			pc = abs
		// op3 format
		case PRIMCALL:
			r := i & OP3_R
			primitive := (i & OP3_N2) >> OP3_N2_SHIFT
			argnum := int((i & OP3_N1) >> OP3_N1_SHIFT)
			args := make([]Obj, argnum)
			// fmt.Printf("primitive: %d, argnum: %d\nargs:",primitive,argnum)
			for i := argnum - 1; i >= 0; i-- {
				args[i] = argstack.Pop()
				// Write(args[i])
				// fmt.Printf(", ")
			}
			// fmt.Printf("\n")
			stack.regs[r] = evprimn(primitive, args, ct)
		case PRIMREF:
			r := i & OP3_R
			stack.regs[r] = primitive[(i&OP3_N2)>>OP3_N2_SHIFT]
		// unknown opcodes
		default:
			panic(fmt.Sprintf("Unimplemented bytecode op: #b%b (in #x%x)",
				i>>I_SHIFT, i))
		}
	}
}

func aprun(proc *Procedure, args []Obj, ct Obj) Obj {
	// This is only called from apply. This unfortunately means that
	// apply breaks TCO. It also breaks stack traces. TODO: do this
	// properly.
	i := proc.code.bc[proc.label]
	if (i >> I_SHIFT) != FRAME {
		panic(fmt.Sprintf("First instruction is not FRAME: %d", i))
	}
	// fmt.Printf("aprun makes a new stack :(\n")
	stack := start_frame(len(args)+int(i&OP1_R2), proc.code)
	stack.savedpc = proc.label
	stack.cc = proc
	stack.argnum = len(args)
	copy(stack.regs, args)
	// stack_trace(stack)
	argstack := make(Argstack, 16)

	return run(ct, stack, &argstack)
}

func stack_trace(stack *Frame) {
	fmt.Printf("-- STACK TRACE --\n")
	for ; stack != nil; stack = stack.up {
		name := "*unknown*"
		if stack.cc != nil {
			name = stack.cc.name
		}
		fmt.Printf("SavedPC=#x%x/#x%x  Closure=%s", stack.savedpc,
			len(stack.code.bc), name)
		fmt.Printf("  Regs=%d", len(stack.regs))
		fmt.Printf("\n")
	}
	fmt.Printf("-- END OF STACK TRACE --\n")
}
