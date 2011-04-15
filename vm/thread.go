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

package conscheme

import (
	"runtime"
	// "time"
)

type Thread struct {
	name, specific Obj
	cstart chan bool
}

func _make_thread(thunk, name Obj) Obj {
	if procedure_p(thunk) == False { panic("bad type") }
	cstart := make(chan bool)
	t := Thread{name, False, cstart}
	thread := wrap(t)
	go func (thunk,current_thread Obj) {
		t := (*current_thread).(Thread)
		<- t.cstart
		// TODO: pass current_thread to ap here
		ap(thunk, nil, current_thread)
	} (thunk,thread);
	return thread
}

func thread_p(thread Obj) Obj {
	if is_immediate(thread) { return False }
	switch v := (*thread).(type) {
	case Thread: return True
	}
	return False
}

func thread_name(thread Obj) Obj {
	if is_immediate(thread) { panic("bad type") }
	t := (*thread).(Thread)
	return t.name
}

func thread_specific(thread Obj) Obj {
	if is_immediate(thread) { panic("bad type") }
	t := (*thread).(Thread)
	return t.specific
}

func thread_specific_set_ex(thread, v Obj) Obj {
	if is_immediate(thread) { panic("bad type") }
	t := (*thread).(Thread)
	t.specific = v
	return Void
}

func thread_yield_ex() Obj {
	runtime.Gosched()
	return Void
}

// XXX: should protect against calling thread-start! twice. use a semaphore.
func thread_start_ex(thread Obj) Obj {
	if is_immediate(thread) { panic("bad type") }
	t := (*thread).(Thread)
	t.cstart <- true
	return Void
}
