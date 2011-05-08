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
	"fmt"
	"io"
	"os"
	"unicode"
	"utf8"
)

// Characters

func char_whitespace_p(x Obj) Obj {
	if char_p(x) == False { panic("bad type") }
	return Make_boolean(unicode.IsSpace(char_to_int(x)))
}

func char_upcase(x Obj) Obj {
	if char_p(x) == False { panic("bad type") }
	return Make_char(unicode.ToUpper(char_to_int(x)))
}

func char_downcase(x Obj) Obj {
	if char_p(x) == False { panic("bad type") }
	return Make_char(unicode.ToLower(char_to_int(x)))
}

// Input and output

type InputPort struct {
	r io.Reader
	is_binary bool
	// one byte of lookahead if is_binary, otherwise one codepoint
	lookahead_valid int
	lookahead [utf8.UTFMax]byte
}

type OutputPort struct {
	w io.Writer
	is_binary bool
	buf [1]byte
}

var stdin, curin, stdout, curout Obj

func init() {
	stdin = wrap(&InputPort{r: os.Stdin, is_binary: false})
	curin = stdin

	stdout = wrap(&OutputPort{w: os.Stdout, is_binary: false})
	curout = stdout
}

func port_p(x Obj) Obj {
	if is_immediate(x) { return False }

	switch v := (*x).(type) {
	case *InputPort:
		return True
	case *OutputPort:
		return True
	}
	return False
}

func input_port_p(x Obj) Obj {
	if is_immediate(x) { return False }

	switch v := (*x).(type) {
	case *InputPort:
		return True
	}
	return False
}

func output_port_p(x Obj) Obj {
	if is_immediate(x) { return False }

	switch v := (*x).(type) {
	case *OutputPort:
		return True
	}
	return False
}

// Called by Obj_display
func display_port(out io.Writer, port Obj) {
	name := "*unnamed*"
	var is_binary bool
	switch v := (*port).(type) {
	case *OutputPort:
		fmt.Fprintf(out, "#<output-port ")
		is_binary = v.is_binary
		switch f := v.w.(type) {
		case *os.File: name = f.Name()
		}
	case *InputPort:
		fmt.Fprintf(out, "#<input-port ")
		is_binary = v.is_binary
		switch f := v.r.(type) {
		case *os.File: name = f.Name()
		}
	}
	if is_binary {
		fmt.Fprintf(out, "binary")
	} else {
		fmt.Fprintf(out, "textual")
	}
	fmt.Fprintf(out, " %s>", name)
}

// XXX: curin and curout should be handled with dynamic-wind and
// should be thread local
func current_input_port() Obj {
	return curin
}

func current_output_port() Obj {
	return curout
}

func file_exists_p(fn Obj) Obj {
	if is_immediate(fn) { panic("bad type") }
	_,err := os.Stat(string((*fn).([]int)))
	return Make_boolean(err == nil)
}

func delete_file(fn Obj) Obj {
	if is_immediate(fn) { panic("bad type") }
	if err := os.Remove(string((*fn).([]int))); err != nil {
		panic(fmt.Sprintf("I/O error",err))
	}
	return Void
}

func open_input_file(fn Obj) Obj {
	if is_immediate(fn) { panic("bad type") }
	f, e := os.Open(string((*fn).([]int)), os.O_RDONLY, 0666)
	if e != nil { panic(fmt.Sprintf("I/O error: %s", e)) }
	return wrap(&InputPort{r: f, is_binary: false})
}

func open_file_output_port(fn Obj) Obj {
	// TODO: takes three more arguments
	if is_immediate(fn) { panic("bad type") }
	f, e := os.Open(string((*fn).([]int)), os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0666)
	if e != nil { panic(fmt.Sprintf("I/O error: %s", e)) }
	return wrap(&OutputPort{w: f, is_binary: true})
}

func close_input_port(port Obj) Obj {
	if is_immediate(port) { panic("bad type") }
	v := (*port).(*InputPort)
	switch f := v.r.(type) {
	case *os.File: f.Close()
	}
	return Void
}

func close_output_port(port Obj) Obj {
	if is_immediate(port) { panic("bad type") }
	v := (*port).(*OutputPort)
	switch f := v.w.(type) {
	case *os.File: f.Close()
	}
	return Void
}

func close_port(port Obj) Obj {
	if is_immediate(port) { panic("bad type") }
	switch v := (*port).(type) {
	case *InputPort:
		switch f := v.r.(type) {
		case *os.File: f.Close()
		}
	case *OutputPort:
		switch f := v.w.(type) {
		case *os.File: f.Close()
		}
	}
	return Void
}

func _read_char(port Obj) Obj {
	if is_immediate(port) { panic("bad type") }
	switch v := (*port).(type) {
	case *InputPort:
		if v.is_binary { panic("bad port type") }
		for ; !utf8.FullRune(v.lookahead[0:v.lookahead_valid]); {
			n, err := io.ReadFull(v.r,
				v.lookahead[v.lookahead_valid:v.lookahead_valid + 1])
			v.lookahead_valid += n
			switch {
			case err == os.EOF: return Eof
			case err != nil: panic("I/O read error")
			}
		}
		cp, _ := utf8.DecodeRune(v.lookahead[0:v.lookahead_valid])
		v.lookahead_valid = 0
		return Make_char(cp)
	}
	panic("bad type")
}

func _peek_char(port Obj) Obj {
	if is_immediate(port) { panic("bad type") }
	switch v := (*port).(type) {
	case *InputPort:
		if v.is_binary { panic("bad port type") }
		for ; !utf8.FullRune(v.lookahead[0:v.lookahead_valid]); {
			n, err := io.ReadFull(v.r,
				v.lookahead[v.lookahead_valid:v.lookahead_valid + 1])
			v.lookahead_valid += n
			switch {
			case err == os.EOF: return Eof
			case err != nil: panic("I/O read error")
			}
		}
		cp, _ := utf8.DecodeRune(v.lookahead[0:v.lookahead_valid])
		return Make_char(cp)
	}
	panic("bad type")
}

func _write_char(ch,port Obj) Obj {
	if char_p(ch) == False || is_immediate(port) { panic("bad type") }
	switch v := (*port).(type) {
	case *OutputPort:
		if v.is_binary { panic("bad port type") }
		buf := make([]int,1)
		buf[0] = char_to_int(ch)
		_, err := io.WriteString(v.w,string(buf))
		// XXX: should check number of bytes written
		if err != nil {
			panic("I/O write error")
		}
		return Void
	}
	panic("bad type")
}

func lookahead_u8(port Obj) Obj {
	if is_immediate(port) { panic("bad type") }
	switch v := (*port).(type) {
	case *InputPort:
		if !v.is_binary { panic("bad port type") }
		if v.lookahead_valid == 0 {
			n, err := io.ReadFull(v.r,v.lookahead[0:1])
			if n != 1 {
				return Eof
			}
			if err != nil {
				panic("I/O read error")
			}
		}
		v.lookahead_valid = 1
		return Make_fixnum(int(v.lookahead[0]))
	}
	panic("bad type")
}

func get_u8(port Obj) Obj {
	if is_immediate(port) { panic("bad type") }
	switch v := (*port).(type) {
	case *InputPort:
		if !v.is_binary { panic("bad port type") }
		if v.lookahead_valid == 0 {
			n, err := io.ReadFull(v.r,v.lookahead[0:1])
			if n != 1 {
				return Eof
			}
			if err != nil {
				panic("I/O read error")
			}
		}
		v.lookahead_valid = 0
		return Make_fixnum(int(v.lookahead[0]))
	}
	panic("bad type")
}

func put_u8(port,octet Obj) Obj {
	if is_immediate(port) { panic("bad type") }
	byt := number_to_int(octet)
	if byt < 0 || byt > 255 { panic("not an octet") }
	switch v := (*port).(type) {
	case *OutputPort:
		if !v.is_binary { panic("bad port type") }
		v.buf[0] = byte(byt)
		n, err := v.w.Write(v.buf[0:1])
		if n != 1 {
			panic("I/O short write")
		}
		if err != nil {
			panic("I/O write error")
		}

		return Void
	}
	panic("bad type")
}

func put_bytevector(port,_bv Obj) Obj {
	if is_immediate(port) || is_immediate(_bv) { panic("bad type") }
	bv := (*_bv).([]byte)
	switch v := (*port).(type) {
	case *OutputPort:
		if !v.is_binary { panic("bad port type") }
		n, err := v.w.Write(bv)
		// XXX: should loop
		if n != len(bv) {
			panic("I/O short write")
		}
		if err != nil {
			panic("I/O write error")
		}

		return Void
	}
	panic("bad type")
}

func display(x,port Obj) Obj {
	if is_immediate(port) { panic("bad type") }
	v := (*port).(*OutputPort)
	if v.is_binary { panic("bad port type") }
	Obj_display(x, v.w, False)
	return Void
}

func write(x,port Obj) Obj {
	if is_immediate(port) { panic("bad type") }
	v := (*port).(*OutputPort)
	if v.is_binary { panic("bad port type") }
	Obj_display(x, v.w, True)
	return Void
}

// Bytevectors

func bytevector_p(x Obj) Obj {
	if is_immediate(x) { return False }
	switch v := (*x).(type) {
	case []byte: return True
	}
	return False
}

func bytevector_length(x Obj) Obj {
	if is_immediate(x) { panic("bad type") }
	switch v := (*x).(type) {
	case []byte: return make_number(len(v))
	}
	panic("bad type")
}

func u8_list_to_bytevector(l Obj) Obj {
	var bv []byte = make([]byte, number_to_int(Length(l)))

	for i := 0; l != Eol; i++ {
		v := (*l).(*[2]Obj)
		octet := number_to_int(v[0])
		if octet < 0 || octet > 255 {
			panic("not an octet")
		}
		bv[i] = byte(octet)
		l = v[1]
	}

	return wrap(bv)
}

func string_to_utf8(str Obj) Obj {
	if is_immediate(str) { panic("bad type") }
	return wrap([]byte(string((*str).([]int))))
}

type ByteSink struct {
	buf []byte
	written int
}

func (sink *ByteSink) Write(buf []byte) (int, os.Error) {
	for len(sink.buf) < sink.written + len(buf) {
		nbuf := make([]byte, len(sink.buf) * 2)
		copy(nbuf, sink.buf[:sink.written])
		sink.buf = nbuf
	}

	copy(sink.buf[sink.written:], buf)

	sink.written += len(buf)

	return len(buf), nil
}

func _open_bytevector_output_port() Obj {
	sink := &ByteSink{buf: make([]byte, 32)}
	return wrap(&OutputPort{w: sink, is_binary: true})
}

func _bytevector_output_port_extract(p Obj) Obj {
	if is_immediate(p) { panic("bad type") }
	v := (*p).(*OutputPort)
	sink := (v.w).(*ByteSink)

	ret := make([]byte, sink.written)
	copy(ret, sink.buf)
	sink.written = 0

	return wrap(ret)
}

// Misc

func Command_line() Obj {
	ret := Eol
	for i := len(os.Args)-1; i >= 0; i-- {
		arg := ([]int)(os.Args[i])
		ret = Cons(wrap(arg), ret)
	}
	return ret
}

