// Copyright (C) 2011 Per Odlund <per.odlund@gmail.com>
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

// A simple deserializer for reading vm-code

package conscheme

import (
	"io"
	"os"
	"fmt"
	"big"
	"encoding/binary"
)

const (
	Integer = 0
	Pair	= 1
	Vector	= 2
	Null	= 3
	String	= 4
	Symbol	= 5
	Boolean	= 6
	Char	= 7
	Rational = 8
	Float64 = 9
	Complex128 = 10
	// Comp = 11
	Bytevector = 12
)

const Version = 1

var HeaderError os.Error = os.ErrorString("wrong format on byte header")

type Deserializer struct {
	r io.Reader
	version byte
}

func (d *Deserializer) readMagic() (os.Error) {
	s := "conscheme serialized object format\n"
	buf := make([]byte, len (s))
	_, e := io.ReadFull(d.r, buf)
	if e != nil {
		return e;
	}
	if s != string(buf) {
		return HeaderError;
	}
	return nil
}

func (d *Deserializer) readVersion() byte {
	var buf [1]byte
	d.r.Read(buf[0:1])
	return buf[0]
}

func (d *Deserializer) readInt() *big.Int {
	var buf [1]byte
	io.ReadFull(d.r, buf[0:1])
	sign := buf[0]
	v := big.NewInt(0)
	tmp := big.NewInt(0)
	for i := uint(0); ;i += 7 {
		io.ReadFull(d.r,buf[0:1])
		tmp = tmp.SetInt64(int64(buf[0] & 0x7F))
		tmp = tmp.Lsh(tmp, i)
		v = v.Or(v,tmp)
		if (buf[0] & 0x80) == 0 {
			break;
		}
	}
	if sign == 1 {
		return v.Neg(v)
	}
	return v
}

func (d *Deserializer) readString(i int64) string {
	b := make([]byte, i)
	d.r.Read(b)
	return string(b)
}

func (d *Deserializer) ReadObject() Obj {
	tag := d.readInt().Int64()
	length := d.readInt()
	switch tag {
	case Integer:
		if length.Cmp(fixnum_min_Int)<0 || length.Cmp(fixnum_max_Int)>0 {
			var vv interface{} = length
			return Obj(&vv)
		}
		return Make_fixnum(int(length.Int64()))
	case Rational:
		i := big.NewRat(1,1)
		return wrap(i.SetFrac(length, d.readInt()))
	case Float64:
		var v float64
		binary.Read(d.r, binary.LittleEndian, &v)
		return wrap(v)
	case Complex128:
		var v complex128
		binary.Read(d.r, binary.LittleEndian, &v)
		return wrap(v)
	case Pair:
		o1 := d.ReadObject()
		o2 := d.ReadObject()
		return Cons(o1,o2)
	case Vector:
		l := length.Int64() // fix
		obj := Make_vector(Make_fixnum(int(l)),Void)
		var i int64 = 0
		for ; i < l; i++ {
			t := d.ReadObject()
			Vector_set_ex(obj,Make_fixnum(int(i)),t)
		}
		return obj
	case Null:
		return Eol
	case String:
		s := d.readString(length.Int64())
		return String_string(s)
	case Symbol:
		i := length.Int64()
		s := d.readString(i)
		return String_to_symbol(String_string(s))
	case Bytevector:
		b := make([]byte, length.Int64())
		d.r.Read(b)
		return wrap(b)
	case Boolean:
		if length.Int64() != 0 {
			return Make_boolean(true)
		}
		return Make_boolean(false)
	case Char:
		c := length.Int64()
		return Make_char(int(c))
	}
	s := fmt.Sprintf("unknown tag: %v length: %v", tag, length)
	panic(s)
}

func NewReader(r io.Reader) (*Deserializer, os.Error) {
	d := new(Deserializer)
	d.r = r
	if e := d.readMagic(); e != nil {
		return nil,e
	}
	d.readVersion()
	return d,nil
}
