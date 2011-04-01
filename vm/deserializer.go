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

// A simple desirializer for reading wm-code

package deserializer

import (
		"io"
		"os"
		"unsafe"
	.		"./types"
//		"fmt"
		"big"
)

const (
	Integer = 0
	Pair	= 1
	Vector	= 2
	Null	= 3
	String	= 4
	Symbol	= 5
	Boolean	= 7
	Char	= 8
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
	if _, e := io.ReadFull(d.r, buf); e != nil {
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
	d.r.Read(buf[0:1])
	sign := buf[0]
	v := big.NewInt(0)
	tmp := big.NewInt(0)
	for i := uint(0); ;i += 7 {
		d.r.Read(buf[0:1])
		tmp.SetInt64(int64(buf[0] & 0x7F))
		tmp.Lsh(tmp, i)
		v.Or(v,tmp)
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

const Void2 = Obj(unsafe.Pointer(uintptr(0x2f)))

func (d *Deserializer) readObject() Obj {
	tag := d.readInt().Int64()
	length := d.readInt()
	switch tag {
	case Integer:
		var vv interface{} = length
		return Obj(&vv)
	case Pair:
		o1 := d.readObject()
		o2 := d.readObject()
		return Cons(o1,o2)
	case Vector:
		l := length.Int64() // fix
		obj := Make_vector(Make_fixnum(int(l)),Void2)
		var i int64 = 0
		for ; i < l; l++ {
			t := d.readObject()
			Vector_set_ex(obj,Make_fixnum(int(i)),t)
		}
		return obj
	case Null:
		return Obj(nil)
	case String:
		s := d.readString(length.Int64())
		return String_string(s)
	case Symbol:
		i := length.Int64()
		s := d.readString(i)
		return String_to_symbol(String_string(s))
	case Boolean:
		if (length.Int64() == 0){
			return Make_boolean(true)
		}
		return Make_boolean(false)
	case Char:
		c := d.readInt().Int64()
		return Make_char(int(c))
	}
	panic("unknown type")
}

func NewReader(r io.Reader) (*Deserializer, os.Error) {
	d := new(Deserializer)
	d.r = r
	if e := d.readMagic(); e != nil {
		return nil,e
	}
	return d,nil
}
