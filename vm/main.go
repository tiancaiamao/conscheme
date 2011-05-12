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

package main

import (
	. "conscheme"
	"flag"
	"fmt"
	"os"
	"strings"
)

const imagename = "conscheme.image"
var conschemedirs []string = []string{
	"/usr/local/share",
	"/usr/share",
	// XXX: this might not be a good idea in the long run
	"./compiler",
	"."}
const dirsep = "/"
const pathsep = ":"

func usage() {
	// TODO: find the image automatically
	fmt.Fprintf(os.Stderr, "Usage: conscheme [OPTION]... [ARGUMENT]...\n")
	flag.PrintDefaults()
	os.Exit(1)
}

func tryimage(fn string) (*Deserializer, os.Error) {
	f, e := os.OpenFile(fn, os.O_RDONLY, 0666)
	if e != nil { return nil, e }
	d, e := NewReader(f)
	if e != nil { return nil, e }
	return d, nil
}

func findimage() *Deserializer {
	var boot *string = flag.String("boot", "", "conscheme boot image file")
	flag.Parse()
	os.Args = flag.Args()

	if *boot != "" {
		d, e := tryimage(*boot)
		if e != nil {
			fmt.Fprintf(os.Stderr, "Not a conscheme image file: %v\n", e)
			usage()
		}
		return d
	}

	dirs := conschemedirs
	search := os.Getenv("CONSCHEMEDIRS")
	if search != "" {
		dirs = strings.Split(search, pathsep, -1)
	}

	for i := range(dirs) {
		d, e := tryimage(fmt.Sprintf("%s%s%s",dirs[i],dirsep,imagename))
		if e == nil { return d }
	}

	return nil
}

func main() {
	d := findimage()
	if d == nil {
		fmt.Fprintf(os.Stderr, "Can't find the conscheme.image file\n")
		usage()
	}
	header := d.ReadObject()
	code := d.ReadObject()
	// Write(header)
	// fmt.Print("\n")
	Conscheme(header, code)
}
