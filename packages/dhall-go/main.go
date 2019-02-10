package main

import (
	"fmt"
	"os"

	"github.com/ugorji/go/codec"

	"github.com/joneshf/open-source/packages/dhall-go/dhall"
)

func main() {
	handle := &codec.CborHandle{}
	t := &dhall.Bool{Value: true}
	f := &dhall.Bool{Value: false}
	expr := &dhall.BoolEqual{Left: t, Right: f}

	encoded, err := dhall.Encode(handle, expr)
	if err != nil {
		fmt.Printf("Failed to encode %+v: %+v\n", expr, err)
		os.Exit(1)
	}
	fmt.Printf("Successfully encoded %+v: %+X\n", expr, encoded)

	decoded, err := dhall.Decode(handle, encoded)
	if err != nil {
		fmt.Printf("Failed to decode %+v: %+v\n", expr, err)
		os.Exit(1)
	}
	fmt.Printf("Successfully decoded %+v: %+v\n", expr, decoded)
}
