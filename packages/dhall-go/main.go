package main

import (
	"os"

	"github.com/sirupsen/logrus"
	"github.com/ugorji/go/codec"

	"github.com/joneshf/open-source/packages/dhall-go/dhall"
)

func main() {
	var log logrus.FieldLogger = &logrus.Logger{
		Formatter: &logrus.TextFormatter{
			DisableLevelTruncation: true,
			FullTimestamp:          true,
		},
		Level: logrus.DebugLevel,
		Out:   os.Stderr,
	}
	handle := &codec.CborHandle{}
	expr, err := dhall.Parse(&log, "True == False")
	if err != nil {
		log.Fatalf("Failed to parse: %+v\n", err)
	}
	log.Debugf("Successfully parsed: %+v\n", expr)

	encoded, err := dhall.Encode(handle, expr)
	if err != nil {
		log.Fatalf("Failed to encode %+v: %+v\n", expr, err)
	}
	log.Debugf("Successfully encoded %+v: %+X\n", expr, encoded)

	decoded, err := dhall.Decode(handle, encoded)
	if err != nil {
		log.Fatalf("Failed to decode %+v: %+v\n", expr, err)
	}
	log.Debugf("Successfully decoded %+v: %+v\n", expr, decoded)
}
