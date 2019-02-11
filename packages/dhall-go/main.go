package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/sirupsen/logrus"
	"github.com/ugorji/go/codec"
	"gopkg.in/alecthomas/kingpin.v2"

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
	var verbose bool
	var verbosity string

	app := kingpin.New(
		"dhall",
		"The non-repetitive alternative to YAML.",
	).Action(func(context *kingpin.ParseContext) error {
		if verbose {
			log.(*logrus.Logger).Level = logrus.DebugLevel
		} else {
			switch verbosity {
			case "debug":
				log.(*logrus.Logger).Level = logrus.DebugLevel
			case "error":
				log.(*logrus.Logger).Level = logrus.ErrorLevel
			case "info":
				log.(*logrus.Logger).Level = logrus.InfoLevel
			case "warn":
				log.(*logrus.Logger).Level = logrus.WarnLevel
			default:
				log.(*logrus.Logger).Level = logrus.WarnLevel
			}
		}

		log = log.WithFields(logrus.Fields{"command": context})

		return nil
	})

	app.Flag(
		"verbose",
		"Log as much information as possible (verbosity `debug`).",
	).Short('v').BoolVar(&verbose)

	app.Flag(
		"verbosity",
		"Select the minimum level to log (`debug`, `info`, `warn`, or `error`).",
	).EnumVar(&verbosity, "debug", "info", "warn", "error")

	app.Command(
		"decode",
		"Decode the given binary value to a Dhall expression.",
	).Action(func(*kingpin.ParseContext) error {
		decoded := decode(&log)
		fmt.Println(dhall.Render(decoded))
		return nil
	})

	app.Command(
		"encode",
		"Encode the given Dhall expression to binary.",
	).Action(func(*kingpin.ParseContext) error {
		encoded := encode(&log)
		os.Stdout.Write(encoded)
		return nil
	})

	app.Command(
		"parse",
		"Parse the given Dhall expression.",
	).Action(func(*kingpin.ParseContext) error {
		expression := parse(&log)
		fmt.Println(dhall.Render(expression))
		return nil
	}).Default()

	kingpin.MustParse(app.Parse(os.Args[1:]))
}

func decode(log *logrus.FieldLogger) dhall.Expression {
	(*log).Info("Decoding binary")
	handle := &codec.CborHandle{}

	input, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		(*log).WithFields(logrus.Fields{"err": err}).Fatal(
			"Could not read from STDIN",
		)
	}
	*log = (*log).WithFields(logrus.Fields{"input": input})

	decoded, err := dhall.Decode(handle, input)
	if err != nil {
		(*log).WithFields(logrus.Fields{"err": err}).Fatal(
			"Could not decode binary",
		)
	}
	*log = (*log).WithFields(logrus.Fields{"decoded": decoded})
	(*log).Debug("Successfully decoded binary")

	return decoded
}

func encode(log *logrus.FieldLogger) []byte {
	expression := parse(log)
	(*log).Info("Encoding expression")
	handle := &codec.CborHandle{}

	encoded, err := dhall.Encode(handle, expression)
	if err != nil {
		(*log).WithFields(logrus.Fields{"err": err}).Fatal(
			"Could not encode expression",
		)
	}
	*log = (*log).WithFields(logrus.Fields{"encoded": encoded})
	(*log).Debug("Successfully encoded expression")

	return encoded
}

func parse(log *logrus.FieldLogger) dhall.Expression {
	(*log).Infof("Parsing input")

	input, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		(*log).WithFields(logrus.Fields{"err": err}).Fatal(
			"Could not read from STDIN",
		)
	}
	*log = (*log).WithFields(logrus.Fields{"input": input})

	expression, err := dhall.Parse(log, input)
	if err != nil {
		(*log).WithFields(logrus.Fields{"err": err}).Fatal(
			"Could not parse input",
		)
	}
	*log = (*log).WithFields(logrus.Fields{"expression": expression})
	(*log).Debugf("Successfully parsed input")

	return expression
}
