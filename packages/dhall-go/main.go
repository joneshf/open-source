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
	var output string
	var render func(dhall.Expression) string
	var verbose bool
	var verbosity string
	outputs := []string{"dhall", "json", "yaml"}
	verbosities := []string{"debug", "info", "warn", "error"}

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

		switch output {
		case "dhall":
			render = renderDhall(&log)
		case "json":
			render = renderJSON(&log)
		case "yaml":
			render = renderYAML(&log)
		default:
			render = renderDhall(&log)
		}

		return nil
	})

	app.Flag(
		"verbose",
		"Log as much information as possible (verbosity \"debug\").",
	).Short('v').BoolVar(&verbose)

	app.Flag(
		"verbosity",
		fmt.Sprintf("Select the minimum level to log %q.", verbosities),
	).Default("warn").EnumVar(&verbosity, verbosities...)

	decodeCommand := app.Command(
		"decode",
		"Decode the given binary value to a Dhall expression.",
	).Action(func(*kingpin.ParseContext) error {
		decoded := decode(&log)
		fmt.Println(render(decoded))
		return nil
	})
	decodeCommand.Flag(
		"output",
		fmt.Sprintf("Render the expression in different formats %q", outputs),
	).Default("dhall").EnumVar(&output, outputs...)

	app.Command(
		"encode",
		"Encode the given Dhall expression to binary.",
	).Action(func(*kingpin.ParseContext) error {
		encoded := encode(&log)
		os.Stdout.Write(encoded)
		return nil
	})

	normalizeCommand := app.Command(
		"normalize",
		"Normalize the given Dhall expression.",
	).Action(func(*kingpin.ParseContext) error {
		normalized := normalize(&log)
		fmt.Println(render(normalized))
		return nil
	}).Default()
	normalizeCommand.Flag(
		"output",
		fmt.Sprintf("Render the expression in different formats %q", outputs),
	).Default("dhall").EnumVar(&output, outputs...)

	app.Command(
		"type",
		"Type check the given Dhall expression.",
	).Action(func(*kingpin.ParseContext) error {
		typeChecked := typeCheck(&log)
		fmt.Println(render(typeChecked))
		return nil
	})

	parseCommand := app.Command(
		"parse",
		"Parse the given Dhall expression.",
	).Action(func(*kingpin.ParseContext) error {
		expression := parse(&log)
		fmt.Println(render(expression))
		return nil
	})
	parseCommand.Flag(
		"output",
		fmt.Sprintf("Render the expression in different formats %q", outputs),
	).Default("dhall").EnumVar(&output, outputs...)

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

func normalize(log *logrus.FieldLogger) dhall.Expression {
	expression := parse(log)

	(*log).Infof("Normalizing expression")
	normalized := dhall.Normalize(expression)
	*log = (*log).WithFields(logrus.Fields{"normalized-expression": normalized})
	(*log).Debug("Successfully normalized expression")

	return normalized
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

func renderDhall(log *logrus.FieldLogger) func(dhall.Expression) string {
	return func(e dhall.Expression) string {
		*log = (*log).WithFields(logrus.Fields{"expression": e})

		(*log).Info("Rendering expression to Dhall")
		rendered := dhall.Render(e)
		*log = (*log).WithFields(logrus.Fields{"output": rendered})
		(*log).Debug("Successfully rendered expression to Dhall")

		return rendered
	}
}

func renderJSON(log *logrus.FieldLogger) func(dhall.Expression) string {
	return func(e dhall.Expression) string {
		*log = (*log).WithFields(logrus.Fields{"expression": e})

		(*log).Info("Attempting to render to JSON")
		rendered, err := dhall.RenderJSON(e)
		if err != nil {
			(*log).WithFields(logrus.Fields{"err": err}).Fatal(
				"Cannot render expression to JSON",
			)
		}
		*log = (*log).WithFields(logrus.Fields{"output": rendered})
		(*log).Debug("Successfully rendered expression to JSON")

		return rendered
	}
}

func renderYAML(log *logrus.FieldLogger) func(dhall.Expression) string {
	return func(e dhall.Expression) string {
		*log = (*log).WithFields(logrus.Fields{"expression": e})

		(*log).Info("Attempting to render to YAML")
		rendered, err := dhall.RenderYAML(e)
		if err != nil {
			(*log).WithFields(logrus.Fields{"err": err}).Fatal(
				"Cannot render expression to YAML",
			)
		}
		*log = (*log).WithFields(logrus.Fields{"output": rendered})
		(*log).Debug("Successfully rendered expression to YAML")

		return rendered
	}
}

func typeCheck(log *logrus.FieldLogger) dhall.Expression {
	expression := parse(log)

	(*log).Infof("Type checking expression")
	reduced, err := dhall.Reduce(expression)
	if err != nil {
		(*log).WithFields(logrus.Fields{"err": err}).Fatal(
			"Could not type check expression",
		)
	}
	*log = (*log).WithFields(logrus.Fields{"type": reduced})
	(*log).Debug("Successfully type checked expression")

	return reduced
}
