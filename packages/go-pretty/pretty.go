package pretty

import (
	"github.com/sirupsen/logrus"
	logrusTest "github.com/sirupsen/logrus/hooks/test"
)

// Document represents the structure of a pretty printed document.
type Document interface {
	be(*logrus.FieldLogger, int, int, int, []indented) underlying
	flatten() Document
}

// Append represents one Document appended to another.
// There must be at least two documents to append together.
// If multiple documents are given, they will be left-associated.
// E.g. `Append(w, x, y, z) == Append(w, Append(x, Append(y, z)))`.
func Append(left, right Document, rest ...Document) Document {
	return Fold(
		func(left, right Document) Document {
			return &documentAPPEND{Left: left, Right: right}
		},
		append([]Document{left, right}, rest...)...,
	)
}

// Fold accumulates documents with the given operation combining each pair.
// If multiple documents are given, they will be left-associated.
// E.g. `Fold(w, x, y, z) == Fold(w, Fold(x, Fold(y, z)))`.
func Fold(f func(Document, Document) Document, rest ...Document) Document {
	lenRest := len(rest)
	if lenRest == 0 {
		return Nil
	}
	if lenRest == 1 {
		return rest[0]
	}
	lastIndex := lenRest - 1
	doc := rest[lastIndex]
	for i := lastIndex - 1; i >= 0; i-- {
		doc = f(rest[i], doc)
	}
	return doc
}

// Group will attempt to flatten a document if it fits within the width.
func Group(x Document) Document {
	return &documentUNION{Left: x.flatten(), Right: x}
}

// Line represents an newline Document.
var Line = &documentLINE{}

// Nest represents an indented Document.
func Nest(value int, document Document) Document {
	return &documentNEST{Document: document, Value: value}
}

// Nil represents an empty Document.
var Nil = &documentNIL{}

// Render renders a Document.
func Render(w int, x Document) string {
	var log logrus.FieldLogger
	log, _ = logrusTest.NewNullLogger()

	return RenderWithLog(&log, w, x)
}

// RenderWithLog renders a Document and also logs information.
func RenderWithLog(log *logrus.FieldLogger, w int, x Document) string {
	return best(log, w, 0, x).layout()
}

// Spread represents one Document appended to another with a space between.
// There must be at least two documents to spread.
// If multiple documents are given, they will be left-associated.
// E.g. `Spread(w, x, y, z) == Spread(w, Spread(x, Spread(y, z)))`.
func Spread(left, right Document, rest ...Document) Document {
	return Fold(
		func(left, right Document) Document {
			return &documentAPPEND{
				Left:  left,
				Right: &documentAPPEND{Left: Text(" "), Right: right},
			}
		},
		append([]Document{left, right}, rest...)...,
	)
}

// Stack represents one Document appended to another with a line between.
// There must be at least two documents to stack.
// If multiple documents are given, they will be left-associated.
// E.g. `Stack(w, x, y, z) == Stack(w, Stack(x, Stack(y, z)))`.
func Stack(left, right Document, rest ...Document) Document {
	return Fold(
		func(left, right Document) Document {
			return &documentAPPEND{
				Left:  left,
				Right: &documentAPPEND{Left: Line, Right: right},
			}
		},
		append([]Document{left, right}, rest...)...,
	)
}

// Text represents a string Document.
func Text(value string) Document { return &documentTEXT{Value: value} }
