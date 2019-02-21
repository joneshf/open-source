package pretty

import (
	"fmt"

	"github.com/sirupsen/logrus"
)

type indented struct {
	indent int
	value  Document
}

func best(log *logrus.FieldLogger, w, k int, x Document) underlying {
	*log = (*log).WithFields(logrus.Fields{"w": w, "k": k})
	*log = (*log).WithField("x", fmt.Sprintf("%#v", x))
	(*log).Info("best")

	acc := []indented{indented{indent: 0, value: x}}
	return be(log, w, k, acc)
}

func be(log *logrus.FieldLogger, w, k int, acc []indented) underlying {
	*log = (*log).WithFields(logrus.Fields{"w": w, "k": k})
	*log = (*log).WithField("acc", fmt.Sprintf("%#v", acc))
	(*log).Info("be")

	if len(acc) == 0 {
		return &underlyingNil{}
	}
	x, xs := acc[0], acc[1:]
	return x.value.be(log, w, k, x.indent, xs)
}

// documentNIL represents an empty Document.
type documentNIL struct{}

func (d *documentNIL) be(log *logrus.FieldLogger, w, k, i int, z []indented) underlying {
	*log = (*log).WithFields(logrus.Fields{"w": w, "k": k, "i": i, "z": z})
	*log = (*log).WithField("document", fmt.Sprintf("%#v", d))
	(*log).Info("documentNIL.be")

	return be(log, w, k, z)
}

func (*documentNIL) flatten() Document { return &documentNIL{} }

// documentAPPEND represents one Document appended to another.
type documentAPPEND struct {
	Left  Document
	Right Document
}

func (d *documentAPPEND) be(log *logrus.FieldLogger, w, k, i int, z []indented) underlying {
	*log = (*log).WithFields(logrus.Fields{"w": w, "k": k, "i": i, "z": z})
	*log = (*log).WithField("document", fmt.Sprintf("%#v", d))
	(*log).Info("documentAPPEND.be")

	left := indented{indent: i, value: d.Left}
	right := indented{indent: i, value: d.Right}
	return be(log, w, k, append([]indented{left, right}, z...))
}

func (d *documentAPPEND) flatten() Document {
	return &documentAPPEND{Left: d.Left.flatten(), Right: d.Right.flatten()}
}

// documentNEST represents an Document indented.
type documentNEST struct {
	Document Document
	Value    int
}

func (d *documentNEST) be(log *logrus.FieldLogger, w, k, i int, z []indented) underlying {
	*log = (*log).WithFields(logrus.Fields{"w": w, "k": k, "i": i, "z": z})
	*log = (*log).WithField("document", fmt.Sprintf("%#v", d))
	(*log).Info("documentNEST.be")

	x := indented{indent: i + d.Value, value: d.Document}
	return be(log, w, k, append([]indented{x}, z...))
}

func (d *documentNEST) flatten() Document {
	return &documentNEST{Value: d.Value, Document: d.Document.flatten()}
}

// documentTEXT represents a string Document.
type documentTEXT struct {
	Value string
}

func (d *documentTEXT) be(log *logrus.FieldLogger, w, k, i int, z []indented) underlying {
	*log = (*log).WithFields(logrus.Fields{"w": w, "k": k, "i": i, "z": z})
	*log = (*log).WithField("document", fmt.Sprintf("%#v", d))
	(*log).Info("documentTEXT.be")

	return &underlyingText{
		Underlying: be(log, w, k+len(d.Value), z),
		Value:      d.Value,
	}
}

func (d *documentTEXT) flatten() Document { return d }

// documentLINE represents a newline Document.
type documentLINE struct{}

func (d *documentLINE) be(log *logrus.FieldLogger, w, k, i int, z []indented) underlying {
	*log = (*log).WithFields(logrus.Fields{"w": w, "k": k, "i": i, "z": z})
	*log = (*log).WithField("document", fmt.Sprintf("%#v", d))
	(*log).Info("documentLINE.be")

	return &underlyingLine{Underlying: be(log, w, i, z), Value: i}
}

func (d *documentLINE) flatten() Document { return &documentTEXT{Value: " "} }

// documentUNION represents one Document unioned to another.
type documentUNION struct {
	Left  Document
	Right Document
}

func (d *documentUNION) be(log *logrus.FieldLogger, w, k, i int, z []indented) underlying {
	*log = (*log).WithFields(logrus.Fields{"w": w, "k": k, "i": i, "z": z})
	*log = (*log).WithField("document", fmt.Sprintf("%#v", d))
	(*log).Info("documentLINE.be")

	left := indented{indent: i, value: d.Left}
	*log = (*log).WithField("documentUNION.left", left)

	x := be(log, w, k, append([]indented{left}, z...))
	if x.fits(w - k) {
		(*log).Debug("It fits!")
		return x
	}
	(*log).Debug("It doesn't fit.")

	right := indented{indent: i, value: d.Right}
	*log = (*log).WithField("documentUNION.right", right)

	return be(log, w, k, append([]indented{right}, z...))
}

func (d *documentUNION) flatten() Document { return d.Left.flatten() }
