package pretty

import (
	"strings"
)

// underlying is the representation that is as close to a string as possible.
// We have a more effcient reprsentation that is used for constructing the AST.
// We then convert to this normalized form for easy layout to a string.
type underlying interface {
	fits(int) bool
	layout() string
}

// underlyingLine indents the underlying.
type underlyingLine struct {
	Underlying underlying
	Value      int
}

func (l *underlyingLine) fits(w int) bool { return w >= 0 }

func (l *underlyingLine) layout() string {
	return "\n" + strings.Repeat(" ", l.Value) + l.Underlying.layout()
}

// underlyingNil is an empty underlying.
type underlyingNil struct{}

func (*underlyingNil) fits(w int) bool { return w >= 0 }

func (*underlyingNil) layout() string { return "" }

// underlyingText embeds a string in a underlying.
type underlyingText struct {
	Underlying underlying
	Value      string
}

func (t *underlyingText) fits(w int) bool {
	return w >= 0 && t.Underlying.fits(w-len(t.Value))
}

func (t *underlyingText) layout() string {
	return t.Value + t.Underlying.layout()
}
