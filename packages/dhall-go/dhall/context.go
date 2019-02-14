package dhall

var emptyContext = Context{value: []annotatedExpression{}}

type annotatedExpression struct {
	variable   string
	annotation Expression
}

// Context represents a Dhall Context.
type Context struct {
	value []annotatedExpression
}

func (c *Context) shift(d int, x string, m int) Context {
	if len(c.value) == 0 {
		return *c
	}
	oldHead, tail := c.value[0], c.value[1:]
	Γ0 := &Context{value: tail}
	Γ1 := Γ0.shift(d, x, m)
	T1 := oldHead.annotation.shift(d, x, m)
	newHead := annotatedExpression{variable: oldHead.variable, annotation: T1}

	return Context{value: append([]annotatedExpression{newHead}, Γ1.value...)}
}
