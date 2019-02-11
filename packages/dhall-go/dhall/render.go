package dhall

// Render renders the given expression Dhall source.
func Render(expression Expression) string { return expression.render() }
