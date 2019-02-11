package dhall

import (
	"fmt"
)

// YAMLError represents errors attempting to render YAML.
type YAMLError struct {
	expression Expression
	message    string
}

func (e *YAMLError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression)
}

// Render renders the given expression as Dhall source.
func Render(expression Expression) string { return expression.render() }

// RenderYAML attempts to render the given expression as YAML.
func RenderYAML(expression Expression) (string, error) {
	return expression.renderYAML()
}
