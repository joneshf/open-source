package dhall

import (
	"fmt"
)

// JSONError represents errors attempting to render JSON.
type JSONError struct {
	expression Expression
	message    string
}

func (e *JSONError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression.render())
}

// YAMLError represents errors attempting to render YAML.
type YAMLError struct {
	expression Expression
	message    string
}

func (e *YAMLError) Error() string {
	return fmt.Sprintf("%s. Expression: %#v", e.message, e.expression.render())
}

// Render renders the given expression as Dhall source.
func Render(expression Expression) string { return expression.render() }

// RenderJSON attempts to render the given expression as JSON.
func RenderJSON(expression Expression) (string, error) {
	return expression.renderJSON()
}

// RenderYAML attempts to render the given expression as YAML.
func RenderYAML(expression Expression) (string, error) {
	return expression.renderYAML()
}
