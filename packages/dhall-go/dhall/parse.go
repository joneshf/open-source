package dhall

import (
	"fmt"

	"github.com/opsidian/parsley/ast"
	"github.com/opsidian/parsley/ast/interpreter"
	"github.com/opsidian/parsley/combinator"
	"github.com/opsidian/parsley/parser"
	"github.com/opsidian/parsley/parsley"
	"github.com/opsidian/parsley/text"
	"github.com/opsidian/parsley/text/terminal"
	"github.com/sirupsen/logrus"
)

// Parse attempts to parse a string into a Dhall Expression.
func Parse(log *logrus.FieldLogger, input []byte) (Expression, error) {
	f := text.NewFile("<stdin>", input)
	fs := parsley.NewFileSet(f)
	r := text.NewReader(f)
	ctx := parsley.NewContext(fs, r)
	ctx.SetUserContext(log)
	parser := combinator.Memoize(completeExpression())
	expr, err := parsley.Evaluate(ctx, parser)
	if err != nil {
		return nil, err
	}
	return expr.(Expression), nil
}

func alpha() parsley.Parser {
	return combinator.Choice(
		terminal.Rune(0x41),
		terminal.Rune(0x42),
		terminal.Rune(0x43),
		terminal.Rune(0x44),
		terminal.Rune(0x45),
		terminal.Rune(0x46),
		terminal.Rune(0x47),
		terminal.Rune(0x48),
		terminal.Rune(0x49),
		terminal.Rune(0x4A),
		terminal.Rune(0x4B),
		terminal.Rune(0x4C),
		terminal.Rune(0x4D),
		terminal.Rune(0x4E),
		terminal.Rune(0x4F),
		terminal.Rune(0x50),
		terminal.Rune(0x51),
		terminal.Rune(0x52),
		terminal.Rune(0x53),
		terminal.Rune(0x54),
		terminal.Rune(0x55),
		terminal.Rune(0x56),
		terminal.Rune(0x57),
		terminal.Rune(0x58),
		terminal.Rune(0x59),
		terminal.Rune(0x5A),
		terminal.Rune(0x61),
		terminal.Rune(0x62),
		terminal.Rune(0x63),
		terminal.Rune(0x64),
		terminal.Rune(0x65),
		terminal.Rune(0x66),
		terminal.Rune(0x67),
		terminal.Rune(0x68),
		terminal.Rune(0x69),
		terminal.Rune(0x6A),
		terminal.Rune(0x6B),
		terminal.Rune(0x6C),
		terminal.Rune(0x6D),
		terminal.Rune(0x6E),
		terminal.Rune(0x6F),
		terminal.Rune(0x70),
		terminal.Rune(0x71),
		terminal.Rune(0x72),
		terminal.Rune(0x73),
		terminal.Rune(0x74),
		terminal.Rune(0x75),
		terminal.Rune(0x76),
		terminal.Rune(0x77),
		terminal.Rune(0x78),
		terminal.Rune(0x79),
		terminal.Rune(0x7A),
	)
}

func and() parsley.Parser {
	return combinator.SeqOf(terminal.Op("&&"), whitespace())
}

func andExpression() parsley.Parser {
	return combinator.SeqOf(
		combineExpression(),
		combinator.Many(combinator.SeqOf(and(), combineExpression()).Bind(
			interpreter.Select(1),
		)).Bind(interpretManyExpressions("and-expression")),
	).Bind(interpretBinaryExpression(
		"and-expression",
		"combine-expression",
		constExpression,
	))
}

func applicationExpression() parsley.Parser {
	interpretApplicationExpression := ast.InterpreterFunc(func(ctx interface{}, node parsley.NonTerminalNode) (interface{}, parsley.Error) {
		log := *ctx.(*logrus.FieldLogger)
		log = log.WithFields(logrus.Fields{"interpreter": "application-expression"})
		log.Info("Interpreting application-expression")
		children := node.Children()
		log = log.WithFields(logrus.Fields{"children": children})

		if len(children) == 3 {
			e1, errE1 := children[0].Value(ctx)
			if errE1 != nil {
				log.WithFields(logrus.Fields{"err": errE1}).Error(
					"Error parsing constructors/Some",
				)
				return nil, errE1
			}
			log = log.WithFields(logrus.Fields{"constructors/Some": e1})
			log.Debug("Successfully parsed constructors/Some")
			e2, errE2 := children[1].Value(ctx)
			if errE2 != nil {
				log.WithFields(logrus.Fields{"err": errE2}).Error(
					"Error parsing import-expression",
				)
				return nil, errE2
			}
			log = log.WithFields(logrus.Fields{"import-expression": e2})
			log.Debug("Successfully parsed import-expression")
			e3, errE3 := children[2].Value(ctx)
			if errE3 != nil {
				log.WithFields(logrus.Fields{"err": errE3}).Error(
					"Error parsing optional-import-expressions",
				)
				return nil, errE3
			}
			log = log.WithFields(logrus.Fields{"many-import-expressions": e3})
			log.Debug("Successfully parsed many-import-expressions")
			importExpressions := e3.([]Expression)

			if e1 == nil && len(importExpressions) == 0 {
				log.WithFields(logrus.Fields{
					"import-expressions": importExpressions,
				}).Debug("No import-expressions")
				return e2, nil
			}
		}

		value, err := node.Value(ctx)
		return nil, parsley.WrapError(err, "Unhandled case: %#v", value)
	})

	return combinator.SeqOf(
		combinator.Optional(combinator.Choice(constructors(), some())),
		importExpression(),
		combinator.Many(
			combinator.SeqOf(whitespaceChunk(), importExpression()).Bind(
				interpreter.Select(1),
			),
		).Bind(interpretManyExpressions("import-expression")),
	).Bind(interpretApplicationExpression)
}

func boolRaw() parsley.Parser {
	return terminal.Word(
		string([]rune{0x42, 0x6f, 0x6f, 0x6c}),
		&BoolType{},
		"reserved",
	)
}

func closeBrace() parsley.Parser {
	return combinator.SeqOf(terminal.Op("}"), whitespace())
}

func colon() parsley.Parser {
	return combinator.SeqOf(terminal.Op(":"), nonemptyWhitespace())
}

func combine() parsley.Parser {
	return combinator.SeqOf(
		combinator.Choice(terminal.Rune(0x2227), terminal.Op("/\\")),
		whitespace(),
	)
}

func combineExpression() parsley.Parser {
	return combinator.SeqOf(
		preferExpression(),
		combinator.Many(combinator.SeqOf(combine(), preferExpression()).Bind(
			interpreter.Select(1),
		)).Bind(interpretManyExpressions("combine-expression")),
	).Bind(interpretBinaryExpression(
		"combine-expression",
		"prefer-expression",
		constExpression,
	))
}

func combineTypes() parsley.Parser {
	return combinator.SeqOf(
		combinator.Choice(terminal.Rune(0x2A53), terminal.Op("//\\")),
		whitespace(),
	)
}

func combineTypesExpression() parsley.Parser {
	return combinator.SeqOf(
		timesExpression(),
		combinator.Many(combinator.SeqOf(combineTypes(), timesExpression()).Bind(
			interpreter.Select(1),
		)).Bind(interpretManyExpressions("combine-types-expression")),
	).Bind(interpretBinaryExpression(
		"combine-types-expression",
		"times-expression",
		constExpression,
	))
}

func comma() parsley.Parser {
	return combinator.SeqOf(terminal.Op(","), whitespace())
}

func completeExpression() parsley.Parser {
	var annotatedExpressionParser parser.Func

	expression := combinator.Choice(
		// TODO: combinator.SeqOf(
		// 	lambda(),
		// 	openParens(),
		// 	label(),
		// 	colon(),
		// 	expression(),
		// 	closeParens(),
		// 	arrow(),
		// 	expression(),
		// ),
		// TODO: combinator.SeqOf(
		// 	if_(),
		// 	expression(),
		// 	then(),
		// 	expression(),
		// 	else_(),
		// 	expression(),
		// ),
		// TODO: combinator.SeqOf(
		// 	combinator.Many1(
		// 		let(),
		// 		label(),
		// 		combinator.Optional(combinator.SeqOf(colon(), expression())),
		// 		equal(),
		// 		expression(),
		// 	),
		// 	in(),
		// 	expression(),
		// ),
		// TODO: combinator.SeqOf(
		// 	forall(),
		// 	openParens(),
		// 	label(),
		// 	colon(),
		// 	expression(),
		// 	closeParens(),
		// 	arrow(),
		// 	expression(),
		// ),
		// TODO: combinator.SeqOf(operatorExpression(), arrow(), expression()),
		&annotatedExpressionParser,
	)

	interpret := ast.InterpreterFunc(func(ctx interface{}, node parsley.NonTerminalNode) (interface{}, parsley.Error) {
		log := *ctx.(*logrus.FieldLogger)
		log = log.WithFields(logrus.Fields{"interpreter": "annotated-expression"})
		log.Info("Interpreting annotated-expression")
		children := node.Children()
		log = log.WithFields(logrus.Fields{"children": children})
		if len(children) == 2 {
			e, errE := children[0].Value(ctx)
			if errE != nil {
				log.WithFields(logrus.Fields{"err": errE}).Error(
					"Error parsing operator-expression",
				)
				return nil, errE
			}
			log = log.WithFields(logrus.Fields{"operator-expression": e})
			log.Debug("Successfully parsed operator-expression")
			t, errT := children[1].Value(ctx)
			if errT != nil {
				log.WithFields(logrus.Fields{"err": errT}).Error(
					"Error parsing annotation",
				)
				return nil, errT
			}
			log = log.WithFields(logrus.Fields{"annotation": t})
			log.Debug("Successfully parsed annotation")
			return e.(Expression), nil
		}
		value, err := node.Value(ctx)
		return nil, parsley.WrapError(err, "Unhandled case: %#v", value)
	})

	annotatedExpressionParser = combinator.Choice(
		// TODO: combinator.SeqOf(
		// 	merge(),
		// 	importExpression(),
		// 	importExpression(),
		// 	combinator.Optional(
		// 		combinator.SeqOf(colon(), applicationExpression()),
		// 	),
		// ),
		// TODO: combinator.SeqOf(
		// 	openBracket(),
		// 	combinator.Choice(emptyCollection(), nonEmptyOptional()),
		// ),
		combinator.SeqOf(
			operatorExpression(),
			combinator.Optional(combinator.SeqOf(colon(), expression).Bind(
				interpreter.Select(1),
			)),
		).Bind(interpret),
	)

	return combinator.SeqOf(whitespace(), expression).Bind(interpreter.Select(1))
}

func constExpression(e Expression, _ Expression) Expression { return e }

func constructors() parsley.Parser {
	return combinator.SeqOf(constructorsRaw(), nonemptyWhitespace())
}

func constructorsRaw() parsley.Parser {
	return terminal.Op(string([]rune{
		0x63, 0x6f, 0x6e, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x6f, 0x72, 0x73,
	}))
}

func digit() parsley.Parser {
	return combinator.Choice(
		terminal.Rune(0x30),
		terminal.Rune(0x31),
		terminal.Rune(0x32),
		terminal.Rune(0x33),
		terminal.Rune(0x34),
		terminal.Rune(0x35),
		terminal.Rune(0x36),
		terminal.Rune(0x37),
		terminal.Rune(0x38),
		terminal.Rune(0x39),
	)
}

func dot() parsley.Parser {
	return combinator.SeqOf(terminal.Op("."), whitespace())
}

func doubleEqual() parsley.Parser {
	return combinator.SeqOf(terminal.Op("=="), whitespace())
}

func endOfLine() parsley.Parser {
	return combinator.Choice(
		terminal.Rune(0x0A),
		terminal.Op(string([]rune{0x0D, 0x0A})),
	)
}

func equalExpression() parsley.Parser {
	return combinator.SeqOf(
		notEqualExpression(),
		combinator.Many(
			combinator.SeqOf(doubleEqual(), notEqualExpression()).Bind(
				interpreter.Select(1),
			),
		).Bind(interpretManyExpressions("double-equal-expression")),
	).Bind(interpretBinaryExpression(
		"equal-expression",
		"not-equal-expression",
		func(left, right Expression) Expression {
			return &BoolEqual{Left: left, Right: right}
		},
	))
}

func falseRaw() parsley.Parser {
	return terminal.Word(
		string([]rune{0x46, 0x61, 0x6c, 0x73, 0x65}),
		&Bool{Value: false},
		"reserved",
	)
}

func importAlt() parsley.Parser {
	return combinator.SeqOf(terminal.Op("?"), nonemptyWhitespace())
}

func importAltExpression() parsley.Parser {
	return combinator.SeqOf(
		orExpression(),
		combinator.Many(combinator.SeqOf(importAlt(), orExpression()).Bind(
			interpreter.Select(1),
		)).Bind(interpretManyExpressions("import-alt-expression")),
	).Bind(interpretBinaryExpression(
		"import-alt-expression",
		"or-expression",
		constExpression,
	))
}

func importExpression() parsley.Parser {
	return combinator.Choice(
		// TODO: importParser(),
		selectorExpression(),
	)
}

func interpretBinaryExpression(operatorName string, operandName string, operator func(Expression, Expression) Expression) parsley.Interpreter {
	operatorNames := fmt.Sprintf("%ss", operatorName)
	manyOperatorNames := fmt.Sprintf("many-%s", operatorNames)
	return interpretFollowedByExpression(
		operatorName,
		operandName,
		manyOperatorNames,
		operator,
	)
}

func interpretFollowedByExpression(
	operatorName string,
	firstName string,
	manyNames string,
	operator func(Expression, Expression) Expression,
) parsley.Interpreter {
	operatorNames := fmt.Sprintf("%ss", operatorName)

	return ast.InterpreterFunc(func(ctx interface{}, node parsley.NonTerminalNode) (interface{}, parsley.Error) {
		log := *ctx.(*logrus.FieldLogger)
		log = log.WithFields(logrus.Fields{"interpreter": operatorName})
		log.Infof("Interpreting %s", operatorName)
		children := node.Children()
		log = log.WithFields(logrus.Fields{"children": children})

		if len(children) == 2 {
			e, err := children[0].Value(ctx)
			if err != nil {
				log.WithFields(logrus.Fields{"err": err}).Errorf(
					"Error parsing %s",
					firstName,
				)
				return nil, err
			}
			log = log.WithFields(logrus.Fields{firstName: e})
			log.Debugf("Successfully parsed %s", firstName)
			expression := e.(Expression)
			e2, err := children[1].Value(ctx)
			if err != nil {
				log.WithFields(logrus.Fields{"err": err}).Errorf(
					"Error parsing %s",
					manyNames,
				)
				return nil, err
			}
			log = log.WithFields(logrus.Fields{manyNames: e2})
			log.Debugf("Successfully parsed %s", manyNames)
			expressions := e2.([]Expression)

			log = log.WithFields(logrus.Fields{operatorNames: expressions})
			log.Debugf("Constructing the final expression")
			for _, newExpression := range expressions {
				log.WithFields(logrus.Fields{
					"new-expression": expression,
				}).Debugf("Appending new-expression")
				expression = operator(expression, newExpression)
			}
			log.Debugf("Constructed the final expression")
			return expression, nil
		}

		value, err := node.Value(ctx)
		return nil, parsley.WrapError(err, "Unhandled case: %#v", value)
	})
}

func interpretManyExpressions(expressionName string) parsley.Interpreter {
	interpreterName := fmt.Sprintf("many-%ss", expressionName)
	return ast.InterpreterFunc(func(ctx interface{}, node parsley.NonTerminalNode) (interface{}, parsley.Error) {
		log := *ctx.(*logrus.FieldLogger)
		log = log.WithFields(logrus.Fields{"interpreter": interpreterName})
		log.Infof("Interpreting %s", interpreterName)
		children := node.Children()
		expressions := []Expression{}
		log = log.WithFields(logrus.Fields{
			"children":           children,
			expressionName + "s": expressions,
		})

		log.Debug("Iterating children")
		for _, child := range children {
			e, err := child.Value(ctx)
			if err != nil {
				log.WithFields(logrus.Fields{"err": err}).Errorf(
					"Error parsing %s",
					expressionName,
				)
				return nil, err
			}
			log = log.WithFields(logrus.Fields{"last-" + expressionName: e})
			log.Debugf("Successfully parsed %s", expressionName)
			expressions = append(expressions, e.(Expression))
		}

		return expressions, nil
	})
}

func kindRaw() parsley.Parser {
	return terminal.Word(
		string([]rune{0x4b, 0x69, 0x6e, 0x64}),
		&Kind{},
		"reserved",
	)
}

func label() parsley.Parser {
	return combinator.SeqOf(
		combinator.Choice(
			combinator.SeqOf(
				terminal.Op("`"),
				quotedLabel(),
				terminal.Op("`"),
			).Bind(interpreter.Select(1)),
			// TODO: simpleLabel(),
		),
		whitespace(),
	).Bind(interpreter.Select(0))
}

func labels() parsley.Parser {
	return combinator.SeqOf(
		openBrace(),
		combinator.Optional(
			combinator.SeqOf(
				label(),
				combinator.Many(combinator.SeqOf(comma(), label())),
			),
		),
		closeBrace(),
	).Bind(interpreter.Select(1))
}

func listAppend() parsley.Parser {
	return combinator.SeqOf(terminal.Op("#"), whitespace())
}

func listAppendExpression() parsley.Parser {
	return combinator.SeqOf(
		andExpression(),
		combinator.Many(combinator.SeqOf(listAppend(), andExpression()).Bind(
			interpreter.Select(1),
		)).Bind(interpretManyExpressions("list-append-expression")),
	).Bind(interpretBinaryExpression(
		"list-append-expression",
		"and-expression",
		constExpression,
	))
}

func nonemptyWhitespace() parsley.Parser {
	return combinator.Many1(whitespaceChunk())
}

func notEqual() parsley.Parser {
	return combinator.SeqOf(terminal.Op("!="), whitespace())
}

func notEqualExpression() parsley.Parser {
	return combinator.SeqOf(
		applicationExpression(),
		combinator.Many(
			combinator.SeqOf(notEqual(), applicationExpression()).Bind(
				interpreter.Select(1),
			),
		).Bind(interpretManyExpressions("not-equal-expression")),
	).Bind(interpretBinaryExpression(
		"not-equal-expression",
		"application-expression",
		constExpression,
	))
}

func openBrace() parsley.Parser {
	return combinator.SeqOf(terminal.Op("{"), whitespace())
}

func operatorExpression() parsley.Parser { return importAltExpression() }

func or() parsley.Parser {
	return combinator.SeqOf(terminal.Op("||"), whitespace())
}

func orExpression() parsley.Parser {
	return combinator.SeqOf(
		plusExpression(),
		combinator.Many(combinator.SeqOf(or(), plusExpression()).Bind(
			interpreter.Select(1),
		)).Bind(interpretManyExpressions("or-expression")),
	).Bind(interpretBinaryExpression(
		"or-expression",
		"plus-expression",
		constExpression,
	))
}

func plus() parsley.Parser {
	return combinator.SeqOf(terminal.Op("+"), nonemptyWhitespace())
}

func plusExpression() parsley.Parser {
	return combinator.SeqOf(
		textAppendExpression(),
		combinator.Many(combinator.SeqOf(plus(), textAppendExpression()).Bind(
			interpreter.Select(1),
		)).Bind(interpretManyExpressions("plus-expression")),
	).Bind(interpretBinaryExpression(
		"plus-expression",
		"text-append-expression",
		constExpression,
	))
}

func prefer() parsley.Parser {
	return combinator.SeqOf(
		combinator.Choice(terminal.Rune(0x2AFD), terminal.Op("//")),
		whitespace(),
	).Bind(interpreter.Select(0))
}

func preferExpression() parsley.Parser {
	return combinator.SeqOf(
		combineTypesExpression(),
		combinator.Many(
			combinator.SeqOf(prefer(), combineTypesExpression()).Bind(
				interpreter.Select(1),
			),
		).Bind(interpretManyExpressions("prefer-expression")),
	).Bind(interpretBinaryExpression(
		"prefer-expression",
		"combine-types-expression",
		constExpression,
	))
}

func primitiveExpression() parsley.Parser {
	return combinator.Choice(
		// TODO: doubleLiteral(),
		// TODO: naturalLiteral(),
		// TODO: integerLiteral(),
		// TODO: combinator.SeqOf(terminal.Op("-"), infinityRaw()),
		// TODO: textLiteral(),
		// TODO: combinator.SeqOf(openBrace(), recordTypeOrLiteral(), closeBrace()),
		// TODO: combinator.SeqOf(openAngle(), unionTypeOrLiteral(), closeAngle()),
		// TODO: nonEmptyListLiteral(),
		// TODO: identifierReservedNamespacedPrefix(),
		// TODO: reservedNamespaced(),
		// TODO: identifierReservedPrefix(),
		reserved(),
		// TODO: identifier(),
		// TODO: combinator.SeqOf(openParens(), expression(), closeParens()),
	)
}

func quotedLabel() parsley.Parser {
	return combinator.Many1(
		combinator.Choice(
			alpha(),
			digit(),
			terminal.Op("-"),
			terminal.Op("/"),
			terminal.Op("_"),
			terminal.Op(":"),
			terminal.Op("."),
			terminal.Op("$"),
		),
	)
}

func reserved() parsley.Parser {
	return combinator.SeqOf(reservedRaw(), whitespace()).Bind(
		interpreter.Select(0),
	)
}

func reservedRaw() parsley.Parser {
	return combinator.Choice(
		boolRaw(),
		// TODO: optionalRaw(),
		// TODO: noneRaw(),
		// TODO: naturalRaw(),
		// TODO: integerRaw(),
		// TODO: doubleRaw(),
		// TODO: textRaw(),
		// TODO: listRaw(),
		trueRaw(),
		falseRaw(),
		// TODO: naNRaw(),
		// TODO: infinityRaw(),
		typeRaw(),
		kindRaw(),
		sortRaw(),
	)
}

func selectorExpression() parsley.Parser {
	return combinator.SeqOf(
		primitiveExpression(),
		combinator.Many(
			combinator.SeqOf(dot(), combinator.Choice(label(), labels())).Bind(
				interpreter.Select(1),
			),
		).Bind(interpretManyExpressions("selector")),
	).Bind(interpretFollowedByExpression(
		"selector-expression",
		"primitive-expression",
		"selectors",
		constExpression,
	))
}

func some() parsley.Parser {
	return combinator.SeqOf(someRaw(), nonemptyWhitespace()).Bind(
		interpreter.Select(1),
	)
}

func someRaw() parsley.Parser {
	return terminal.Op(string([]rune{0x53, 0x6f, 0x6d, 0x65}))
}

func sortRaw() parsley.Parser {
	return terminal.Word(
		string([]rune{0x53, 0x6f, 0x72, 0x74}),
		&Sort{},
		"reserved",
	)
}

func tab() parsley.Parser { return terminal.Rune(0x09) }

func textAppend() parsley.Parser {
	return combinator.SeqOf(terminal.Op("++"), whitespace())
}

func textAppendExpression() parsley.Parser {
	return combinator.SeqOf(
		listAppendExpression(),
		combinator.Many(
			combinator.SeqOf(textAppend(), listAppendExpression()).Bind(
				interpreter.Select(1),
			),
		).Bind(interpretManyExpressions("text-append-expression")),
	).Bind(interpretBinaryExpression(
		"text-append-expression",
		"list-append-expression",
		constExpression,
	))
}

func times() parsley.Parser {
	return combinator.SeqOf(terminal.Op("*"), whitespace())
}

func timesExpression() parsley.Parser {
	return combinator.SeqOf(
		equalExpression(),
		combinator.Many(
			combinator.SeqOf(times(), equalExpression()).Bind(
				interpreter.Select(1),
			),
		).Bind(interpretManyExpressions("times-expression")),
	).Bind(interpretBinaryExpression(
		"times-expression",
		"equal-expression",
		constExpression,
	))
}

func trueRaw() parsley.Parser {
	return terminal.Word(
		string([]rune{0x54, 0x72, 0x75, 0x65}),
		&Bool{Value: true},
		"reserved",
	)
}

func typeRaw() parsley.Parser {
	return terminal.Word(
		string([]rune{0x54, 0x79, 0x70, 0x65}),
		&Type{},
		"reserved",
	)
}

func whitespace() parsley.Parser { return combinator.Many(whitespaceChunk()) }

func whitespaceChunk() parsley.Parser {
	return combinator.Choice(
		terminal.Op(" "),
		tab(),
		endOfLine(),
		// TODO: lineComment(),
		// TODO: blockComment(),
	)
}
