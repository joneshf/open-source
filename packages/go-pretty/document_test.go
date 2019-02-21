package pretty

import (
	"reflect"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
	testifyAssert "github.com/stretchr/testify/assert"
	testifyRequire "github.com/stretchr/testify/require"
)

var genDocument = sized(genDocumentSized)

var genLINE = gen.Const(&documentLINE{}).Map(func(d *documentLINE) Document {
	return d
})

var genNIL = gen.Const(&documentNIL{}).Map(func(d *documentNIL) Document {
	return d
})

var genTEXT = gen.StructPtr(
	reflect.TypeOf(&documentTEXT{}),
	map[string]gopter.Gen{
		"Value": gen.AlphaString(),
	},
).Map(func(d *documentTEXT) Document { return d })

func genAPPEND(size int) gopter.Gen {
	return gen.StructPtr(
		reflect.TypeOf(&documentAPPEND{}),
		map[string]gopter.Gen{
			"Left":  genDocumentSized(size / 2),
			"Right": genDocumentSized(size / 2),
		},
	).Map(func(d *documentAPPEND) Document { return d })

}

func genDocumentSized(size int) gopter.Gen {
	if size < 1 {
		return gen.OneGenOf(genLINE, genNIL, genTEXT)
	}
	return gen.OneGenOf(genAPPEND(size), genNEST(size), genUNION(size))
}

func genNEST(size int) gopter.Gen {
	return gen.StructPtr(
		reflect.TypeOf(&documentNEST{}),
		map[string]gopter.Gen{
			"Document": genDocumentSized(size / 2),
			"Value":    gen.IntRange(0, 100),
		},
	).Map(func(d *documentNEST) Document { return d })

}

func genUNION(size int) gopter.Gen {
	return gen.StructPtr(
		reflect.TypeOf(&documentUNION{}),
		map[string]gopter.Gen{
			"Left":  genDocumentSized(size / 2),
			"Right": genDocumentSized(size / 2),
		},
	).Map(func(d *documentUNION) Document { return d })

}

func sized(f func(int) gopter.Gen) gopter.Gen {
	return func(params *gopter.GenParameters) *gopter.GenResult {
		return f(params.Rng.Intn(3))(params)
	}
}

func TestDocumentAPPEND(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)

	properties.Property("documentAPPEND is associative", prop.ForAll(
		func(x, y, z Document) bool {
			left := &documentAPPEND{
				Left:  x,
				Right: &documentAPPEND{Left: y, Right: z},
			}
			right := &documentAPPEND{
				Left:  &documentAPPEND{Left: x, Right: y},
				Right: z,
			}
			leftRendered := Render(30, left)
			rightRendered := Render(30, right)
			return assert.Equal(leftRendered, rightRendered)
		},
		genDocument,
		genDocument,
		genDocument,
	))

	properties.Property("documentAPPEND has Nil as identity", prop.ForAll(
		func(x Document) bool {
			left := &documentAPPEND{Left: x, Right: &documentNIL{}}
			right := &documentAPPEND{Left: &documentNIL{}, Right: x}
			leftRendered := Render(30, left)
			rightRendered := Render(30, right)
			return assert.Equal(leftRendered, rightRendered)
		},
		genDocument,
	))

	properties.TestingRun(t)
}

func TestDocumentTEXT(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)
	require := testifyRequire.New(t)

	properties.Property("documentTEXT is a semigroup homomorphism", prop.ForAll(
		func(x, y string) bool {
			left := &documentTEXT{Value: x + y}
			right := &documentAPPEND{
				Left:  &documentTEXT{Value: x},
				Right: &documentTEXT{Value: y},
			}
			leftRendered := Render(30, left)
			rightRendered := Render(30, right)
			return assert.Equal(leftRendered, rightRendered)
		},
		gen.AnyString(),
		gen.AnyString(),
	))

	t.Run("documentTEXT is a monoid homomorphism", func(t *testing.T) {
		left := &documentTEXT{Value: ""}
		right := &documentNIL{}
		leftRendered := Render(30, left)
		rightRendered := Render(30, right)
		require.Equal(leftRendered, rightRendered)
	})

	properties.TestingRun(t)
}

func TestDocumentNEST(t *testing.T) {
	assert := testifyAssert.New(t)
	properties := gopter.NewProperties(nil)

	properties.Property("documentNEST is a semigroup homomorphism", prop.ForAll(
		func(x, y int, z Document) bool {
			left := &documentNEST{Document: z, Value: x + y}
			right := &documentNEST{
				Document: &documentNEST{Document: z, Value: y},
				Value:    x,
			}
			leftRendered := Render(30, left)
			rightRendered := Render(30, right)
			return assert.Equal(leftRendered, rightRendered)
		},
		gen.IntRange(0, 100),
		gen.IntRange(0, 100),
		genDocument,
	))

	properties.Property("documentNEST is a monoid homomorphism", prop.ForAll(
		func(x Document) bool {
			left := &documentNEST{Document: x, Value: 0}
			right := x
			leftRendered := Render(30, left)
			rightRendered := Render(30, right)
			return assert.Equal(leftRendered, rightRendered)
		},
		genDocument,
	))

	properties.Property("documentNEST distributes over documentAPPEND", prop.ForAll(
		func(x int, y, z Document) bool {
			left := &documentNEST{
				Document: &documentAPPEND{Left: y, Right: z},
				Value:    x,
			}
			right := &documentAPPEND{
				Left:  &documentNEST{Document: y, Value: x},
				Right: &documentNEST{Document: z, Value: x},
			}
			leftRendered := Render(30, left)
			rightRendered := Render(30, right)
			return assert.Equal(leftRendered, rightRendered)
		},
		gen.IntRange(0, 100),
		genDocument,
		genDocument,
	))

	properties.Property("documentNEST is annihilated by documentNIL", prop.ForAll(
		func(x int) bool {
			left := &documentNEST{Document: &documentNIL{}, Value: x}
			right := &documentNIL{}
			leftRendered := Render(30, left)
			rightRendered := Render(30, right)
			return assert.Equal(leftRendered, rightRendered)
		},
		gen.IntRange(0, 100),
	))

	properties.Property("documentNEST is annihilated by documentTEXT", prop.ForAll(
		func(x int, y string) bool {
			left := &documentNEST{Document: &documentTEXT{Value: y}, Value: x}
			right := &documentTEXT{Value: y}
			leftRendered := Render(30, left)
			rightRendered := Render(30, right)
			return assert.Equal(leftRendered, rightRendered)
		},
		gen.IntRange(0, 100),
		gen.AnyString(),
	))

	properties.TestingRun(t)
}
