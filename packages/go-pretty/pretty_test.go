package pretty

import (
	"fmt"
)

var showBracket, showTrees func([]tree) Document
var showTree func(tree) Document
var example = tree{
	string: "aaa",
	forest: []tree{
		tree{
			string: "bbbbb",
			forest: []tree{
				tree{string: "ccc", forest: []tree{}},
				tree{string: "dd", forest: []tree{}},
			},
		},
		tree{string: "eee", forest: []tree{}},
		tree{
			string: "ffff",
			forest: []tree{
				tree{string: "gg", forest: []tree{}},
				tree{string: "hhh", forest: []tree{}},
				tree{string: "ii", forest: []tree{}},
			},
		},
	},
}

type tree struct {
	string string
	forest []tree
}

func ExampleTreeFirst() {
	showBracket = func(ts []tree) Document {
		if len(ts) == 0 {
			return Nil
		}
		return Append(Text("["), Nest(1, showTrees(ts)), Text("]"))
	}

	showTree = func(x tree) Document {
		return Append(Text(x.string), Nest(len(x.string), showBracket(x.forest)))
	}

	showTrees = func(ts []tree) Document {
		if len(ts) == 1 {
			return showTree(ts[0])
		}
		return Append(showTree(ts[0]), Text(","), Line, showTrees(ts[1:]))
	}

	fmt.Println(Render(30, showTree(example)))
	// Output:
	// aaa[bbbbb[ccc,
	//           dd],
	//     eee,
	//     ffff[gg,
	//          hhh,
	//          ii]]
}

func ExampleTreeSecond() {
	showBracket = func(ts []tree) Document {
		if len(ts) == 0 {
			return Nil
		}
		return Append(
			Text("["),
			Nest(2, Append(Line, showTrees(ts))),
			Line,
			Text("]"),
		)
	}

	showTree = func(x tree) Document {
		return Append(Text(x.string), showBracket(x.forest))
	}

	showTrees = func(ts []tree) Document {
		if len(ts) == 1 {
			return showTree(ts[0])
		}
		return Append(showTree(ts[0]), Text(","), Line, showTrees(ts[1:]))
	}

	fmt.Println(Render(30, showTree(example)))
	// Output:
	// aaa[
	//   bbbbb[
	//     ccc,
	//     dd
	//   ],
	//   eee,
	//   ffff[
	//     gg,
	//     hhh,
	//     ii
	//   ]
	// ]
}

func ExampleTreeThird() {
	showBracket = func(ts []tree) Document {
		if len(ts) == 0 {
			return Nil
		}
		return Append(Text("["), Nest(1, showTrees(ts)), Text("]"))
	}

	showTree = func(x tree) Document {
		return Group(
			Append(Text(x.string), Nest(len(x.string), showBracket(x.forest))),
		)
	}

	showTrees = func(ts []tree) Document {
		if len(ts) == 1 {
			return showTree(ts[0])
		}
		return Append(showTree(ts[0]), Text(","), Line, showTrees(ts[1:]))
	}

	fmt.Println(Render(30, showTree(example)))
	// Output:
	// aaa[bbbbb[ccc, dd],
	//     eee,
	//     ffff[gg, hhh, ii]]
}
