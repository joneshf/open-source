    let PureScript : Type = (./Dependency/PureScript.dhall).PureScript

in  let Dependency : Type = < PureScript : PureScript >

in  Dependency
