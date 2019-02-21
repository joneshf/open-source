module github.com/joneshf/open-source/programs/dhall-go

require (
	github.com/alecthomas/template v0.0.0-20160405071501-a0175ee3bccc // indirect
	github.com/alecthomas/units v0.0.0-20151022065526-2efee857e7cf // indirect
	github.com/joneshf/open-source/packages/dhall-go v0.0.0-20190130140743-ec3351e76e39
	github.com/sirupsen/logrus v1.3.0
	github.com/ugorji/go/codec v0.0.0-20190204201341-e444a5086c43
	gopkg.in/alecthomas/kingpin.v2 v2.2.6
)

replace github.com/joneshf/open-source/packages/dhall-go => ../../packages/dhall-go
