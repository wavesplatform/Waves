name := "waves-grpc-server"

libraryDependencies ++= Dependencies.grpc

inConfig(Compile)(Seq(
  PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
))
