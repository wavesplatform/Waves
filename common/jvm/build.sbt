
inConfig(Compile)(Seq(
  PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value,
))