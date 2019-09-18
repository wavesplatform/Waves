name := "blockchain-updates-model"

libraryDependencies ++= Dependencies.protobuf.value

inConfig(Compile)(
  Seq(
    PB.protoSources in Compile := Seq(PB.externalIncludePath.value),
    includeFilter in PB.generate := new SimpleFileFilter((f: File) => f.getName.endsWith(".proto") && f.getParent.endsWith("events")),
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
  )
)
