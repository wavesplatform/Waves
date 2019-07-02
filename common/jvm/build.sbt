import java.nio.file.Paths

val source = Paths.get("common/jvm/target/src_managed/main").toAbsolutePath.toFile
val dest   = Paths.get("lang/shared/src/main/scala").toAbsolutePath.toFile

val moveGeneratedModel = Def.task {
  val filePath = "com/wavesplatform/protobuf/dapp/DAppMeta.scala"
  IO.copyFile(
    source / filePath,
    dest / filePath
  )
  Seq(dest / filePath)
}
inConfig(Compile)(Seq(
  PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value,
  sourceGenerators += moveGeneratedModel
))