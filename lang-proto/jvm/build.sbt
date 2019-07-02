import java.nio.file.Paths

inConfig(Compile)(Seq(
  PB.targets += scalapb.gen(flatPackage = true) -> Paths.get("lang-proto/shared/src/main/scala").toAbsolutePath.toFile,
))