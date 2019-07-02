import java.nio.file.Paths

coverageExcludedPackages := ""
publishMavenStyle := true
credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
publishTo := Some("Sonatype Nexus" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
name := "RIDE Compiler"
normalizedName := "lang"
description := "The RIDE smart contract language compiler"
homepage := Some(url("https://docs.wavesplatform.com/en/technical-details/waves-contracts-language-description/maven-compiler-package.html"))
developers := List(Developer("petermz", "Peter Zhelezniakov", "peterz@rambler.ru", url("https://wavesplatform.com")))
libraryDependencies ++=
  Seq(
    "org.scala-js"                      %% "scalajs-stubs" % "1.0.0-RC1" % Provided,
    "com.github.spullara.mustache.java" % "compiler"       % "0.9.5"
  )

val source = Paths.get("common/jvm/target/src_managed/main").toAbsolutePath.toFile
val dest   = Paths.get("lang/shared/src/main/scala").toAbsolutePath.toFile

val moveGeneratedModel = Def.task {
  val filePath = "com/wavesplatform/protobuf/dapp/DAppMeta.scala"
  IO.copyFile(
    source / filePath,
    dest / filePath
  )
  val filePath2 = "com/wavesplatform/protobuf/dapp/DAppMetaProto.scala"
  IO.copyFile(
    source / filePath2,
    dest / filePath2
  )
  while (!((dest / filePath).exists && (dest / filePath2).exists)) {}
  println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  Seq(dest / filePath, dest / filePath2)
}
inConfig(Compile)(Seq(
  sourceGenerators += moveGeneratedModel
))