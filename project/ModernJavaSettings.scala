object ModernJavaSettings {
  val options: Seq[String] = Seq(
    "-XX:+IgnoreUnrecognizedVMOptions",
    "--add-modules=java.xml.bind",
    "--add-exports=java.base/jdk.internal.ref=ALL-UNNAMED"
  )
}
