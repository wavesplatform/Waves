object ModernJavaSettings {
  val options: Seq[String] = Seq(
    "-XX:+IgnoreUnrecognizedVMOptions",
    "--add-exports=java.base/jdk.internal.ref=ALL-UNNAMED"
  )
}
