package scorex.transaction.assets.exchange

case class Validation(status: Boolean, labels: Set[String] = Set.empty) {
  def hasError(error: String): Boolean = !status && labels.contains(error)

  def messages(): String = labels.mkString(", ")

  def &&(r: => Validation): Validation =
    if (!this.status) {
      this
    } else {
      if (!r.status) r
      else Validation(true)
    }

  def :|(l: String): Validation = if (!this.status) copy(labels = labels + l) else this

  def |:(l: String): Validation = if (!this.status) copy(labels = labels.map(l + " " + _)) else this

}

class ExtendedBoolean(b: => Boolean) {
  def :|(l: String): Validation = Validation(b) :| l

  def |:(l: String): Validation = l |: Validation(b)
}

case object Validation {
  implicit def booleanOperators(b: => Boolean): ExtendedBoolean = new ExtendedBoolean(b)

  implicit def result2Boolean(x: Validation): Boolean = x.status

  val success             = Validation(status = true)
  val failure: Validation = Validation(status = false)

  def failure(l: String): Validation = Validation(status = false, Set(l))
}
