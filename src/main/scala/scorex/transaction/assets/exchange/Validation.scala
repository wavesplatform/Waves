package scorex.transaction.assets.exchange

case class Validation(
                   status: Boolean,
                   labels: Set[String] = Set.empty
                 ) {
  def hasError(error: String): Boolean = !status && labels.contains(error)

  def messages(): String = labels.mkString(", ")

  def &&(r: => Validation): Validation =
    if (!this.status) {
      this
    } else {
      if (!r.status) r
      else Validation(true)
    }

  private def mergeRes(x: Validation, y: Validation, st: Boolean) =
    Validation(
      status = st,
      labels = x.labels ++ y.labels
    )

  def :|(l: String): Validation = if (!this.status) copy(labels = labels + l) else this

  def |:(l: String): Validation = if (!this.status) copy(labels = labels.map(l + " " + _)) else this

}

class ExtendedBoolean(b: => Boolean) {
  def :|(l: String) = Validation(b) :| l
  def |:(l: String) = l |: Validation(b)
}

case object Validation {
  implicit def BooleanOperators(b: => Boolean): ExtendedBoolean = new ExtendedBoolean(b)
  implicit def result2Bolean(x: Validation): Boolean = x.status

  val success = Validation(status = true)
}

