package scorex.transaction.assets.exchange

case class Validation(
                   status: Boolean,
                   labels: Set[String] = Set.empty
                 ) {
  def hasError(error: String): Boolean = !status && labels.contains(error)

  def messages(): String = labels.mkString(", ")

  def &&(r: Validation) = (this.status, r.status) match {
    case (false,_) => this
    case (_,false) => r
    case (true, true) => Validation(true)
  }

  private def mergeRes(x: Validation, y: Validation, st: Boolean) =
    Validation(
      status = st,
      labels = x.labels ++ y.labels
    )

  def :|(l: String): Validation = copy(labels = labels + l)

  def |:(l: String): Validation = copy(labels = labels.map(l + " " + _))

}

class ExtendedBoolean(b: => Boolean) {
  def :|(l: String) = Validation(b) :| l
  def |:(l: String) = l |: Validation(b)
}

case object Validation {
  implicit def BooleanOperators(b: => Boolean): ExtendedBoolean = new ExtendedBoolean(b)
  implicit def result2Bolean(x: Validation): Boolean = x.status
}