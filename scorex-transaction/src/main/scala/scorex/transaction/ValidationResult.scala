package scorex.transaction

object ValidationResult extends Enumeration {
  type ValidationResult = Value

  val ValidateOke = Value(1)
  val InvalidAddress = Value(2)
  val NegativeAmount = Value(3)
  val InsufficientFee = Value(4)
  val NoBalance = Value(5)
  val TooBigArray = Value(6)
  val InvalidSignature = Value(7)
  val InvalidName = Value(8)
  val StateCheckFailed = Value(9)
  val OverflowError = Value(10)
}