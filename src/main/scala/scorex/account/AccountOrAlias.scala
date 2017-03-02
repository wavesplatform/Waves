package scorex.account


trait AccountOrAlias {
  def stringRepr : String
  def bytes : Array[Byte]
}