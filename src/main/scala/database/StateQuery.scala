package database


trait StateQuery {
  def balance(address:String, fromHeight:Int, confirmations:Int):BigDecimal
  def balance(address:String):BigDecimal = balance(address, 0, 0)
  def generationBalance(address:String):BigDecimal = balance(address, 0, 50)
}
