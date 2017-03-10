package com.wavesplatform.state2

import scorex.account.Account
import scorex.transaction.{Transaction, TransactionParser}
import Primitives._

trait StateReader {
  def getTxInfo(id: Array[Byte]): Option[(Int, Transaction)]

  def accountPortfolio(a: Account): Portfolio
}

class StateReaderImpl(p: JavaMapStorage) extends StateReader {

  override def getTxInfo(id: Array[Byte]): Option[(Int, Transaction)] = Option(p.txs.get(id)).map {
    case (h, bytes) => (h, TransactionParser.parseBytes(bytes).get)
  }

  override def accountPortfolio(a: Account): Portfolio = {
    Option(p.portfolios.get(a.bytes)).map { case (b, e, as) => Portfolio(b, e, as) }.toM
  }
}


