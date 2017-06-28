package com.wavesplatform.it.transactions

import com.wavesplatform.TransactionGen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scorex.transaction.Data.DataTransaction
import scorex.transaction.TransactionParser.TransactionType
import scala.util.Try

class DataTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {
  def parseBytes(bytes: Array[Byte]): Try[DataTransaction] = Try {
    require(bytes.head == TransactionType.DataTransaction.id)
    DataTransaction.parseTail(bytes.tail).get
  }

  property("Data serialization roundtrip") {
    forAll(dataGen) { data: DataTransaction =>
      val recovered = parseBytes(data.bytes).get
      recovered.fee shouldEqual data.fee
    }
  }

}