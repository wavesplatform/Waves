package scorex.transaction

import com.wavesplatform.{TransactionGen, WithDB}
import com.wavesplatform.state.HistoryTest
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.IssueTransactionV2

class IssueTransactionV2Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen with WithDB with HistoryTest {

  property("SmartIssueTransaction serialization roundtrip") {
    forAll(smartIssueTransactionGen()) { tx: IssueTransactionV2 =>
      val recovered = IssueTransactionV2.parseBytes(tx.bytes()).get

      tx.sender.address shouldEqual recovered.sender.address
      tx.timestamp shouldEqual recovered.timestamp
      tx.decimals shouldEqual recovered.decimals
      tx.description shouldEqual recovered.description
      tx.script shouldEqual recovered.script
      tx.reissuable shouldEqual recovered.reissuable
      tx.fee shouldEqual recovered.fee
      tx.name shouldEqual recovered.name
      tx.chainId shouldEqual recovered.chainId
      tx.bytes() shouldEqual recovered.bytes()
    }
  }
}
