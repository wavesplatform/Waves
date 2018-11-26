package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.transaction.smart.ContractInvokationTransaction
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class ContractInvokationTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("SponsorFee serialization roundtrip") {
    forAll(contractInvokationGen) { transaction: ContractInvokationTransaction =>
      val bytes = transaction.bytes()
      val deser = ContractInvokationTransaction.parseBytes(bytes).get
      deser.sender == transaction.sender
      deser.contractAddress == transaction.contractAddress
      deser.fc == transaction.fc
      deser.fee == transaction.fee
      deser.timestamp == transaction.timestamp
      deser.proofs == transaction.proofs

    }

  }
}
