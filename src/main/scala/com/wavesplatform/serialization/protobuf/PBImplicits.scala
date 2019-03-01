package com.wavesplatform.serialization.protobuf
import com.wavesplatform.account.protobuf.PBRecipientImplicits
import com.wavesplatform.block.protobuf.PBBlockImplicits
import com.wavesplatform.transaction.protobuf._

trait PBImplicits
    extends PBTransactionImplicits
    with PBSignedTransactionImplicits
    with PBBlockImplicits
    with PBAmountImplicits
    with PBRecipientImplicits
    with PBMappers

object PBImplicits extends PBImplicits
