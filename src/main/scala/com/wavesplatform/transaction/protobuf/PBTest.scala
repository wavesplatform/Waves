package com.wavesplatform.transaction.protobuf
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import play.api.libs.json.Json

object PBTest extends App {
  import PBTransactionImplicits._

  val vanillaTx = {
    val transfers = MassTransferTransaction
      .parseTransfersList(
        List(Transfer("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 100000000L), Transfer("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 200000000L)))
      .right
      .get

    MassTransferTransaction
      .create(
        None,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        transfers,
        1518091313964L,
        200000,
        Base58.decode("59QuUcqP6p").get,
        Proofs(Seq(ByteStr.decodeBase58("FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ").get))
      )
      .right
      .get
  }

  val tx = vanillaTx.toPB
  println(Json.toJson(tx))
}
