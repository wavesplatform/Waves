package com.wavesplatform.state.snapshot

import com.google.protobuf.ByteString
import com.wavesplatform.account.Alias
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot.AssetStatic
import com.wavesplatform.state.*
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.reader.LeaseDetails.Status
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers.{defaultAddress, defaultSigner, secondAddress, secondSigner}

import scala.collection.immutable.VectorMap

class StateSnapshotProtoTest extends PropSpec {
  property("serialization") {
    val snapshot = StateSnapshot(
      transactions = VectorMap(),
      VectorMap(
        (defaultAddress, Waves)                                    -> 123,
        (secondAddress, IssuedAsset(ByteStr.fromBytes(7, 17, 77))) -> 456
      ),
      Map(
        defaultAddress -> LeaseBalance(999, 888),
        secondAddress  -> LeaseBalance.empty
      ),
      VectorMap(
        IssuedAsset(ByteStr.fromBytes(1, 1, 1)) -> AssetStatic(
          ByteString.copyFrom(Array[Byte](1, 1, 1)),
          ByteString.copyFromUtf8("txId"),
          ByteString.copyFromUtf8("pk"),
          5,
          nft = true
        ),
        IssuedAsset(ByteStr.fromBytes(2, 2, 2)) -> AssetStatic(
          ByteString.copyFrom(Array[Byte](2, 2, 2)),
          ByteString.copyFromUtf8("txId"),
          ByteString.copyFromUtf8("pk"),
          5,
          nft = false
        )
      ),
      Map(
        IssuedAsset(ByteStr.fromBytes(1, 1, 1)) -> AssetVolumeInfo(isReissuable = true, BigInt(123)),
        IssuedAsset(ByteStr.fromBytes(2, 2, 2)) -> AssetVolumeInfo(isReissuable = false, BigInt(0))
      ),
      Map(
        IssuedAsset(ByteStr.fromBytes(1, 1, 1)) -> AssetInfo("name1", "desc1", Height @@ 888),
        IssuedAsset(ByteStr.fromBytes(2, 2, 2)) -> AssetInfo("name2", "desc2", Height @@ 999)
      ),
      Map(
        IssuedAsset(ByteStr.fromBytes(2, 2, 2)) -> AssetScriptInfo(TestCompiler(V6).compileAsset("this != this"), 0)
      ),
      Map(
        IssuedAsset(ByteStr.fromBytes(1, 1, 1)) -> SponsorshipValue(99999),
        IssuedAsset(ByteStr.fromBytes(2, 2, 2)) -> SponsorshipValue(0)
      ),
      Map(
        ByteStr.fromBytes(4, 5, 6) -> LeaseDetails(defaultSigner.publicKey, secondAddress, 123, Status.Active, ByteStr.fromBytes(1, 2, 3), 4),
        ByteStr.fromBytes(7, 8, 9) -> LeaseDetails(
          secondSigner.publicKey,
          defaultAddress,
          0,
          Status.Cancelled(2, Some(ByteStr.fromBytes(5, 5, 5))),
          ByteStr.fromBytes(1, 2, 3),
          777777777
        )
      ),
      Map(
        Alias.create("alias").explicitGet()  -> defaultAddress,
        Alias.create("alias2").explicitGet() -> secondAddress
      ),
      Map(
        ByteStr.fromBytes(1, 1, 1) -> VolumeAndFee(100, 200),
        ByteStr.fromBytes(2, 2, 2) -> VolumeAndFee(0, 0)
      ),
      Map(
        defaultSigner.publicKey -> Some(AccountScriptInfo(defaultSigner.publicKey, TestCompiler(V6).compileExpression("this != this"), 123)),
        secondSigner.publicKey  -> None
      ),
      Map(
        defaultAddress -> Map(
          "a" -> IntegerDataEntry("a", 123),
          "b" -> StringDataEntry("b", "string"),
          "c" -> BooleanDataEntry("c", true),
          "d" -> BinaryDataEntry("d", ByteStr.fromBytes(7, 7, 7)),
          "e" -> EmptyDataEntry("e")
        ),
        secondAddress -> Map(
          "a" -> BinaryDataEntry("a", ByteStr.fromBytes(8, 8, 8)),
          "b" -> EmptyDataEntry("b")
        )
      )
    )
    Seq(TxMeta.Status.Succeeded, TxMeta.Status.Failed, TxMeta.Status.Elided)
      .foreach(txStatus => StateSnapshot.fromProtobuf(snapshot.toProtobuf(txStatus)) shouldBe (snapshot, txStatus))
  }
}
