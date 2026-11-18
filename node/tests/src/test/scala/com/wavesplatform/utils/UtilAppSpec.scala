package com.wavesplatform.utils

import com.wavesplatform.db.WithDomain
import com.wavesplatform.state.Height
import com.wavesplatform.test.*
import org.bouncycastle.util.encoders.Hex
import play.api.libs.json.{JsObject, Json}

class UtilAppSpec extends FreeSpec with WithDomain {
  private val withStateHashes = DomainPresets.RideV6.copy(dbSettings = DomainPresets.RideV6.dbSettings.copy(storeStateHashes = true))

  private def json(result: Either[String, Array[Byte]]): JsObject = Json.parse(result.value).as[JsObject]

  "state-hash" - {
    "shows the debug API state hash at the given height" in withDomain(withStateHashes) { d =>
      (1 to 3).foreach(_ => d.appendBlock())

      val result = json(UtilApp.Actions.doStateHash(d.rocksDBWriter, Some(Height(2))))

      (result \ "height").as[Int] shouldBe 2
      (result \ "blockId").as[String] shouldBe d.rocksDBWriter.blockHeader(2).value.id().toString
      (result \ "stateHash").as[String] shouldBe Hex.toHexString(d.rocksDBWriter.loadStateHash(Height(2)).value.totalHash.arr)
    }

    "shows the state hash at the top persisted height when no height is given" in withDomain(withStateHashes) { d =>
      (1 to 3).foreach(_ => d.appendBlock())

      val result = json(UtilApp.Actions.doStateHash(d.rocksDBWriter, None))

      (result \ "height").as[Int] shouldBe d.rocksDBWriter.height
    }

    "fails when no state hash is stored at the height" in withDomain(DomainPresets.RideV6) { d =>
      (1 to 3).foreach(_ => d.appendBlock())

      UtilApp.Actions.doStateHash(d.rocksDBWriter, Some(Height(2))) shouldBe Left("No state hash at height 2")
    }
  }

  "rollback-to" - {
    "rolls the persisted state back to the given height" in withDomain(DomainPresets.RideV6) { d =>
      (1 to 5).foreach(_ => d.appendBlock())

      UtilApp.Actions.doRollbackTo(d.rocksDBWriter, Height(2)).map(new String(_)) shouldBe Right("Rolled back to height 2\n")
      d.rocksDBWriter.height shouldBe 2
    }

    "refuses a height that is not below the persisted height" in withDomain(DomainPresets.RideV6) { d =>
      (1 to 5).foreach(_ => d.appendBlock())
      val height = d.rocksDBWriter.height

      UtilApp.Actions.doRollbackTo(d.rocksDBWriter, Height(height + 10)) shouldBe Left(s"Height ${height + 10} is not below the current height $height")
      UtilApp.Actions.doRollbackTo(d.rocksDBWriter, Height(height)) shouldBe Left(s"Height $height is not below the current height $height")
      d.rocksDBWriter.height shouldBe height
    }

    "refuses to roll back below the safe rollback height" in withDomain(
      DomainPresets.RideV6.copy(dbSettings = DomainPresets.RideV6.dbSettings.copy(maxRollbackDepth = 2))
    ) { d =>
      (1 to 6).foreach(_ => d.appendBlock())

      UtilApp.Actions.doRollbackTo(d.rocksDBWriter, Height(1)) shouldBe Left("Rollback is possible only to the block at the height: 3")
      d.rocksDBWriter.height shouldBe 5
    }
  }
}
