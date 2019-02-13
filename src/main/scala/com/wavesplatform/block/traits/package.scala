package com.wavesplatform.block
import com.wavesplatform.block.Block.BlockId
import monix.eval.Coeval
import play.api.libs.json.JsObject

package object traits {
  trait HasBlockHeaderJson {
    def headerJson: Coeval[JsObject]
  }

  trait HasBlockJson {
    def json: Coeval[JsObject]
  }

  trait HasBlockScore {
    def blockScore: Coeval[BigInt]
  }

  trait HasBlockId {
    def uniqueId: BlockId
  }

  trait HasBlockBytes {
    def bytes: Coeval[Array[Byte]]
    def bytesWithoutSignature: Coeval[Array[Byte]]
  }

  trait BaseBlock extends HasBlockHeaderJson with HasBlockJson with HasBlockScore with HasBlockBytes with HasBlockId
}
