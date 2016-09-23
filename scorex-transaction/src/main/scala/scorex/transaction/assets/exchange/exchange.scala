package scorex.transaction.assets

package object exchange {

  type AssetId = Array[Byte]
  val WavesAssetId = Array.fill(32)(-127: Byte)
  val PriceConstant = 100000000

}
