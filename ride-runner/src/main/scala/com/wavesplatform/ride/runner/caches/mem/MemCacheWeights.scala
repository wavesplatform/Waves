package com.wavesplatform.ride.runner.caches.mem

import com.google.protobuf.ByteString
import com.wavesplatform.account.Alias
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.caches.{LazyBlockchain, WeighedAssetDescription}
import com.wavesplatform.ride.runner.requests.{RideScriptRunRequest, RideScriptRunResult}
import com.wavesplatform.state.{AssetScriptInfo, BinaryDataEntry, BooleanDataEntry, DataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.{base58Length, base64Length}
import org.ehcache.sizeof.SizeOf
import org.ehcache.sizeof.filters.SizeOfFilter

import java.lang.reflect.Field
import java.util

// Close size of objects in bytes
object MemCacheWeights {
  private val sizeOf = SizeOf.newInstance(
    false,
    true,
    new SizeOfFilter {
      private val excludeClassNames = Set(classOf[LazyBlockchain[?]].getName)
      override def filterFields(klazz: Class[?], fields: util.Collection[Field]): util.Collection[Field] = fields
      override def filterClass(klazz: Class[?]): Boolean = {
        // Because we can't determine their sizes or they shared among all objects
        val remove = klazz.getName.contains("$$Lambda$") || excludeClassNames.contains(klazz.getName)
        !remove
      }
    }
  )

  // Uses the reflection, so generally much slower than custom calculations
  private def ofAny(x: Any): Int = {
    val longWeight = sizeOf.deepSizeOf(x)
    if (longWeight.isValidInt) longWeight.toInt
    else throw new ArithmeticException(s"Weight of ${x.getClass.getSimpleName} overflow: $longWeight")
  }

  val OfCachedRemoteDataOverhead = 16 // = 12 (header) + 4 (ref)

  val ofByteStr32 = ofByteStr(32) // 368

  // 40 = 12 (header) + 1 (chainId) + 4 (publicKeyHash ref) + 4 (checksum ref) + 4 (bytes ref) + 4 (toString ref) + 4 (hc) + 6 (align) +
  // .publicKeyHash: 40 = 12 (header) + 4 (len) + 20 (xs) + 4 (align)
  // .checksum:      24 = 12 (header) + 4 (len) + 4 (xs) + 4 (align)
  // .bytes:         48 = 12 (header) + 4 (len) + 26 (xs) + 6 (align)
  // .toString:      80 = ofAsciiString(35) = 24 + (16 + 35 + 5 (align)) = 80
  val OfAddress = 232

  val OfPublicKey     = ofByteStr32
  val OfTransactionId = ofByteStr32
  val OfIssuedAsset   = 16 + OfTransactionId // = 12 (header) + 4 (ref) +

  def ofAsset(x: Asset): Int = x.fold(16)(_ => OfIssuedAsset) // 12 (header) + 4 (align)

  // It is expected, that x is a valid long
  val OfBigIntTotalVolume = 24 // = 12 (header) + 4 (ref, null _bigInteger) + 8 (_long)

  val OfLeaseBalance = 32 // = 12 (header) + 8 (in) + 8 (out) + 4 (align)

  def ofRideScriptRunRequest(x: RideScriptRunRequest): Int = ofAny(x)
  def ofRideScriptRunResult(x: RideScriptRunResult): Int   = ofAny(x)
  def ofAssetScriptInfo(x: AssetScriptInfo): Int           = ofAny(x)
  def ofScript(x: Script): Int                             = ofAny(x)

  def ofWeighedAssetDescription(x: WeighedAssetDescription): Int = {
    24 + // 24 = 12 (header) + 4 (scriptWeight) + 4 (ref: assetDescription) + 4 (align)
      // AssetDescription: 12 (header) + 4 (decimals) + 8 (sponsorship) + 4 (lastUpdatedAt) + 4 (sequenceInBlock) + 4 (issueHeight) + 1 (reissuable)
      //   + 1 (nft) + 2 (align) + 4 * 6 (ref: originTransactionId, issuer, name, description, totalVolume, script) +
      64 +
      OfTransactionId + OfPublicKey + ofByteString(x.assetDescription.name) + ofByteString(x.assetDescription.description) +
      OfBigIntTotalVolume +
      x.assetDescription.script.fold(16) { _ => // None: 16 = 12 (header) + 4 (align)
        16 + x.scriptWeight                     // Some: 16 = 12 (header) + 4 (ref)
      }
  }

  def ofDataEntry(x: DataEntry[?]): Int = {
    // 24 = 12 (header) + 4 (ref, type) + 4 (ref, key) + 4 (ref, value) +
    24 + ofAsciiString(x.`type`) + ofAsciiString(x.key) + (x match {
      case _: IntegerDataEntry => 24 // Long = 12 (header) + 4 (align) + 8 (value)
      case _: BooleanDataEntry => 16 // Boolean = 12 (header) + 1 (value) + 3 (align)
      case x: BinaryDataEntry  => ofByteStr(x.value)
      case x: StringDataEntry  => ofAsciiString(x.value)
      case _: EmptyDataEntry   => 16 // Unit = 12 (header) + 4 (align)
    })
  }

  def ofAlias(x: Alias): Int = {
    // 32 = 12 (header) + 1 (chainId) + 1 (lazy vals) + 2 (align) + 4 (ref, bytes) + 4 (ref, toString) + 4 (ref, name) + 4 (align) +
    32 +
      ofArray(4 + x.name.length) +       // bytes: 1 (AddressVersion) + 1 (chainId) + 2 (short len)
      ofAsciiString(8 + x.name.length) + // toString: 8 is a prefix
      ofAsciiString(x.name.length)       // name
  }

  def ofByteString(x: ByteString): Int = {
    // 24 = 12 (header) + 4 (ref) + 4 (hash) + 4 (align)
    24 + ofArray(x.size())
  }

  def ofByteStr(x: ByteStr): Int = ofByteStr(x.arr.length)

  def ofByteStr(len: Int): Int = ofByteStrB64(len) + ofAsciiString(base58Length(len)) // base58

  /** @return
    *   With evaluated base64 fields
    */
  def ofByteStrB64(len: Int): Int = {
    // val isSmall   = x.arr.length < 1024
    val b64len    = base64Length(len)
    val base64Raw = ofAsciiString(b64len)
    // 48 = 12 (header) + 4 (hc) + 1 (bitmap for lazy vals) + 3 (align) + 4 * 6 (ref: base58, base64Raw, base64, trim, toString, arr) + 4 (align) +
    48 +
      base64Raw +
      ofAsciiString(7 + b64len) + // base64: 7 is a prefix "base64:"
      // Commented: we don't use it
      // (if (isSmall) ofAsciiString(10) else ofAsciiString(b64len + 3)) + // trim: 10 is 7 + 3 ("...")
      // toString: 0, because it references the same string
      ofArray(len) // arr
  }

  //  24 = 12 (header) + 4 (hash) + 1 (coder) + 1 (hashIsZero) + 2 (align) + 4 (array ref)
  def ofAsciiString(str: String): Int = ofAsciiString(str.length)
  def ofAsciiString(len: Int): Int    = 24 + ofArray(len)

  // 16 = 12 (header) + 4 (len)
  def ofArray(len: Int): Int = roundUp8(16 + len)

  def roundUp8(x: Int): Int = ((x + 7) >> 3) << 3 // padding
}
