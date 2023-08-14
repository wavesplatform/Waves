package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.Address
import com.wavesplatform.database.AddressId
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}

trait AddressIdPersistentCache {
  def getAddress(addressId: AddressId)(implicit ctx: ReadOnly): Option[Address]
  def getAddressId(address: Address)(implicit ctx: ReadOnly): Option[AddressId]

  def getOrMkAddressId(address: Address)(implicit ctx: ReadWrite): AddressId
}
