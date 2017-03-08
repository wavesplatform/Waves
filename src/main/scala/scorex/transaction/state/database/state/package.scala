package scorex.transaction.state.database

import scorex.transaction.StateChangeReason

package object state {
  type AddressString = String
  type ReasonIds = List[Array[Byte]]
  type Reasons = List[StateChangeReason]
}
