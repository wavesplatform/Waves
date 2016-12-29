package scorex.transaction.state.database

import scorex.transaction.StateChangeReason

package object state {
  type Address = String
  type Reason = List[StateChangeReason]

}
