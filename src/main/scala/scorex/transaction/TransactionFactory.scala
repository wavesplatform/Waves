package scorex.transaction

import java.util.Arrays
import com.google.common.primitives.Ints
import scorex.transaction.Transaction.TransactionType
import scorex.transaction.Transaction.TransactionType.{PAYMENT_TRANSACTION, GENESIS_TRANSACTION}

object TransactionFactory {
	def parse(data:Array[Byte]):Transaction = {
		val typeBytes = Arrays.copyOfRange(data, 0, Transaction.TYPE_LENGTH)
		val txType = Ints.fromByteArray(typeBytes)
		
		txType match{
			case i:Int if i == GENESIS_TRANSACTION.id =>
				GenesisTransaction.Parse(Arrays.copyOfRange(data, 4, data.length))
			
			case i:Int if i == PAYMENT_TRANSACTION.id =>
				PaymentTransaction.Parse(Arrays.copyOfRange(data, 4, data.length))

			case _ => throw new Exception("Invalid transaction type")
		}
	}
}
