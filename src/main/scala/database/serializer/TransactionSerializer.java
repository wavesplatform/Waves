package database.serializer;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.Serializable;

import org.mapdb.Serializer;

import scorex.transaction.Transaction;
import scorex.transaction.TransactionFactory;

public class TransactionSerializer implements Serializer<Transaction>, Serializable
{
	private static final long serialVersionUID = -6538913048331349777L;

	@Override
	public void serialize(DataOutput out, Transaction value) throws IOException 
	{
		out.writeInt(value.getDataLength());
        out.write(value.toBytes());
    }

    @Override
    public Transaction deserialize(DataInput in, int available) throws IOException 
    {
    	int length = in.readInt();
        byte[] bytes = new byte[length];
        in.readFully(bytes);
        try 
        {
        	return TransactionFactory.getInstance().parse(bytes);
		} 
        catch (Exception e) 
        {
        	e.printStackTrace();
		}
		return null;
    }

    @Override
    public int fixedSize() 
    {
    	return -1;
    }
}
