package database;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.mapdb.BTreeKeySerializer;
import org.mapdb.DB;

import scorex.block.Block;
import scorex.transaction.Transaction;

import com.google.common.primitives.UnsignedBytes;

public class TransactionParentMap extends DBMap<byte[], byte[]> 
{
	private Map<Integer, Integer> observableData = new HashMap<Integer, Integer>();
	
	private BlockMap blockMap;
	
	public TransactionParentMap(DBSet databaseSet, DB database)
	{
		super(databaseSet, database);
		this.blockMap = databaseSet.getBlockMap();
	}

	public TransactionParentMap(BlockMap blockMap, TransactionParentMap parent) 
	{
		super(parent);
		this.blockMap = blockMap;
	}
	
	protected void createIndexes(DB database){}

	@Override
	protected Map<byte[], byte[]> getMap(DB database) 
	{
		//OPEN MAP
		return database.createTreeMap("children")
				.keySerializer(BTreeKeySerializer.BASIC)
				.comparator(UnsignedBytes.lexicographicalComparator())
				.makeOrGet();
	}

	@Override
	protected Map<byte[], byte[]> getMemoryMap() 
	{
		return new TreeMap<byte[], byte[]>(UnsignedBytes.lexicographicalComparator());
	}

	@Override
	protected byte[] getDefaultValue() 
	{
		return null;
	}
	
	@Override
	protected Map<Integer, Integer> getObservableData() 
	{
		return this.observableData;
	}
	
	public Block getParent(byte[] signature)
	{
		return this.blockMap.get(this.get(signature));
	}
	
	public void set(Transaction child, Block parent)
	{
		this.set(child.getSignature(), parent.signature());
	}
}
