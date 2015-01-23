package database;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.mapdb.BTreeKeySerializer;
import org.mapdb.DB;

import scorex.block.Block;

import com.google.common.primitives.UnsignedBytes;

public class ChildMap extends DBMap<byte[], byte[]> 
{
	private Map<Integer, Integer> observableData = new HashMap<Integer, Integer>();
	
	private BlockMap blockMap;
	
	public ChildMap(DBSet databaseSet, DB database)
	{
		super(databaseSet, database);
		this.blockMap = databaseSet.getBlockMap();
	}

	public ChildMap(BlockMap blockMap, ChildMap parent) 
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
	
	public Block get(Block parent)
	{
		if(this.contains(parent.signature()))
		{
			return this.blockMap.get(this.get(parent.signature()));
		}
		
		return null;
	}
	
	public void set(Block parent, Block child)
	{
		this.set(parent.signature(), child.signature());
	}
}
