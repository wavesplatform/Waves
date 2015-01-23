package database;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.mapdb.BTreeKeySerializer;
import org.mapdb.DB;

import scorex.block.Block;

import com.google.common.primitives.UnsignedBytes;

public class HeightMap extends DBMap<byte[], Integer> 
{
	private Map<Integer, Integer> observableData = new HashMap<Integer, Integer>();
	
	public HeightMap(DBSet databaseSet, DB database)
	{
		super(databaseSet, database);
	}

	public HeightMap(HeightMap parent) 
	{
		super(parent);
	}
	
	protected void createIndexes(DB database){}

	@Override
	protected Map<byte[], Integer> getMap(DB database) 
	{
		//OPEN MAP
		return database.createTreeMap("height")
			.keySerializer(BTreeKeySerializer.BASIC)
			.comparator(UnsignedBytes.lexicographicalComparator())
			.makeOrGet();
	}

	@Override
	protected Map<byte[], Integer> getMemoryMap() 
	{
		return new TreeMap<byte[], Integer>(UnsignedBytes.lexicographicalComparator());
	}

	@Override
	protected Integer getDefaultValue() 
	{
		return -1;
	}
	
	@Override
	protected Map<Integer, Integer> getObservableData() 
	{
		return this.observableData;
	}
	
	public int get(Block block)
	{
		return this.get(block.signature());
	}
	
	public void set(Block block, int height)
	{
		this.set(block.signature(), height);
	}
}
