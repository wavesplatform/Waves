package database.serializer;

import org.mapdb.Serializer;
import scorex.block.Block;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.Serializable;

public class BlockSerializer implements Serializer<Block>, Serializable {
    private static final long serialVersionUID = -6538913048331349777L;

    @Override
    public void serialize(DataOutput out, Block value) throws IOException {
        out.writeInt(value.dataLength());
        out.write(value.toBytes());
    }

    @Override
    public Block deserialize(DataInput in, int available) throws IOException {
        int length = in.readInt();
        byte[] bytes = new byte[length];
        in.readFully(bytes);
        try {
            return Block.parse(bytes).get();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public int fixedSize() {
        return -1;
    }
}
