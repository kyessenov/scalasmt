package cap.scalasmt

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

/**
 * Persistence of symbolic expressions.
 * @author kuat
 */ 
object Persistence {
  def serialize(e : AnyRef) : Array[Byte] = {
    val baos = new ByteArrayOutputStream(1024);
    val o = new ObjectOutputStream(baos);
    o.writeObject(e);
    baos.toByteArray();
  }

  def deserialize[T] (s : Array[Byte]) : T = {
    val bais = new ByteArrayInputStream(s);
    val input = new ObjectInputStream(bais);
    val obj = input.readObject();
    obj.asInstanceOf[T]
  }
}

