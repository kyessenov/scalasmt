package cap.scalasmt

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

object SceevesStr {
  def serialize[T] (e : T) : Array[Byte] = {
    val baos = new ByteArrayOutputStream(1024);
    val o = new ObjectOutputStream(baos);
    o.writeObject(e);
    baos.toByteArray();
  }

  def unserialize[T] (s : Array[Byte]) : T = {
    val bais = new ByteArrayInputStream(s);
    val input = new ObjectInputStream(bais);
    val obj = input.readObject();
    obj.asInstanceOf[T]
  }
}

