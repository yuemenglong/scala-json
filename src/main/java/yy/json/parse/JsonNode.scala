package yy.json.parse

/**
  * Created by Administrator on 2017/7/5.
  */
abstract class JsonNode extends AsT {
  override def as[T](clazz: Class[T]): T = {
    (this, clazz.isArray) match {
      case (arr: JsonArr, true) => Convert.toArray(arr, clazz).asInstanceOf[T]
      case (obj: JsonObj, false) => Convert.toObject(obj, clazz).asInstanceOf[T]
      case _ => throw new RuntimeException("Type Not Match, Maybe Need Pass Array Class ")
    }
  }
}

case class JsonNull() extends JsonNode {
  override def toString: String = {
    "null"
  }
}

case class JsonBool(var value: Boolean) extends JsonNode {
  override def toString: String = {
    s"$value"
  }
}

case class JsonNum(var value: Double) extends JsonNode {
  override def toString: String = {
    s"$value"
  }
}

case class JsonStr(var value: String) extends JsonNode {
  override def toString: String = {
    s""""$value""""
  }
}

case class JsonObj(var map: Map[String, JsonNode]) extends JsonNode {
  override def toString: String = {
    val content = map.toList.map(p => {
      val (name, node) = p
      s""""$name": $node"""
    }).mkString(",")
    s"{$content}"
  }
}

case class JsonArr(var array: Array[JsonNode]) extends JsonNode {
  override def toString: String = {
    val content = array.map(node => {
      s"$node"
    }).mkString(",")
    s"[$content]"
  }
}

case class JsonPlain(var value: String) extends JsonNode {
  override def toString: String = {
    value
  }
}