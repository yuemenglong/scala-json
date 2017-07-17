package yy.json.parse

/**
  * Created by <yuemenglong@126.com> on 2017/7/14.
  */
object Walk {
  def walk(node: JsonNode, fn: (JsonNode) => JsonNode): JsonNode = {
    val newNode = fn(node)
    newNode match {
      case v: JsonObj => JsonObj(v.map.map { case (k, v) => (k, walk(v, fn)) })
      case v: JsonArr => JsonArr(v.array.map(walk(_, fn)))
      case _ => newNode
    }
  }
}
