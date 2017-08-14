package io.github.yuemenglong.json.parse

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Administrator on 2017/7/5.
  */
object Parse {

  def parseNull(json: String, start: Int): (JsonNode, Int) = {
    val s = json.slice(start, start + "null".length)
    require(s == "null")
    (JsonNull(), start + "null".length - 1)
  }

  def parseBool(json: String, start: Int): (JsonNode, Int) = {
    json(start) match {
      case 't' =>
        val s = json.slice(start, start + "true".length)
        require(s == "true")
        (JsonBool(s.toBoolean), start + "true".length - 1)
      case 'f' =>
        val s = json.slice(start, start + "false".length)
        require(s == "false")
        (JsonBool(s.toBoolean), start + "false".length - 1)
    }
  }

  def parseStr(json: String, start: Int): (JsonNode, Int) = {
    var pos = start + 1
    var s = ""
    while (json(pos) != '"') {
      if (json(pos) == '\\' && json(pos + 1) == '"') {
        s += '"'
        pos += 2
      } else if (json(pos) == '\\' && json(pos + 1) == '\\') {
        s += '\\'
        pos += 2
      } else {
        s += json(pos)
        pos += 1
      }
    }
    (JsonStr(s), pos)
  }

  def parseNum(json: String, start: Int): (JsonNode, Int) = {
    var pos = start
    var n = ""
    var hasDot = false
    var c = json(pos)
    while (('0' to '9').contains(c) || (c == '.' && !hasDot) || (c == '-' && pos == start)) {
      if (c == '.') {
        hasDot = true
      }
      n += c
      pos += 1
      c = json(pos)
    }
    if (hasDot) {
      (JsonDouble(n.toDouble), pos - 1)
    } else {
      (JsonLong(n.toLong), pos - 1)
    }
  }

  def parseObj(json: String, start: Int): (JsonNode, Int) = {
    var pos = start + 1
    var state = "name" // colon value dot finish
    var map = Map[String, JsonNode]()
    var name = ""

    while (state != "finish") {
      //1. get name
      if (state == "name") {
        json(pos) match {
          // 特殊情况
          case '}' =>
            state = "finish"
          case ' ' | '\n' =>
            pos += 1
          case '"' =>
            val (n, p) = parseStr(json, pos)
            state = "colon"
            name = n.asInstanceOf[JsonStr].value
            pos = p + 1
          case _ => throw new RuntimeException(s"Unexpected ${json(pos)} At $pos When Parse [Name] Of Object")
        }
      }
      //2. eat colon
      if (state == "colon") {
        json(pos) match {
          // 特殊情况
          case ' ' | '\n' =>
            pos += 1
          case ':' =>
            state = "value"
            pos += 1
          case _ => throw new RuntimeException(s"Unexpected ${json(pos)} At $pos When Parse [:] Of Object")
        }
      }
      //3. get value
      if (state == "value") {
        json(pos) match {
          case ' ' | '\n' =>
            pos += 1
          // 一般情况
          case 'n' =>
            val (n, p) = parseNull(json, pos)
            map += (name -> n)
            state = "dot"
            pos = p + 1
          case 't' | 'f' =>
            val (n, p) = parseBool(json, pos)
            map += (name -> n)
            state = "dot"
            pos = p + 1
          case n if ('0' to '9').contains(n) || '-' == n =>
            val (n, p) = parseNum(json, pos)
            map += (name -> n)
            state = "dot"
            pos = p + 1
          case '"' =>
            val (n, p) = parseStr(json, pos)
            map += (name -> n)
            state = "dot"
            pos = p + 1
          case '{' =>
            val (n, p) = parseObj(json, pos)
            map += (name -> n)
            state = "dot"
            pos = p + 1
          case '[' =>
            val (n, p) = parseArr(json, pos)
            map += (name -> n)
            state = "dot"
            pos = p + 1
          case _ => throw new RuntimeException(s"Unexpected ${json(pos)} At $pos When Parse [Value] Of Object")
        }
      }
      //4. eat dot
      if (state == "dot") {
        json(pos) match {
          // 特殊情况
          case '}' =>
            state = "finish"
          case ' ' | '\n' =>
            pos += 1
          case ',' =>
            state = "name"
            pos += 1
          case _ => throw new RuntimeException(s"Unexpected ${json(pos)} At $pos When Parse [,] Of Object")
        }
      }
    }
    (JsonObj(map), pos)
  }

  def parseArr(json: String, start: Int): (JsonNode, Int) = {
    var pos = start + 1
    var state = "item" // dot finish
    var arr = ArrayBuffer[JsonNode]()

    while (state != "finish") {
      //1. get item
      if (state == "item") {
        json(pos) match {
          // 特殊情况
          case ']' =>
            state = "finish"
          case ' ' | '\n' =>
            pos += 1
          // 一般情况
          case 'n' =>
            var (n, p) = parseNull(json, pos)
            arr += n
            state = "dot"
            pos = p + 1
          case n if ('0' to '9').contains(n) || '-' == n =>
            var (n, p) = parseNum(json, pos)
            arr += n
            state = "dot"
            pos = p + 1
          case '"' =>
            var (n, p) = parseStr(json, pos)
            arr += n
            state = "dot"
            pos = p + 1
          case '{' =>
            var (n, p) = parseObj(json, pos)
            arr += n
            state = "dot"
            pos = p + 1
          case '[' =>
            var (n, p) = parseArr(json, pos)
            arr += n
            state = "dot"
            pos = p + 1
          case _ => throw new RuntimeException(s"Unexpected ${json(pos)} At $pos")
        }
      }
      //2. eat dot
      if (state == "dot") {
        json(pos) match {
          // 特殊情况
          case ']' =>
            state = "finish"
          case ' ' | '\n' =>
            pos += 1
          case ',' =>
            state = "item"
            pos += 1
          case _ => throw new RuntimeException(s"Unexpected ${json(pos)} At $pos")
        }
      }
    }
    (JsonArr(arr.toArray), pos)
  }
}
