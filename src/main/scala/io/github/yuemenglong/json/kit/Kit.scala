package io.github.yuemenglong.json.kit

import java.lang.reflect.{Field, Method, ParameterizedType}
import java.util.concurrent.ConcurrentHashMap
import java.util.regex.Pattern

import io.github.yuemenglong.json.lang.JsonIgnore

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by Administrator on 2017/7/5.
 */
object Kit {

  var debugLog = false

  def upperCaseFirst(str: String): String = {
    if (str == null) {
      return null
    }
    if (str.isEmpty) {
      return str
    }
    str(0).toUpper + str.substring(1)
  }

  def getArrayType(clazz: Class[_]): Class[_] = {
    if (!clazz.isArray) {
      return clazz
    }
    // 这里必须是对象，不处理原生类型
    // 处理多维数组的特殊情况
    if (clazz.getName.startsWith("[[")) {
      return Class.forName(clazz.getName.substring(1))
    }
    val name = clazz.getName.replaceAll("(^\\[L)|(;$)", "")
    Class.forName(name)
  }

  def isGenericType(clazz: Class[_]): Boolean = {
    clazz.getGenericSuperclass.isInstanceOf[ParameterizedType]
  }

  def getGenericType(clazz: Class[_]): Class[_] = {
    //返回表示此 Class 所表示的实体（类、接口、基本类型或 void）的直接超类的 Type。
    val genericType = clazz.getGenericSuperclass
    if (!genericType.isInstanceOf[ParameterizedType]) return clazz
    //返回表示此类型实际类型参数的 Type 对象的数组。
    val params = genericType.asInstanceOf[ParameterizedType].getActualTypeArguments
    params(0).asInstanceOf[Class[_]]
  }

  private val fieldCache: ConcurrentHashMap[Class[_], Array[Field]] = new ConcurrentHashMap[Class[_], Array[Field]]()
  private val methodCache: ConcurrentHashMap[Class[_], Array[Method]] = new ConcurrentHashMap[Class[_], Array[Method]]()
  private val fieldMapCache: ConcurrentHashMap[Class[_], Map[String, Field]] = new ConcurrentHashMap[Class[_], Map[String, Field]]()
  private val methodMapCache: ConcurrentHashMap[Class[_], Map[String, Method]] = new ConcurrentHashMap[Class[_], Map[String, Method]]()

  def getValidFieldMap(clazz: Class[_]): Map[String, Field] = {
    if (fieldMapCache.contains(clazz)) {
      return fieldMapCache.get(clazz)
    }
    val ret: Map[String, Field] = Kit.getValidFields(clazz)
      .map(f => (f.getName, f))(collection.breakOut)
    fieldMapCache.put(clazz, ret)
    ret
  }

  def getValidMethodMap(clazz: Class[_]): Map[String, Method] = {
    if (methodMapCache.contains(clazz)) {
      return methodMapCache.get(clazz)
    }
    val ret: Map[String, Method] = Kit.getValidMethod(clazz)
      .map(m => (m.getName, m))(collection.breakOut)
    methodMapCache.put(clazz, ret)
    ret
  }

  def getValidFields(clazz: Class[_]): Array[Field] = {
    if (fieldCache.contains(clazz)) {
      return fieldCache.get(clazz)
    }
    val ab = new ArrayBuffer[Field]()
    clazz.getDeclaredFields.foreach(ab += _)
    var parent = clazz.getSuperclass
    while (parent != null) {
      parent.getDeclaredFields.foreach(ab += _)
      parent = parent.getSuperclass
    }
    val ret = ab.filter(f => f.getAnnotation(classOf[JsonIgnore]) == null).toArray
    fieldCache.put(clazz, ret)
    ret
  }

  def getValidMethod(clazz: Class[_]): Array[Method] = {
    if (methodCache.contains(clazz)) {
      return methodCache.get(clazz)
    }
    val ab = new ArrayBuffer[Method]()
    clazz.getDeclaredMethods.foreach(ab += _)
    var parent = clazz.getSuperclass
    while (parent != null) {
      parent.getDeclaredMethods.foreach(ab += _)
      parent = parent.getSuperclass
    }
    val ret = ab.toArray
    methodCache.put(clazz, ret)
    ret
  }

  def escapeString(str: String): String = {
    """["\\\r\n\t]""".r.replaceAllIn(str, word => {
      word.group(0) match {
        case "\"" => "\\\\\""
        case "\\" => "\\\\\\\\"
        case "\r" => "\\\\r"
        case "\n" => "\\\\n"
        case "\t" => "\\\\t"
      }
    })
  }

  val re: Pattern = Pattern.compile("[0-9|a-fA-F]{4}")

  // 返回处理到的pos位置
  def unescapeString(sb: StringBuilder, json: String, pos: Int): Int = {
    json(pos + 1) match {
      case '"' =>
        sb.append('"')
        pos + 1
      case '\\' =>
        sb.append('\\')
        pos + 1
      case 'r' =>
        sb.append('\r')
        pos + 1
      case 'n' =>
        sb.append('\n')
        pos + 1
      case 't' =>
        sb.append('\t')
        pos + 1
      case 'u' =>
        if (json.length >= pos + 6) {
          val sub = json.substring(pos + 2, pos + 6)
          if (re.matcher(sub).matches()) {
            sb.append(Integer.parseInt(sub, 16).toChar)
            pos + 5
          } else {
            sb.append("\\u")
            pos + 1
          }
        } else {
          sb.append("\\u")
          pos + 1
        }
      case c =>
        sb.append("\\").append(c)
        pos + 1
    }
  }
}
