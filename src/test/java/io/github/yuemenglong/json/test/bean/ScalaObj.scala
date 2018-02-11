package io.github.yuemenglong.json.test.bean

import io.github.yuemenglong.json.lang.JsonIgnore

/**
  * Created by <yuemenglong@126.com> on 2017/7/26.
  */
class ScalaObj {
  var id: Integer = _
  var name: String = _
  var map: Map[String, ScalaObj] = Map()
  @JsonIgnore
  var ign: String = _
}
