package yy;

import org.junit.Assert;
import org.junit.Test;
import yy.bean.Obj;
import yy.bean.ScalaObj;
import yy.json.JSON;
import yy.json.parse.Convert;
import yy.json.parse.JsonLong;
import yy.json.parse.JsonNode;
import yy.json.parse.JsonObj;

/**
 * Created by <yuemenglong@126.com> on 2017/7/12.
 */
public class SimpleTest {
    @Test
    public void testConvert() {
        Obj obj = new Obj();
        obj.setIntValue(1);
        obj.setLongValue(2L);
        obj.setDoubleValue(3.4);
        obj.setStringValue("str");
        obj.setBooleanValue(true);

        obj.setObjValue(new Obj());
        obj.getObjValue().setIntValue(10);
        obj.getObjValue().setLongValue(20L);

        obj.setObjs(new Obj[]{new Obj(), new Obj()});
        obj.getObjs()[0].setStringValue("obj0");
        obj.getObjs()[1].setStringValue("obj1");

        JsonObj root = Convert.fromObject(obj);
        String s1 = root.toString();
        Obj obj2 = (Obj) Convert.toObject(root, Obj.class);
        root = Convert.fromObject(obj2);
        String s2 = root.toString();
        Assert.assertEquals(s1, s2);
    }

    @Test
    public void testApi() {
        Obj obj = new Obj();
        obj.setIntValue(1);
        obj.setLongValue(2L);
        obj.setDoubleValue(3.4);
        obj.setStringValue("str");
        obj.setBooleanValue(true);

        obj.setObjValue(new Obj());
        obj.getObjValue().setIntValue(10);
        obj.getObjValue().setLongValue(20L);

        obj.setObjs(new Obj[]{new Obj(), new Obj()});
        obj.getObjs()[0].setStringValue("obj0");
        obj.getObjs()[1].setStringValue("obj1");

        String s1 = JSON.stringify(obj);
        Obj o2 = JSON.parse(s1, Obj.class);
        String s2 = JSON.stringify(o2);
        Assert.assertEquals(s1, s2);

        String s3 = JSON.stringify(o2.getObjs());
        Obj[] os = JSON.parse(s3, Obj[].class);
        String s4 = JSON.stringify(os);
        Assert.assertEquals(s3, s4);
    }

    @Test
    public void testIgnoreNull() {
        Obj obj = new Obj();
        obj.setIntValue(1);
        String s1 = JSON.stringify(obj);
        String s2 = JSON.stringify(obj, true);
        Assert.assertEquals(s1, "{\"intValue\":1}");
        Assert.assertNotEquals(s1, s2);
        JsonObj root = JSON.convert(obj).asObj();
        Assert.assertEquals(root.getInt("intValue").intValue(), 1);
    }

    @Test
    public void testPlain() {
        Obj obj = new Obj();
        obj.setIntValue(1);
        obj.setObjs(new Obj[]{new Obj()});
        obj.getObjs()[0].setIntValue(100);
        JsonObj root = JSON.convert(obj).asObj();
        String plain = root.get("objs").toString();
        root.setPlain("objs", plain);
        Assert.assertEquals(root.toString(), JSON.stringify(obj));
    }

    @Test
    public void testGetFieldWhenNull() {
        JsonObj obj = JSON.obj();
        String s = obj.getStr("a");
        Assert.assertEquals(s, null);
    }

    @Test
    public void testWalk() {
        JsonObj obj = JSON.obj();
        obj.setLong("a", 1L);
        obj.setLong("b", 2L);
        obj.setLong("c", 3L);
        obj = JSON.walk(obj, (node) -> {
            if (!(node instanceof JsonLong)) {
                return node;
            }
            JsonLong n = (JsonLong) node;
            if (n.value() == 3) {
                return new JsonLong(-3);
            } else {
                return node;
            }
        }).asObj();
        Assert.assertEquals(obj.getLong("a").longValue(), 1L);
        Assert.assertEquals(obj.getLong("b").longValue(), 2L);
        Assert.assertEquals(obj.getLong("c").longValue(), -3L);
    }

    @Test
    public void testEscape() {
        Obj obj = new Obj();
        obj.setStringValue("\"name\"");
        JsonObj jo = JSON.convert(obj).asObj();
        Assert.assertEquals(jo.getStr("stringValue"), "\"name\"");
        Assert.assertEquals(jo.toString(), "{\"stringValue\":\"\\\"name\\\"\"}");
    }

}
