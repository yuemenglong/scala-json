package io.github.yuemenglong.json.test;

import org.junit.Assert;
import org.junit.Test;
import io.github.yuemenglong.json.test.bean.Obj;
import io.github.yuemenglong.json.JSON;
import io.github.yuemenglong.json.parse.Convert;
import io.github.yuemenglong.json.parse.JsonLong;
import io.github.yuemenglong.json.parse.JsonObj;

import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

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

    @Test
    public void testEscape2() {
        Obj obj = new Obj();
        obj.setStringValue("\\name\\");
        JsonObj jo = JSON.convert(obj).asObj();
        Assert.assertEquals(jo.getStr("stringValue"), "\\name\\");
        Assert.assertEquals(jo.toString(), "{\"stringValue\":\"\\\\name\\\\\"}");
    }

    @Test
    public void testBigDecimalAndDateTime() throws ParseException {
        {
            String s = "{\"bigDecimal\":1.2,\"datetime\":\"2017-12-12\"}";
            Obj obj = JSON.parse(s, Obj.class);
            Assert.assertEquals(obj.getBigDecimal().doubleValue(), 1.2, 0.0000001);
            Assert.assertEquals(obj.getDatetime(),
                    new SimpleDateFormat("yyyy-MM-dd").parse("2017-12-12"));
        }
        {
            String s = "{\"bigDecimal\":120,\"datetime\":\"2017-12-12 01:01:01\"}";
            Obj obj = JSON.parse(s, Obj.class);
            Assert.assertEquals(obj.getBigDecimal().doubleValue(), 120, 0.00000001);
            Assert.assertEquals(obj.getDatetime(),
                    new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
                            .parse("2017-12-12 01:01:01"));
        }
        {
            Obj obj = new Obj();
            obj.setBigDecimal(new BigDecimal(1.2));
            obj.setDatetime(new Date(2017 - 1900, 12 - 1, 12, 14, 14, 14));
            String json = JSON.stringify(obj);
            Assert.assertEquals(json, "{\"bigDecimal\":1.2,\"datetime\":\"2017-12-12 14:14:14\"}");
        }
    }

    @Test
    public void testDate() throws ParseException {
        Obj obj = new Obj();
        obj.setDate(new Date(2017 - 1900, 12 - 1, 12, 14, 14, 14));
        String json = JSON.stringify(obj);
        Assert.assertEquals(json, "{\"date\":\"2017-12-12\"}");
    }
}
