package io.github.yuemenglong.json.test.bean;


import io.github.yuemenglong.json.lang.JsonDate;

import java.math.BigDecimal;
import java.util.Date;

/**
 * Created by Administrator on 2017/7/13.
 */
public class Obj {
    private Integer intValue;
    private Long longValue;
    private Double doubleValue;
    private String stringValue;
    private Boolean booleanValue;
    private Object nullValue;
    private Obj objValue;
    private Obj[] objs;
    private BigDecimal bigDecimal;
    private Date datetime;
    @JsonDate
    private Date date;

    public Integer getIntValue() {
        return intValue;
    }

    public void setIntValue(Integer intValue) {
        this.intValue = intValue;
    }

    public Long getLongValue() {
        return longValue;
    }

    public void setLongValue(Long longValue) {
        this.longValue = longValue;
    }

    public Double getDoubleValue() {
        return doubleValue;
    }

    public void setDoubleValue(Double doubleValue) {
        this.doubleValue = doubleValue;
    }

    public String getStringValue() {
        return stringValue;
    }

    public void setStringValue(String stringValue) {
        this.stringValue = stringValue;
    }

    public Boolean getBooleanValue() {
        return booleanValue;
    }

    public void setBooleanValue(Boolean booleanValue) {
        this.booleanValue = booleanValue;
    }

    public Object getNullValue() {
        return nullValue;
    }

    public void setNullValue(Object nullValue) {
        this.nullValue = nullValue;
    }

    public Obj getObjValue() {
        return objValue;
    }

    public void setObjValue(Obj objValue) {
        this.objValue = objValue;
    }

    public Obj[] getObjs() {
        return objs;
    }

    public void setObjs(Obj[] objs) {
        this.objs = objs;
    }

    public BigDecimal getBigDecimal() {
        return bigDecimal;
    }

    public void setBigDecimal(BigDecimal bigDecimal) {
        this.bigDecimal = bigDecimal;
    }

    public Date getDatetime() {
        return datetime;
    }

    public void setDatetime(Date datetime) {
        this.datetime = datetime;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }
}