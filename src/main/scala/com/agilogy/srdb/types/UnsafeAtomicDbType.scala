package com.agilogy.srdb.types

import java.sql.{ResultSet, PreparedStatement}

sealed trait UnsafeAtomicDbType[T] {
  def unsafeSet(ps: PreparedStatement,pos: Int,value: T): Unit
  def unsafeGet(rs: ResultSet, pos: Int): T
  def unsafeGet(rs: ResultSet, name: String): T
  val jdbcTypes: Seq[JdbcType]
  def get(rs:ResultSet, pos:Int):Option[T] = {
    val res = unsafeGet(rs,pos)
    if(rs.wasNull()) None else Some(res)
  }
  def get(rs:ResultSet, name:String):Option[T] = {
    val res = unsafeGet(rs,name)
    if(rs.wasNull()) None else Some(res)
  }
}

object UnsafeAtomicDbType{
  
  private def unsafe[T](
                         uset:(PreparedStatement,Int,T) => Unit,
                         ugetp:(ResultSet,Int) => T,
                         ugetn:(ResultSet,String) => T,
                         jdcb: JdbcType*):UnsafeAtomicDbType[T] = new UnsafeAtomicDbType[T] {

    override def unsafeSet(ps: PreparedStatement, pos: Int, value: T): Unit = uset(ps,pos,value)

    override def unsafeGet(rs: ResultSet, pos: Int): T = ugetp(rs,pos)

    override def unsafeGet(rs: ResultSet, name: String): T = ugetn(rs,name)

    override val jdbcTypes: Seq[JdbcType] = jdcb
  }

  implicit val ByteDbType = unsafe[Byte](_.setByte(_,_),_.getByte(_:Int),_.getByte(_:String),JdbcType.TinyInt)
  implicit val ShortDbType = unsafe[Short](_.setShort(_,_),_.getShort(_:Int),_.getShort(_:String),JdbcType.SmallInt)
  implicit val IntDbType = unsafe[Int](_.setInt(_,_),_.getInt(_:Int),_.getInt(_:String),JdbcType.Integer)
  implicit val LongDbType = unsafe[Long](_.setLong(_,_),_.getLong(_:Int),_.getLong(_:String),JdbcType.BigInt)

  implicit val FloatDbType = unsafe[Float](_.setFloat(_,_),_.getFloat(_:Int),_.getFloat(_:String),JdbcType.BigInt)
  implicit val DoubleDbType = unsafe[Double](_.setDouble(_,_),_.getDouble(_:Int),_.getDouble(_:String),JdbcType.BigInt)

  implicit val StringDbType = unsafe[String](_.setString(_,_),_.getString(_:Int),_.getString(_:String),JdbcType.Varchar, JdbcType.Char, JdbcType.LongVarchar)

  implicit val BooleanDbType = unsafe[Boolean](_.setBoolean(_,_),_.getBoolean(_:Int),_.getBoolean(_:String),JdbcType.Boolean)

  private def toSqlDate(d:java.util.Date) = new java.sql.Date(d.getTime)

  implicit val DateDbType = unsafe[java.util.Date](
    (ps,pos,v) => ps.setDate(pos,toSqlDate(v)),
    _.getDate(_:Int),
    _.getDate(_:String),JdbcType.Date)

  implicit val BigDecimalDbType = unsafe[BigDecimal](
    (ps,pos,v) => ps.setBigDecimal(pos,v.bigDecimal),
    _.getBigDecimal(_:Int),
    _.getBigDecimal(_:String),JdbcType.BigDecimal)


}


