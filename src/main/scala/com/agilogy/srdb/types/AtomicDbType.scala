package com.agilogy.srdb.types

import java.sql.{ResultSet, PreparedStatement}

sealed trait TypeMap[T] {
  def unsafeSet(ps: PreparedStatement, pos: Int, value: T): Unit

  def unsafeGet(rs: ResultSet, pos: Int): T

  def unsafeGet(rs: ResultSet, name: String): T

  val jdbcTypes: Seq[JdbcType]

  def get(rs: ResultSet, pos: Int): Option[T] = {
    val res = unsafeGet(rs, pos)
    if (rs.wasNull()) None else Some(res)
  }

  def get(rs: ResultSet, name: String): Option[T] = {
    val res = unsafeGet(rs, name)
    if (rs.wasNull()) None else Some(res)
  }

}

sealed trait AtomicDbType[T] extends DbType[T] {
  self =>

  final val length = 1

  def apply(name:String):NamedDbReader[T] =  NamedDbReader[T](this,name)

}

case class NotNullAtomicDbType[T](optional:OptionalDbType[T]) extends AtomicDbType[T] {

  override def set(ps: PreparedStatement, pos: Int, value: T): Unit = {
    if (value == null) throw new IllegalArgumentException("Can't set null")
    optional.unsafeSet(ps, pos, value)
  }

  override def get(rs: ResultSet, pos: Int): T = optional.get(rs, pos).getOrElse(throw new NullColumnReadException())

  override def get(rs: ResultSet, name: String): T = optional.get(rs, name).getOrElse(throw new NullColumnReadException())

}

trait OptionalDbType[T] extends AtomicDbType[Option[T]] with TypeMap[T] {
  
  def notNull: NotNullAtomicDbType[T] = NotNullAtomicDbType(this)
  
  def set(ps: PreparedStatement, pos: Int, value: Option[T]): Unit = {
    value match {
      case Some(t) => unsafeSet(ps, pos, t)
      case None => ps.setNull(pos, jdbcTypes.head.code)
    }

  }

}

object OptionalDbType{
  private[types] def apply[T](
                               uset: (PreparedStatement, Int, T) => Unit,
                               ugetp: (ResultSet, Int) => T,
                               ugetn: (ResultSet, String) => T,
                               inJdbcTypes: JdbcType*): OptionalDbType[T] = new OptionalDbType[T] {

    require(inJdbcTypes.nonEmpty)

    override def unsafeSet(ps: PreparedStatement, pos: Int, value: T): Unit = uset(ps, pos, value)

    override def unsafeGet(rs: ResultSet, pos: Int): T = ugetp(rs, pos)

    override def unsafeGet(rs: ResultSet, name: String): T = ugetn(rs, name)

    override val jdbcTypes: Seq[JdbcType] = inJdbcTypes
  }

}


trait AtomicDbTypeImplicits {

  implicit def notNull[T: OptionalDbType]: AtomicDbType[T] = new NotNullAtomicDbType[T](optional[T])
  
  def optional[T:OptionalDbType]:OptionalDbType[T] = implicitly[OptionalDbType[T]]

  implicit val DbByte = OptionalDbType[Byte](_.setByte(_, _), _.getByte(_: Int), _.getByte(_: String), JdbcType.TinyInt)
  implicit val DbShort = OptionalDbType[Short](_.setShort(_, _), _.getShort(_: Int), _.getShort(_: String), JdbcType.SmallInt)
  implicit val DbInt = OptionalDbType[Int](_.setInt(_, _), _.getInt(_: Int), _.getInt(_: String), JdbcType.Integer)
  implicit val DbLong = OptionalDbType[Long](_.setLong(_, _), _.getLong(_: Int), _.getLong(_: String), JdbcType.BigInt)

  implicit val DbFloat = OptionalDbType[Float](_.setFloat(_, _), _.getFloat(_: Int), _.getFloat(_: String), JdbcType.BigInt)
  implicit val DbDouble = OptionalDbType[Double](_.setDouble(_, _), _.getDouble(_: Int), _.getDouble(_: String), JdbcType.BigInt)

  implicit val DbString = OptionalDbType[String](_.setString(_, _), _.getString(_: Int), _.getString(_: String), JdbcType.Varchar, JdbcType.Char, JdbcType.LongVarchar)

  implicit val DbBoolean = OptionalDbType[Boolean](_.setBoolean(_, _), _.getBoolean(_: Int), _.getBoolean(_: String), JdbcType.Boolean)

  private def toSqlDate(d: java.util.Date) = new java.sql.Date(d.getTime)

  implicit val DbDate = OptionalDbType[java.util.Date](
    (ps, pos, v) => ps.setDate(pos, toSqlDate(v)),
    _.getDate(_: Int),
    _.getDate(_: String), JdbcType.Date)

  implicit val DbBigDecimal = OptionalDbType[BigDecimal](
    (ps, pos, v) => ps.setBigDecimal(pos, v.bigDecimal),
    _.getBigDecimal(_: Int),
    _.getBigDecimal(_: String), JdbcType.BigDecimal)

}

