package com.agilogy.srdb.types

import java.sql.{ResultSet, PreparedStatement}

sealed trait ColumnType[T] {
  self =>

  def get(rs: ResultSet, name: String): T

  def get(rs: ResultSet, pos: Int): T

  def set(ps: PreparedStatement, pos: Int, value: T): Unit

  def apply(name: String): NamedDbReader[T] = new NamedDbReader[T] {
    override def get(rs: ResultSet): T = self.get(rs,name)
  }

}

object ColumnType{

  def apply[T:ColumnType](name:String):NamedDbReader[T] = implicitly[ColumnType[T]].apply(name)
}

case class NotNullColumnType[T](optional: OptionalColumnType[T]) extends ColumnType[T] {

  override def set(ps: PreparedStatement, pos: Int, value: T): Unit = {
    if (value == null) throw new IllegalArgumentException("Can't set null")
    optional.unsafeSet(ps, pos, value)
  }

  override def get(rs: ResultSet, pos: Int): T = optional.get(rs, pos).getOrElse(throw new NullColumnReadException())

  override def get(rs: ResultSet, name: String): T = optional.get(rs, name).getOrElse(throw new NullColumnReadException())

  def xmap[T2](f: (T) => T2, xf: (T2) => T): NotNullColumnType[T2] = this.copy(optional.xmap(f, xf))
}

trait OptionalColumnType[T] extends ColumnType[Option[T]] {

  self =>

  protected[types] def unsafeSet(ps: PreparedStatement, pos: Int, value: T): Unit

  protected[types] def unsafeGet(rs: ResultSet, pos: Int): T

  protected[types] def unsafeGet(rs: ResultSet, name: String): T

  protected val jdbcTypes: Seq[JdbcType]

  def get(rs: ResultSet, pos: Int): Option[T] = {
    val res = unsafeGet(rs, pos)
    if (rs.wasNull()) None else Some(res)
  }

  def get(rs: ResultSet, name: String): Option[T] = {
    val res = unsafeGet(rs, name)
    if (rs.wasNull()) None else Some(res)
  }

  def set(ps: PreparedStatement, pos: Int, value: Option[T]): Unit = {
    value match {
      case Some(t) => unsafeSet(ps, pos, t)
      case None => ps.setNull(pos, jdbcTypes.head.code)
    }
  }

  def notNull: NotNullColumnType[T] = NotNullColumnType(this)

  def xmap[T2](f: T => T2, xf: T2 => T): OptionalColumnType[T2] = new OptionalColumnType[T2] {

    override def unsafeSet(ps: PreparedStatement, pos: Int, value: T2): Unit = self.unsafeSet(ps, pos, xf(value))

    override def unsafeGet(rs: ResultSet, pos: Int): T2 = f(self.unsafeGet(rs, pos))

    override def unsafeGet(rs: ResultSet, name: String): T2 = f(self.unsafeGet(rs, name))

    override val jdbcTypes: Seq[JdbcType] = self.jdbcTypes

  }
}

object OptionalColumnType {
  private[types] def apply[T](
                               uset: (PreparedStatement, Int, T) => Unit,
                               ugetp: (ResultSet, Int) => T,
                               ugetn: (ResultSet, String) => T,
                               inJdbcTypes: JdbcType*): OptionalColumnType[T] = new OptionalColumnType[T] {

    require(inJdbcTypes.nonEmpty)

    override def unsafeSet(ps: PreparedStatement, pos: Int, value: T): Unit = uset(ps, pos, value)

    override def unsafeGet(rs: ResultSet, pos: Int): T = ugetp(rs, pos)

    override def unsafeGet(rs: ResultSet, name: String): T = ugetn(rs, name)

    override val jdbcTypes: Seq[JdbcType] = inJdbcTypes
  }

}


trait ColumnTypeInstances {

  implicit def notNull[T: OptionalColumnType]: NotNullColumnType[T] = new NotNullColumnType[T](optional[T])
  def notNull[T: OptionalColumnType](name:String): NamedDbReader[T] = new NotNullColumnType[T](optional[T]).apply(name)

  def optional[T: OptionalColumnType]: OptionalColumnType[T] = implicitly[OptionalColumnType[T]]
  def optional[T: OptionalColumnType](name:String): NamedDbReader[Option[T]] = implicitly[OptionalColumnType[T]].apply(name)

  implicit val DbByte = OptionalColumnType[Byte](_.setByte(_, _), _.getByte(_: Int), _.getByte(_: String), JdbcType.TinyInt)
  implicit val DbShort = OptionalColumnType[Short](_.setShort(_, _), _.getShort(_: Int), _.getShort(_: String), JdbcType.SmallInt)
  implicit val DbInt = OptionalColumnType[Int](_.setInt(_, _), _.getInt(_: Int), _.getInt(_: String), JdbcType.Integer)
  implicit val DbLong = OptionalColumnType[Long](_.setLong(_, _), _.getLong(_: Int), _.getLong(_: String), JdbcType.BigInt)

  implicit val DbFloat = OptionalColumnType[Float](_.setFloat(_, _), _.getFloat(_: Int), _.getFloat(_: String), JdbcType.BigInt)
  implicit val DbDouble = OptionalColumnType[Double](_.setDouble(_, _), _.getDouble(_: Int), _.getDouble(_: String), JdbcType.BigInt)

  implicit val DbString = OptionalColumnType[String](_.setString(_, _), _.getString(_: Int), _.getString(_: String), JdbcType.Varchar, JdbcType.Char, JdbcType.LongVarchar)

  implicit val DbBoolean = OptionalColumnType[Boolean](_.setBoolean(_, _), _.getBoolean(_: Int), _.getBoolean(_: String), JdbcType.Boolean)

  private def toSqlDate(d: java.util.Date) = new java.sql.Date(d.getTime)

  implicit val DbDate = OptionalColumnType[java.util.Date](
    (ps, pos, v) => ps.setDate(pos, toSqlDate(v)),
    _.getDate(_: Int),
    _.getDate(_: String), JdbcType.Date)

  implicit val DbBigDecimal = OptionalColumnType[BigDecimal](
    (ps, pos, v) => ps.setBigDecimal(pos, v.bigDecimal),
    _.getBigDecimal(_: Int),
    _.getBigDecimal(_: String), JdbcType.BigDecimal)

}

