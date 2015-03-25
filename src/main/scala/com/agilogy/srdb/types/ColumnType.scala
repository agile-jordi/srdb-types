package com.agilogy.srdb.types

import java.sql.{PreparedStatement, ResultSet}
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

sealed trait ColumnType[T] {
  self =>

  protected[types] def unsafeSet(ps: PreparedStatement, pos: Int, value: T): Unit

  protected[types] def unsafeGet(rs: ResultSet, pos: Int): T

  protected[types] def unsafeGet(rs: ResultSet, name: String): T

  val jdbcTypes: Seq[JdbcType]

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

  def xmap[T2](f: T => T2, xf: T2 => T): ColumnType[T2] = new ColumnType[T2] {

    override def unsafeSet(ps: PreparedStatement, pos: Int, value: T2): Unit = self.unsafeSet(ps, pos, xf(value))

    override def unsafeGet(rs: ResultSet, pos: Int): T2 = f(self.unsafeGet(rs, pos))

    override def unsafeGet(rs: ResultSet, name: String): T2 = f(self.unsafeGet(rs, name))

    override val jdbcTypes: Seq[JdbcType] = self.jdbcTypes

  }

}

object ColumnType {

  def apply[T: ColumnType]: ColumnType[T] = implicitly[ColumnType[T]]

  private[types] def from[T](
                              uset: (PreparedStatement, Int, T) => Unit,
                              ugetp: (ResultSet, Int) => T,
                              ugetn: (ResultSet, String) => T,
                              inJdbcTypes: JdbcType*): ColumnType[T] = new ColumnType[T] {

    require(inJdbcTypes.nonEmpty)

    override def unsafeSet(ps: PreparedStatement, pos: Int, value: T): Unit = uset(ps, pos, value)

    override def unsafeGet(rs: ResultSet, pos: Int): T = ugetp(rs, pos)

    override def unsafeGet(rs: ResultSet, name: String): T = ugetn(rs, name)

    override val jdbcTypes: Seq[JdbcType] = inJdbcTypes
  }

}


trait ColumnTypeInstances {

  implicit val DbByte: ColumnType[Byte] = ColumnType.from[Byte](_.setByte(_, _), _.getByte(_: Int), _.getByte(_: String), JdbcType.TinyInt)
  implicit val DbShort = ColumnType.from[Short](_.setShort(_, _), _.getShort(_: Int), _.getShort(_: String), JdbcType.SmallInt)
  implicit val DbInt = ColumnType.from[Int](_.setInt(_, _), _.getInt(_: Int), _.getInt(_: String), JdbcType.Integer)
  implicit val DbLong = ColumnType.from[Long](_.setLong(_, _), _.getLong(_: Int), _.getLong(_: String), JdbcType.BigInt)

  implicit val DbFloat = ColumnType.from[Float](_.setFloat(_, _), _.getFloat(_: Int), _.getFloat(_: String), JdbcType.Float)
  implicit val DbDouble = ColumnType.from[Double](_.setDouble(_, _), _.getDouble(_: Int), _.getDouble(_: String), JdbcType.Double)

  implicit val DbString = ColumnType.from[String](_.setString(_, _), _.getString(_: Int), _.getString(_: String), JdbcType.Varchar, JdbcType.Char, JdbcType.LongVarchar)

  implicit val DbBoolean = ColumnType.from[Boolean](_.setBoolean(_, _), _.getBoolean(_: Int), _.getBoolean(_: String), JdbcType.Boolean)

  private def toSqlDate(d: java.util.Date) = new java.sql.Date(d.getTime)

  implicit val DbDate = ColumnType.from[java.util.Date](
    (ps, pos, v) => ps.setDate(pos, toSqlDate(v)),
    _.getDate(_: Int),
    _.getDate(_: String), JdbcType.Date)

  implicit val DbBigDecimal = ColumnType.from[BigDecimal](
    (ps, pos, v) => ps.setBigDecimal(pos, v.bigDecimal),
    _.getBigDecimal(_: Int),
    _.getBigDecimal(_: String), JdbcType.Numeric)

  def arrayDbType[T: ColumnType : ClassTag](databaseTypeName: String) = new ColumnType[Seq[T]] {

    val simpleType = implicitly[ColumnType[T]]

    override protected[types] def unsafeSet(ps: PreparedStatement, pos: Int, value: Seq[T]): Unit = {
      val a = Array(value.map(_.asInstanceOf[AnyRef]): _*)
      ps.setArray(pos, ps.getConnection.createArrayOf(databaseTypeName, a))
    }

    private def readArray(a: java.sql.Array): Seq[T] = {
      val arrayRs = a.getResultSet
      val res = ListBuffer[T]()
      while (arrayRs.next()) {
        res.append(simpleType.get(arrayRs, 1).get)
      }
      res
    }

    override protected[types] def unsafeGet(rs: ResultSet, pos: Int): Seq[T] = readArray(rs.getArray(pos))

    override protected[types] def unsafeGet(rs: ResultSet, name: String): Seq[T] = readArray(rs.getArray(name))

    override val jdbcTypes: Seq[JdbcType] = Seq(JdbcType.Array)
  }


}

