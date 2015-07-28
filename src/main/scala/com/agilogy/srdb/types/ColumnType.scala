package com.agilogy.srdb.types

import java.sql.{ Timestamp, PreparedStatement, ResultSet }
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

/**
 * An type class representing an atomic database type
 *
 * A ColumnType is a mapping between a Scala type `T` and a database column type
 *
 * @tparam T The Scala type mapped to the database type
 * @group API
 */
sealed trait ColumnType[T] {
  self =>

  protected[types] def toJdbc(v: T): AnyRef

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

    protected[types] override def toJdbc(v: T2): AnyRef = self.toJdbc(xf(v))

    protected[types] override def unsafeSet(ps: PreparedStatement, pos: Int, value: T2): Unit = self.unsafeSet(ps, pos, xf(value))

    // TODO: We should be able to map from a primitive to another one
    // Therefore, we should remove this null.asInstanceOf[T2]
    protected[types] override def unsafeGet(rs: ResultSet, pos: Int): T2 = {
      val v = self.unsafeGet(rs, pos)
      if (v == null) null.asInstanceOf[T2] else f(v)
    }

    // TODO: We should be able to map from a primitive to another one
    // Therefore, we should remove this null.asInstanceOf[T2]
    protected[types] override def unsafeGet(rs: ResultSet, name: String): T2 = {
      val v = self.unsafeGet(rs, name)
      if (v == null) null.asInstanceOf[T2] else f(v)
    }

    override val jdbcTypes: Seq[JdbcType] = self.jdbcTypes

  }

}

/** @group API */
object ColumnType {

  def apply[T: ColumnType]: ColumnType[T] = implicitly[ColumnType[T]]

  def from[T](
    toJdbcArg: (T) => AnyRef,
    uset: (PreparedStatement, Int, T) => Unit,
    ugetp: (ResultSet, Int) => T,
    ugetn: (ResultSet, String) => T,
    inJdbcTypes: JdbcType*
  ): ColumnType[T] = new ColumnType[T] {

    require(inJdbcTypes.nonEmpty)

    override protected[types] def toJdbc(v: T): AnyRef = toJdbcArg(v)

    override def unsafeSet(ps: PreparedStatement, pos: Int, value: T): Unit = uset(ps, pos, value)

    override def unsafeGet(rs: ResultSet, pos: Int): T = ugetp(rs, pos)

    override def unsafeGet(rs: ResultSet, name: String): T = ugetn(rs, name)

    override val jdbcTypes: Seq[JdbcType] = inJdbcTypes
  }

}

/**
 * Available instances of [[ColumnType]]
 * @group Column type instances
 */
trait ColumnTypeInstances {

  private val wrarp: (AnyVal) => AnyRef = (v: AnyVal) => v.asInstanceOf[AnyRef]

  /** @group Column type instances */
  implicit val DbByte: ColumnType[Byte] = ColumnType.from[Byte](wrarp, _.setByte(_, _), _.getByte(_: Int), _.getByte(_: String), JdbcType.TinyInt)
  /** @group Column type instances */
  implicit val DbShort = ColumnType.from[Short](wrarp, _.setShort(_, _), _.getShort(_: Int), _.getShort(_: String), JdbcType.SmallInt)
  /** @group Column type instances */
  implicit val DbInt = ColumnType.from[Int](wrarp, _.setInt(_, _), _.getInt(_: Int), _.getInt(_: String), JdbcType.Integer)
  /** @group Column type instances */
  implicit val DbLong = ColumnType.from[Long](wrarp, _.setLong(_, _), _.getLong(_: Int), _.getLong(_: String), JdbcType.BigInt)

  /** @group Column type instances */
  implicit val DbFloat = ColumnType.from[Float](wrarp, _.setFloat(_, _), _.getFloat(_: Int), _.getFloat(_: String), JdbcType.Float)
  /** @group Column type instances */
  implicit val DbDouble = ColumnType.from[Double](wrarp, _.setDouble(_, _), _.getDouble(_: Int), _.getDouble(_: String), JdbcType.Double)

  /** @group Column type instances */
  implicit val DbString = ColumnType.from[String](identity, _.setString(_, _), _.getString(_: Int), _.getString(_: String), JdbcType.Varchar, JdbcType.Char, JdbcType.LongVarchar)

  /** @group Column type instances */
  implicit val DbBoolean = ColumnType.from[Boolean](wrarp, _.setBoolean(_, _), _.getBoolean(_: Int), _.getBoolean(_: String), JdbcType.Boolean)

  private def toSqlDate(d: java.util.Date) = new java.sql.Date(d.getTime)

  private def toSqlTimestamp(d: java.util.Date) = new Timestamp(d.getTime)

  /** @group Column type instances */
  val DbDate = ColumnType.from[java.util.Date](
    toSqlDate,
    (ps, pos, v) => ps.setDate(pos, toSqlDate(v)),
    _.getDate(_: Int),
    _.getDate(_: String),
    JdbcType.Date
  )

  /** @group Column type instances */
  implicit val DbTimestamp: ColumnType[Date] = ColumnType.from[java.util.Date](
    toSqlTimestamp,
    (ps, pos, v) => ps.setTimestamp(pos, toSqlTimestamp(v)),
    { (rs, pos) =>
      val ts = rs.getTimestamp(pos)
      if (ts == null) null
      else new java.util.Date(ts.getTime)
    },
    { (rs, name) =>
      val ts = rs.getTimestamp(name)
      if (ts == null) null
      else new java.util.Date(ts.getTime)
    },
    JdbcType.Timestamp
  )

  /** @group Column type instances */
  implicit val DbBigDecimal = ColumnType.from[BigDecimal](
    _.bigDecimal,
    (ps, pos, v) => ps.setBigDecimal(pos, v.bigDecimal), { (rs, pos) =>
      val jbd: java.math.BigDecimal = rs.getBigDecimal(pos)
      if (jbd == null) null
      else jbd
    }, { (rs, name) =>
      val jbd: java.math.BigDecimal = rs.getBigDecimal(name)
      if (jbd == null) null
      else jbd
    },
    JdbcType.Numeric
  )

  /** @group Column type instances */
  implicit val DbBigInt = ColumnType.from[BigInt](
    v => BigDecimal(v).bigDecimal,
    (ps, pos, v) => ps.setBigDecimal(pos, BigDecimal(v).bigDecimal), { (rs, pos) =>
      val bd = rs.getBigDecimal(pos)
      if (bd == null) null
      else bd.toBigIntegerExact
    }, { (rs, name) =>
      val bd = rs.getBigDecimal(name)
      if (bd == null) null
      else bd.toBigIntegerExact
    }, JdbcType.Numeric
  )

  /** @group Column type instances */
  def arrayDbType[T: ColumnType: ClassTag](databaseTypeName: String): ColumnType[Seq[T]] = new ColumnType[Seq[T]] {

    val columnType = implicitly[ColumnType[T]]
    val reader = implicitly[DbType[T]]

    override protected[types] def toJdbc(arrayValue: Seq[T]): Array[AnyRef] = Array(arrayValue.map(v => columnType.toJdbc(v).asInstanceOf[AnyRef]): _*)

    override protected[types] def unsafeSet(ps: PreparedStatement, pos: Int, value: Seq[T]): Unit = {
      val a = toJdbc(value)
      ps.setArray(pos, ps.getConnection.createArrayOf(databaseTypeName, a))
    }

    private def readArray(a: java.sql.Array): Seq[T] = {
      if (a == null) null
      else {
        val arrayRs = a.getResultSet
        val res = ListBuffer[T]()
        while (arrayRs.next()) {
          res.append(reader.get(arrayRs, 2))
        }
        res
      }
    }

    override protected[types] def unsafeGet(rs: ResultSet, pos: Int): Seq[T] = readArray(rs.getArray(pos))

    override protected[types] def unsafeGet(rs: ResultSet, name: String): Seq[T] = readArray(rs.getArray(name))

    override val jdbcTypes: Seq[JdbcType] = Seq(JdbcType.Array)
  }

}

