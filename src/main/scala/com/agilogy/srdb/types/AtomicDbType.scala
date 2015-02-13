package com.agilogy.srdb.types

import java.sql.{ResultSet, PreparedStatement}

sealed trait AtomicDbType[T] extends PositionalDbType[T] with DbTypeWithNameAccess[T] {
  final val length = 1
  val jdbcTypes: Seq[JdbcType]
}

trait AtomicDbTypeImplicits {

  implicit def notNull[T: UnsafeAtomicDbType]: AtomicDbType[T] = new AtomicDbType[T] {

    private val udbt = implicitly[UnsafeAtomicDbType[T]]

    override val jdbcTypes: Seq[JdbcType] = udbt.jdbcTypes

    override def set(ps: PreparedStatement, pos: Int, value: T): Unit = {
      if (value == null) throw new IllegalArgumentException("Can't set null")
      udbt.unsafeSet(ps, pos, value)
    }

    override def get(rs: ResultSet, pos: Int): T = udbt.get(rs, pos).getOrElse(throw new NullColumnReadException())

    override def get(rs: ResultSet, name: String): T = udbt.get(rs,name).getOrElse(throw new NullColumnReadException())
  }

  implicit def optional[T: UnsafeAtomicDbType]: AtomicDbType[Option[T]] = new AtomicDbType[Option[T]] {

    lazy val tDbType = implicitly[UnsafeAtomicDbType[T]]

    override def set(ps: PreparedStatement, pos: Int, value: Option[T]): Unit = {
      value match {
        case Some(t) => tDbType.unsafeSet(ps, pos, t)
        case None => ps.setNull(pos, tDbType.jdbcTypes.head.code)
      }

    }

    override def get(rs: ResultSet, pos: Int): Option[T] = tDbType.get(rs, pos)

    override val jdbcTypes: Seq[JdbcType] = tDbType.jdbcTypes

    override def get(rs: ResultSet, name: String): Option[T] = tDbType.get(rs,name)
  }

}