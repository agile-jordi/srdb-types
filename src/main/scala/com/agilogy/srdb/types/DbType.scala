package com.agilogy.srdb.types

import java.sql.{ResultSet, PreparedStatement}

sealed trait DbReader[T] {

  def get(rs: ResultSet): T
  def get(rs: ResultSet, name: String): T

}

sealed trait DbType[T] extends DbReader[T] {

  def set(ps: PreparedStatement, pos: Int, value: T): Unit

  val length: Int
}

case class NamedDbType[T:DbType](as:String) extends DbType[T] {

  private val dbType = implicitly[DbType[T]]

  def get(rs: ResultSet): T = get(rs, "")

  def get(rs: ResultSet, name: String): T = dbType.get(rs, as + name)
  
  override def set(ps: PreparedStatement, pos: Int, value: T): Unit = dbType.set(ps,pos,value)

  override val length: Int = dbType.length
}

trait PositionalDbType[T] extends DbType[T] {
  def get(rs: ResultSet): T = get(rs, 1)

  def get(rs: ResultSet, pos: Int): T
}

object DbType extends AtomicDbTypeImplicits {

  implicit def combine[T1: PositionalDbType, T2: PositionalDbType]: PositionalDbType[(T1, T2)] = new PositionalDbType[(T1, T2)] {

    val t1 = implicitly[PositionalDbType[T1]]
    val t2 = implicitly[PositionalDbType[T2]]

    override val length: Int = 2

    override def set(ps: PreparedStatement, pos: Int, value: (T1, T2)): Unit = {
      t1.set(ps, pos, value._1)
      t2.set(ps, pos + t1.length, value._2)
    }

    override def get(rs: ResultSet, pos: Int): (T1, T2) = (t1.get(rs, pos), t2.get(rs, pos + t1.length))

    override def get(rs: ResultSet, name: String): (T1, T2) = (t1.get(rs,name), t2.get(rs,name))
  }

}

