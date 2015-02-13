package com.agilogy.srdb.types

import java.sql.{ResultSet, PreparedStatement}

sealed trait DbReader[T] {

  def get(rs: ResultSet): T

}

sealed trait DbType[T] extends DbReader[T] {

  def set(ps: PreparedStatement, pos: Int, value: T): Unit

  val length: Int
}

trait DbTypeWithNameAccess[T] extends DbType[T]{
  self =>
  
  def get(rs: ResultSet, name: String): T

  def xmap[T2](f:T => T2, xf:T2 => T): DbTypeWithNameAccess[T2] = new DbTypeWithNameAccess[T2] {
    
    override def get(rs: ResultSet, name: String): T2 = f(self.get(rs,name))

    override def set(ps: PreparedStatement, pos: Int, value: T2): Unit = self.set(ps,pos,xf(value))

    override val length: Int = self.length

    override def get(rs: ResultSet): T2 = f(self.get(rs))
  }
}

case class NamedDbType[T](dbType:DbTypeWithNameAccess[T], as:String) extends DbTypeWithNameAccess[T] {

  def get(rs: ResultSet): T = get(rs, "")

  def get(rs: ResultSet, name: String): T = dbType.get(rs, as + name)
  
  override def set(ps: PreparedStatement, pos: Int, value: T): Unit = dbType.set(ps,pos,value)

  override val length: Int = dbType.length

}

trait PositionalDbType[T] extends DbType[T] {
  self =>
  def get(rs: ResultSet): T = get(rs, 1)

  def get(rs: ResultSet, pos: Int): T

  def xmap[T2](f:T => T2, xf:T2 => T): PositionalDbType[T2] = new PositionalDbType[T2]{
    
    override def get(rs: ResultSet, pos: Int): T2 = f(self.get(rs,pos))

    override def set(ps: PreparedStatement, pos: Int, value: T2): Unit = self.set(ps,pos,xf(value))

    override val length: Int = self.length
  }
}

object DbType extends AtomicDbTypeImplicits {

  implicit def combine[T1: PositionalDbType, T2: PositionalDbType]: PositionalDbType[(T1, T2)] = new PositionalDbType[(T1, T2)] {

    val t1 = implicitly[PositionalDbType[T1]]
    val t2 = implicitly[PositionalDbType[T2]]

    override val length: Int = t1.length + t2.length
    
    override def set(ps: PreparedStatement, pos: Int, value: (T1, T2)): Unit = {
      t1.set(ps, pos, value._1)
      t2.set(ps, pos + t1.length, value._2)
    }

    override def get(rs: ResultSet, pos: Int): (T1, T2) = (t1.get(rs, pos), t2.get(rs, pos + t1.length))
  }
  
}

