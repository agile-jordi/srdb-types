package com.agilogy.srdb.types

import java.sql.{ResultSet, PreparedStatement}

trait DbType[T] extends DbReader[T] with DbWriter[T]{
  self =>

  def xmap[T2](f:T => T2, xf:T2 => T): DbType[T2] = new DbType[T2] {

    val reader = self.map(f)

    val writer = self.contramap(xf)

    override def get(rs: ResultSet): T2 = reader.get(rs)

    override def get(rs: ResultSet, pos: Int): T2 = reader.get(rs,pos)

    override def get(rs: ResultSet, name: String): T2 = reader.get(rs,name)

    override def set(ps: PreparedStatement, pos: Int, value: T2): Unit = writer.set(ps,pos,value)

    override val length: Int = self.length
  }

}

trait DbTypeImplicits extends DbTypeCombinators{
   
}
