package com.agilogy.srdb.test

import java.sql.{ ResultSet, PreparedStatement, Connection }

import com.agilogy.srdb.types._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

case class Id(value: Long)

class ArrayDbTypesTest extends FlatSpec with MockFactory {

  val conn: Connection = mock[Connection]
  val ps: PreparedStatement = mock[PreparedStatement]
  val rs: ResultSet = mock[ResultSet]

  it should "prepare statements with an array param and read resultsets with an array column" in {
    val value = Seq(1, 2)
    val arr = mock[java.sql.Array]
    implicit val intArrayDbType = arrayDbType[Int]("int")
    val arrRs = mock[ResultSet]
    def expectReadArray() = {
      (() => arr.getResultSet).expects().returning(arrRs)
      (arrRs.next _).expects().returning(true)
      (arrRs.getInt(_: Int)).expects(2).returning(1)
      (arrRs.wasNull _).expects().returning(false)
      (arrRs.next _).expects().returning(true)
      (arrRs.getInt(_: Int)).expects(2).returning(2)
      (arrRs.wasNull _).expects().returning(false)
      (arrRs.next _).expects().returning(false)
    }
    inSequence {
      (ps.getConnection _).expects().returning(conn)
      (conn.createArrayOf _).expects(where {
        (typeName: String, elements: scala.Array[AnyRef]) =>
          typeName == "int" && elements.toSeq == value.asInstanceOf[Seq[AnyRef]]
      }).returning(arr)
      (ps.setArray _).expects(1, arr)
      (rs.getArray(_: Int)).expects(1).returning(arr)
      expectReadArray()
      (rs.wasNull _).expects().returning(false)
      (rs.getArray(_: String)).expects("c").returning(arr)
      expectReadArray()
      (rs.wasNull _).expects().returning(false)
    }
    set(ps, value)
    assert(get[Seq[Int]](rs) === value)
    assert(get(rs)(notNull[Seq[Int]]("c")) === value)
  }

  implicit val DbId: ColumnType[Id] = DbLong.xmap[Id](Id.apply, _.value)

  it should "prepare statements with a mapped array param and read resultsets with a mapped array column" in {
    val value = Seq(Id(1l), Id(2l))
    val arr = mock[java.sql.Array]
    implicit val intArrayDbType = arrayDbType[Id]("bigint")
    val arrRs = mock[ResultSet]
    def expectReadArray() = {
      (() => arr.getResultSet).expects().returning(arrRs)
      (arrRs.next _).expects().returning(true)
      (arrRs.getLong(_: Int)).expects(2).returning(1l)
      (arrRs.wasNull _).expects().returning(false)
      (arrRs.next _).expects().returning(true)
      (arrRs.getLong(_: Int)).expects(2).returning(2l)
      (arrRs.wasNull _).expects().returning(false)
      (arrRs.next _).expects().returning(false)
    }
    inSequence {
      (ps.getConnection _).expects().returning(conn)
      (conn.createArrayOf _).expects(where {
        (typeName: String, elements: scala.Array[AnyRef]) =>
          typeName == "bigint" && elements.toSeq == value.map(_.value).asInstanceOf[Seq[AnyRef]]
      }).returning(arr)
      (ps.setArray _).expects(1, arr)
      (rs.getArray(_: Int)).expects(1).returning(arr)
      expectReadArray()
      (rs.wasNull _).expects().returning(false)
      (rs.getArray(_: String)).expects("c").returning(arr)
      expectReadArray()
      (rs.wasNull _).expects().returning(false)
    }
    set(ps, value)
    assert(get[Seq[Id]](rs) === value)
    assert(get(rs)(notNull[Seq[Id]]("c")) === value)
  }

}
