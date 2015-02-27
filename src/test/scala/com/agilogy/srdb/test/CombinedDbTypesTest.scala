package com.agilogy.srdb.test

import java.sql.{ResultSet, PreparedStatement}

import com.agilogy.srdb.types
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class CombinedDbTypesTest extends FlatSpec with MockFactory{

  val ps = mock[PreparedStatement]
  val rs = mock[ResultSet]
  val db = new DummyDb(ps, rs)

  behavior of "combined positional db type"
  
  import types._
  
  it should "read a ResultSet by position" in{
    inSequence{
      (rs.getString(_:Int)).expects(1).returning("John")
      (rs.wasNull _).expects().returning(false)
      (rs.getInt(_:Int)).expects(2).returning(23)
      (rs.wasNull _).expects().returning(false)
    }
    assert(db.read(dbType[String,Int]) === ("John",23))
  }
  
  it should "set PreparedStatement parameters" in {
    inSequence{
      (ps.setString _).expects(1,"John")
      (ps.setInt _).expects(2,23)
    }
    db.prepare(("John", 23))
  }

  behavior of "combined named db type"

  it should "read a ResultSet by name" in{
    inSequence{
      (rs.getString(_:String)).expects("name").returning("John")
      (rs.wasNull _).expects().returning(false)
      (rs.getInt(_:String)).expects("age").returning(23)
      (rs.wasNull _).expects().returning(false)
    }
    assert(db.read(dbType(notNull[String]("name"), notNull[Int]("age"))) === ("John",23))
  }

}
