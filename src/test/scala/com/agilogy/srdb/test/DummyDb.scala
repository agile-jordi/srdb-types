package com.agilogy.srdb.test

import java.sql.{ResultSet, PreparedStatement}


/**
 * A dummy DB that expects the mocked PreparedStatement and ResultSet with wich it will simulate the behavior of a query
 * Note that no com.agilogy.srdb is done in this file. This db represents the DB library used by the user, wich does NOT depend on SRDB
 */
class DummyDb(ps: PreparedStatement, rs: ResultSet) {

  def prepare[T](psPreparer: PreparedStatement => Unit): Unit = {
    psPreparer(ps)
  }
  
  def read[T](rsReader: ResultSet => T):T = {
    rsReader(rs)
  }

}
