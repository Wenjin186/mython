package edu.colorado.csci3155.mython

import scala.collection.mutable.ArrayBuffer

class MythonStore {
    // A mython store is built from an array whose length indicates
    // the number of variables present.
    var a: ArrayBuffer[MythonValue] = new ArrayBuffer[MythonValue]()

    def numVars: Int = a.length
    /* Create a new cell in the store */
    def createNewCell(v: MythonValue): Int = {
        val j = a.length
        a += v
        return j
    }

    /* Lookup the value stored in a cell */
    def lookupCell(j: Int): MythonValue = {
        assert(j >= 0 && j < a.length)
        a(j)
    }

    /* Assign a new value to a cell */
    def assignToCell(j: Int, v: MythonValue) = {
        assert(j >= 0 && j < a.length)
        a(j) = v
    }

}
