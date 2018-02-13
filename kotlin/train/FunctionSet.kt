package train

import java.nio.file.Files
import java.nio.file.Paths

fun sum1(a: Int, b: Int): Unit {
    println(a + b)
}

fun sum2(a: Int, b: Int): Int {
    return a + b
}

fun sum3(a: Int, b: Int) = a + b


fun max1(a: Int, b: Int): Int {
    if (a > b) {
        return a
    } else {
        return b
    }
}

fun max2(a: Int, b: Int) = if (a > b) a else b

fun parseInt(str: String?): Int {
//    if (str != null) {
//        return str.length
//    } else {
//        return -1
//    }
    return str?.toInt() ?: -1
//    return str!!.length
}

fun getStringLength(obj: Any): Int? {
    /**
     * obj is String
     * are be equals to
     * if (obj instanceof String) obj = (String) obj
     */
    if (obj is String) {
        return obj.length
    } else {
        return null
    }
    /**
     * 同时还有 obj !is String 这种用法
     * 相当于 !(obj is String)
     *
     */
}

/**
 * when 表达式
 */
fun cases(obj: Any) {
    when (obj) {
        1 -> println("One")
        "ldc" -> println("Hello $obj")
        is Int -> print("Int")
        !is Int -> print("Not a string")
        else -> print("Unknown")
    }
}

fun foo(name: String = "", age: Int = 0) {
    println("a = $name, b = $age")
}

/**
 * Kotlin 中不需要三元表达式，因为 if 本身也是一个表达式
 * if 表达式接收一个布尔值，具有一个返回值
 */
fun testIf(param: Int) {
    val result = if (param == 1) {
        "one"
    } else if (param == 2) {
        "two"
    } else {
        "three"
    }
}

/**
 * 与 if 语句一样， when 在 Kotlin 中也是一个函数并具有返回值
 */
fun testWhen(color: String): Int {
    return when (color) {
        "Red" -> 0
        "Green" -> 1
        "Blue" -> 2
        else -> throw IllegalArgumentException("Invalid color param value")
    }
}

/**
 * 就连 try 也未能幸免
 */
fun testTry() {
    val color = try {
        testWhen("Red")
        println(testIf(465))
    } catch (e: ArithmeticException) {
        throw IllegalStateException(e)
    }
    println(color)
}

/**
 * with 语法
 */
//
class Turtle {
    fun penDown() {
        println("pen down")
    }

    fun penUp() {
        println("pen up")
    }

    fun turn(degrees: Double) {
        println("pen turn $degrees℃")
    }

    fun forward(pixels: Double) {
        println("go straight $pixels pixels")
    }
}

fun testWith() {
    val myTurtle = Turtle()

    val a = with(myTurtle) {
        penDown()
        for (i in 1..4) {
            forward(100.0)
            turn(90.0)
        }
        penUp()
    }

    val b = myTurtle.run {
        penDown()
        for (i in 1..4) {
            forward(100.0)
            turn(90.0)
        }
        penUp()
    }
    println(a)
    println(b)
}

/**
 * Java try with resource
 * use 函数会自动关闭 resource
 */
fun testUse() {
    val stream = Files.newInputStream(Paths.get("/some/file.txt"))
    stream.buffered().reader().use { reader ->
        println(reader.readText())
    }
}